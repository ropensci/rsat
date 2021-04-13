#' Mosaic the tiles intersecting the region of interest
#'
#' Satellite measurements are divided into indivisible units called tiles. The
#' mosaic function binds and crops the tiles to generate a single image
#' of the region of interest for each date.
#'
#' @param x an \code{rtoi} object.
#' @param db_path path to the database. By default, the path
#' is defined by \code{x}.
#' @param region an sf object. Region for cropping the images around.
#' By default, the path is defined by \code{x}.
#' @param out_path path to save the mosaicked images. By default, the path
#' is defined by \code{x}.
#' @param bfilter a vector of bands to. If not supplied, all are used.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param warp character. If equal to "extent", it also crops the images
#' around the \code{rtoi}. Use "" otherwise.
#' @param y omitted parameter.
#' @param ... additional arguments.
#'
#' @importFrom raster raster
#' @include rtoi.R records.R
#' @export
#' @examples
#' \dontrun{
#' library(rsat)
#'
#' # load navarre sf from the package
#' data(ex.navarre)
#'
#' # set the credentials
#' set_credentials("username", "password")
#'
#' # path where the region is stored
#' rtoi.path <- tempdir()
#' # path where downloads are stored
#' db.path <- file.path(tempdir(), "DATABASE")
#' navarre <- new_rtoi(
#'   "Navarre",
#'   ex.navarre,
#'   rtoi.path,
#'   db.path
#' ) #'
#' # Landsat-5
#' sat_search(
#'   region = navarre,
#'   product = "LANDSAT_TM_C1",
#'   dates = as.Date("1988-08-01") + seq(1, 35)
#' )
#' download(navarre)
#'
#' mosaic(navarre, overwrite = T)
#'
#' derive(navarre, "NDVI", product = "LANDSAT_TM_C1")
#' }
setMethod(
  f = "mosaic",
  signature = c("rtoi"),
  function(x, ...) {
    rtoi_size_cal(x)
    # sat<-"sen2"
    args <- list(...)
    r <- records(x)
    if (length(r) == 0) {
      stop("There are no records in for mosaic.")
    }
    if (get_database(x) == "") {
      stop("db_path in rtoi must be defined.")
    }
    for (p in unique(product(r))) {
      mosaic(
        x = subset(r, p, "product"),
        out_path = get_dir(x),
        db_path = get_database(x),
        region = region(x),
        ...
      )
    }
    rtoi_size_cal(x)
  }
)
# subset(navarre$records,"mod","sat")

#' @rdname mosaic-rtoi-ANY-method
#' @aliases mosaic,records
#' @importFrom raster mosaic
#' @importFrom utils untar
#' @importFrom sp proj4string
setMethod(
  f = "mosaic",
  signature = c("records"),
  function(x,
           out_path,
           db_path,
           bfilter,
           warp = "extent",
           region,
           overwrite = FALSE, ...) {
    args <- list(...)
    if ("dates" %in% names(args)) {
      days <- args$dates
    } else {
      days <- dates(x)
    }

    if (length(unique(product(x))) > 1) {
      stop("All the records must be from the same satellite")
    }
    scratch.tmp <- file.path(tempdir(), "rgt_scratch")
    dir.create(scratch.tmp, recursive = TRUE, showWarnings = FALSE)
    for (d in unique(days)) {
      d <- as.Date(d)
      out.dir <- file.path(tempdir(),
                           get_mosaic_dir(x[1]),
                           paste0(format(d, "%Y%j")))
      out.zip <- file.path(out_path,
                           get_mosaic_dir(x[1]),
                           paste0(format(d, "%Y%j"), ".zip"))
      dir.create(dirname(out.zip), recursive = TRUE, showWarnings = FALSE)
      if (file.exists(out.zip)) {
        if (overwrite) {
          file.remove(out.zip)
          mosaiced.bands <- NULL
        } else {
          mosaiced.bands <- utils::unzip(out.zip, list = TRUE)$Name
        }
      } else {
        mosaiced.bands <- NULL
      }
      dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

      dr <- x[which(days %in% d)]
      mfiles <- file.path(db_path, get_file_path(dr))

      mfiles <- mfiles[file.exists(mfiles)]
      if (length(mfiles) < 1) next

      if (grepl("^Landsat", sat_name(dr)[1])) {
        mosaicFunctions <- mosaic_fun_ls(mfiles)
      } else if ("Sentinel-3" == sat_name(dr)[1]) {
        if (product(dr[1]) == "SY_2_SYN___") {
          mosaicFunctions <- mosaic_fun_SY_2_SYN(mfiles,
                                                 scratch.tmp = scratch.tmp)
        } else {
          message(paste0("Product ", product(dr[1]), " not supported"))
        }
      } else if ("Sentinel-2" == sat_name(dr)[1]) {
        mosaicFunctions <- mosaic_fun_sen2(mfiles)
      } else if ("Modis" == sat_name(dr)[1]) {
        mosaicFunctions <- mosaic_fun_mod(mfiles)
      }

      # get product functions
      bands <- mosaicFunctions$bands
      bands.files <- mosaicFunctions$bands.files
      filterchunks <- mosaicFunctions$filterchunks
      readfromscratch <- mosaicFunctions$readfromscratch
      defineNodata <- mosaicFunctions$defineNodata

      allfiles <- c()
      for (m in mfiles) {
        allfiles <- c(allfiles, readfromscratch(m,
                                                bands.files,
                                                scratch.tmp = scratch.tmp))
      }
      ######################################
      # Bands
      ######################################
      message(paste0("Mosaicking bands for period ", d))
      for (bnds in bands) {
        chunks <- filterchunks(allfiles, bnds)
        if (length(chunks) > 0) {
          bname <- gsub(":", "_", bnds)
          tmpfile <- file.path(tempdir(), paste0(bname, "1.vrt"))
          cmpfile <- file.path(out.dir, paste0(bname, "_tmp.tif"))
          out.file.name <- gsub("_tmp", "", cmpfile)
          out.file.name <- gsub("band", "B", out.file.name)
          if (basename(out.file.name) %in% mosaiced.bands) {
            next
          }

          genMosaicGdalUtils(
            typechunks = chunks,
            temp = tmpfile,
            nodata = defineNodata(chunks, bnds),
            out.name = cmpfile
          )

          tryCatch(
            {
              switch(tolower(warp),
                "extent" = {
                  r.tmp <- raster(cmpfile)
                  region <- st_transform(region, proj4string(r.tmp))
                  rm(r.tmp)

                  # TODO get projection using gdal_crs,
                  #cannot close the connection and remove the file
                  # region <- st_transform(region,gdal_crs(cmpfile)$input)
                  ext <- extent(region)

                  gdal_utils(
                    util = "warp",
                    source = cmpfile,
                    destination = out.file.name,
                    options = c(
                      "-te", ext@xmin, ext@ymin, ext@xmax, ext@ymax,
                      "-te_srs", st_crs(region)$proj4string
                    )
                  )

                  gc()
                  file.remove(cmpfile)
                },
                {
                  file.rename(
                    cmpfile,
                    out.file.name
                  )
                }
              )
            },
            error = function(e) {
              warning(e)
              file.rename(
                cmpfile,
                out.file.name
              )
              warning(paste0("Error warping image in period ", d))
            }
          )
        }
        add2rtoi(infile = out.file.name, out.zip = out.zip)
      }
      unlink(out.dir, recursive = TRUE)
    }
  }
)
