#' Create cloud mask from an rtoi
#'
#' @param x rtoi object from which cloud masks are computed.
#' @param products the name of the dataset from which cloud masks are computed.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param ... additional arguments
#'
#' @importFrom terra minmax NAflag<- values
#' @importFrom raster readAll
#' @include rtoi.R
#' @export
#' @examples
#' ## Smooth data in rtoi
#' library(rsat)
#'
#' # create a copy of pamplona in temp file
#' file.copy(from=system.file("ex/Pamplona",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#'
#' # load example rtoi
#' pamplona <- read_rtoi(file.path(tempdir(),"Pamplona"))
#'
#' rsat_cloudMask(pamplona)
#'
#' rsat_list_data(pamplona)
#' @importFrom zip zip_list
setGeneric("rsat_cloudMask", function(x, ...) {
  standardGeneric("rsat_cloudMask")
})

#' @rdname rsat_cloudMask
#' @aliases cloud_mask,rtoi
setMethod(
  f = "rsat_cloudMask",
  signature = c("rtoi"),
  function(x, products = "ALL", verbose = FALSE, overwrite = FALSE, ...) {
    if (products == "ALL") products <- product(x)
    for (p in products) {
      img_dir <- get_mosaic_dir(x, p)
      out_dir <- file.path(dirname(img_dir), "CloudMask")
      dir.create(out_dir, showWarnings = FALSE)
      all_files <- unlist(lapply(list.files(img_dir,
                                            full.names = TRUE,
                                            pattern = "\\.zip$"),
                                 function(x) {
        file.path("/vsizip", x, utils::unzip(x, list = TRUE)$Name)
      }))

      # Modis
      if (any(grepl(tolower(substr(p, 1, 5)), c("mod09", "myd09")))) {
        all_files <- all_files[grepl("_state_1km_1", all_files)]
        fun_clkmsk <- modCloudMask
        # Landsat
      } else if (grepl("LANDSAT", p)) {
        all_files <- all_files[grepl("BQA", all_files) |
                               grepl("pixel_qa", all_files)]
        fun_clkmsk <- lsCloudMask
        if (grepl("8", p)) {
          cldcl <- c(322, 386, 834, 898, 1346, 324, 388, 836, 900, 1348)
        } else {
          cldcl <- c(66, 130, 68, 132)
        }
        # Sentinel
      } else if (p %in% unlist(SENPRODUCTS)) {
        all_files <- all_files[grepl("CLD", all_files)]
        fun_clkmsk <- senCloudMask
      } else {
        if (verbose) message(paste0("Product",
                                    p,
                                    " unsopported for cloud masking."))
        next
      }

      for (f in all_files) {
        message(paste0("Creating cloud mask of date ",
                       genGetDates(f),
                       " in product ", p, "."))
        if(fun_clkmsk(
          infile = f,
          outfile = file.path(out_dir,
                              paste0(format(genGetDates(f), "%Y%j"), ".tif")),
          overwrite = overwrite,
          cldcl = cldcl,
          ...
        )) {
          message(paste0("Cloud mask of date ",
                         genGetDates(f),
                         " already exists."))
        }
      }
    }
    unlink(out_dir, recursive = TRUE)
  }
)


modCloudMask <- function(infile,
                         outfile,
                         overwrite = FALSE,
                         verbose = FALSE, ...) {
  if ((!file.exists(outfile)) | overwrite) {
    r <- rast(infile)
    v <- matrix(as.numeric(matrix(intToBits(values(r)),
                                  ncol = 32,
                                  byrow = TRUE)[, 1:3]), ncol = 3)
    # clouds
    # interpret the bytes: 0 = clear, 1+1 = not known, assumed clear
    r[] <- rowSums(v[, 1:2])
    r[r == 1] <- NA
    r[r != 1] <- 1
    # r[(r == 0 | r == 2)] <- 1
    # shadows
    # interpret the bytes: 0 = clear, 1 = shadow
    r_shadow <- r
    r_shadow[] <- 1 - v[, 3]
    r_shadow[r_shadow == 0] <- NA
    # save the result
    ras.cloud <- r * r_shadow
    writeRaster(ras.cloud, outfile, overwrite = overwrite)
    add2rtoi(outfile, paste0(dirname(outfile), ".zip"))
    return(FALSE)
  }
  return(TRUE)
}

lsCloudMask <- function(infile,
                        outfile,
                        cldcl,
                        overwrite = FALSE,
                        verbose = FALSE, ...) {
  if ((!file.exists(outfile)) | overwrite) {
    ras.cloud <- rast(infile)
    mn <- min(minmax(ras.cloud)[1,])
    if (verbose) {
      message(paste0("Minimun: ", mn))
    }
    # ras.cloud[ras.cloud<=max(mn,1)]<-1
    # ras.cloud[ras.cloud>=sensitivity]<-1
    # ras.cloud[ras.cloud!=1]<-NA

    ras.cloud[ras.cloud %in% cldcl] <- 1
    ras.cloud[ras.cloud != 1] <- NA
    NAflag(ras.cloud) <- 0

    writeRaster(ras.cloud, outfile, overwrite = overwrite)
    add2rtoi(outfile, paste0(dirname(outfile), ".zip"))
    return(FALSE)
  }
  return(TRUE)
}

senCloudMask <- function(infile,
                         outfile,
                         overwrite = FALSE,
                         verbose = FALSE,
                         sensitivity = 50, ...) {
  if ((!file.exists(outfile)) | overwrite) {
    ras.cloud <- rast(infile)
    mn <- min(minmax(ras.cloud)[1,])
    if (verbose) {
      message(paste0("Minimun: ", mn))
    }
    ras.cloud[ras.cloud <= max(mn, 1)] <- 1
    ras.cloud[ras.cloud >= sensitivity] <- 1
    ras.cloud[ras.cloud != 1] <- NA

    NAflag(ras.cloud) <- 0
    writeRaster(ras.cloud, outfile, overwrite = overwrite)
    add2rtoi(outfile, paste0(dirname(outfile), ".zip"))
    return(FALSE)
  }
  return(TRUE)
}
