#' Plot an \code{rtoi} object
#'
#' Plot (a map of) the values of an \code{rtoi} or \code{records} object.
#'
#' @param x an \code{rtoi} or \code{records}.
#' @param y character argument. The valid values are "dates", "preview", or
#' "view".
#' @param variable character argument. The variable to be plotted. By default,
#'  a color (RGB) variable is selected .
#' @param band_name character vector argument. Enables false color plots. By
#' default, usual bands are selected \code{c("red","green","blue")}.
#' @param product character argument. The product name to be plotted.
#' @param dates date vector argument. The dates to be plotted.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @param xsize the number of samples on the horizontal axis.
#' @param ysize the number of samples on the vertical axis.
#' @param ... additional arguments.
#'
#' @return \code{tmap} plot.
#'
#' @importFrom tmap tm_shape tm_raster tm_layout tm_rgb tm_graticules
#' @importFrom sp proj4string proj4string<-
#' @importFrom calendR calendR
#' @importFrom grDevices colors
#' @importFrom sf gdal_utils
#' @importFrom terra clamp
#' @importFrom raster raster stack
#' @importFrom stars st_apply read_stars
#' @include rtoi.R records.R
#' @export
#' @rdname plot
#' @examples
#' library(rsat)
#'  \dontrun{
#'
#' # load example rtoi
#' file.copy(from=system.file("ex/Navarre",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#'
#' navarre <- read_rtoi(file.path(tempdir(),"Navarre"))
#'
#' print(navarre)
#'
#' # plot the calendar
#' plot(navarre, "dates")
#'
#'
#'
#' # replace with your own "username" and "password"
#' set_credentials("username", "password")
#'
#' # plot the quicklook images before the download
#' # needs credentials to download preview images
#' plot(navarre, y = "preview")
#'
#' # select partially cloud free
#' rcds <- records(navarre)
#' rcds <- rcds[dates(rcds) %in% as.Date(c("20210310", "20210313"), "%Y%m%d")]
#' records(navarre) <- rcds
#'
#' plot(navarre, "preview")
#'
#' file.copy(from=system.file("ex/Pamplona",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#' # plot already mosaicked rtoi ("view" mode)
#' pamplona <- read_rtoi(file.path(tempdir(),"Pamplona"))
#'
#' rsat_list_data(pamplona)
#'
#' # plot can compute the rgb image on the fly from mosaicek bands
#' plot(pamplona, "view", product="mod09ga")
#'
#' # plot on the fly with false color
#' plot(pamplona, "view",
#'      product = "mod09ga",
#'      band_name = c("nir", "red", "green"))
#'
#' file.copy(from=system.file("ex/PamplonaDerived",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#' # plot already mosaicked rtoi ("view" mode)
#' pamplona.derived <- read_rtoi(file.path(tempdir(),"PamplonaDerived"))
#'
#' rsat_list_data(pamplona.derived)
#'
#' # plot derived variables
#' plot(pamplona.derived, "view",
#'      product = "mod09ga",
#'      variable = "NDVI")
#'
#' # Set the max and min value in plot
#' plot(pamplona.derived,"view",
#'      variable="NDVI",
#'      product="mod09ga",
#'      zlim=c(0,1))
#' }
setMethod(
  f = "plot",
  signature = c("rtoi", "Date"),
  function(x, y, ...,
           variable = "rgb",
           band_name = c("red", "green", "blue"),
           verbose = FALSE,
           xsize = 250,
           ysize = 250) {
    # load the data
    switch(variable,
      "rgb" = {
        dirs <- list.dirs(get_dir(x))
        mosaics.dir <- dirs[grepl("mosaic", dirs)]
        files <- list.files(mosaics.dir,
                            full.names = TRUE,
                            pattern = format(y, "%Y%j"))
        if (length(files) == 0)
          stop("Plotting requires mosaicked images and there is none.")
        plot.list <- list()
        for (p in product(x)) {
          debands <- deriveBandsData(p)
          if (!is.null(debands)) {
            files.p <- files[grepl(p, files)]
            if (length(files.p) > 0) {
              plot.list <- append(plot.list,
                                  list(read_rgb(files.p,
                                                p,
                                                debands$bands,
                                                band_name = band_name,
                                                y,
                                                xsize,
                                                ysize)))
            }
          }
        }
      },
      { # otherwise
        dirs <- list.dirs(get_dir(x))
        var.dir <- dirs[grepl("variables", dirs)]
        var.zip <- list.files(var.dir, full.names = TRUE)
        plot.list <- NULL
        for (p in product(x)) {
          files.p <- var.zip[grepl(p, var.zip)]
          files.p <- files.p[grepl(variable, files.p)]
          if (length(files.p) == 0) next

          aux <- read_variables(files.p, p, variable, y, xsize, ysize)
          # aux<-aux %>% st_transform(st_crs(4326))

          if (is.null(plot.list)) {
            plot.list <- aux
          } else {
            attributes(aux)$dimensions <- attributes(plot.list)$dimensions
            plot.list <- c(plot.list, aux)
          }
        }
      }
    )

    # plot
    genPlotGIS(r = plot.list, region=region(x), verbose=verbose,...)
  }
)

#' @rdname plot
#' @aliases plot,character
setMethod(
  f = "plot",
  signature = c("rtoi", "character"),
  function(x, y, ...,
           variable = "rgb",
           product = "ALL",
           band_name = c("red", "green", "blue"),
           dates = NULL,
           verbose = FALSE,
           xsize = 250,
           ysize = 250) {
    if (y == "dates") {
      r <- records(x)
      if(length(r)==0){
        message("The calendar cannot be created without records. Use
                rsat_search to get new records.")
        return(NULL)
      }
      if (!is.null(dates)) {
        r <- r[dates(r) %in% dates]
      }
      months <- sort(unique(format(dates(r), "%Y%m")))
      if (length(months) > 12) {
        months <- months[1:12]
        r <- r[format(dates(r), "%Y%m") %in% months]
        message(paste0("Plotting is restricted to one year,",
                       " use the 'dates' argument to plot another year."))
      }
      date <- dates(r)

      all.products <- unique(sat_name(r))
      all.dates <- seq(min(date), max(date), 1)
      df <- data.frame(date = all.dates)
      for (p in all.products) {
        p.date <- date[sat_name(r) %in% p]
        df[p] <- unlist(lapply(all.dates %in% p.date, function(x, p) {
          if (x) {
            return(p)
          } else {
            return("")
          }
        }, p), recursive = TRUE)
      }
      if (length(all.products) > 1) {
        n.product <- apply(df[, 2:ncol(df)], 1, paste, collapse = " + ")
        for (a in 1:(ncol(df) - 2)) {
          n.product <- gsub(" \\+  \\+ ", " + ", n.product, useBytes = TRUE)
        }
        n.product <- gsub(" \\+ $", "", n.product, useBytes = TRUE)
        n.product <- gsub("^ \\+ ", "", n.product, useBytes = TRUE)
      } else {
        n.product <- df[, 2]
      }
      n.product[n.product %in% ""] <- "No captures"
      # df["product"]<-n.product
      # return(ggplot_calendar_heatmap(
      #  df,
      #  'date',
      #  'product'
      # ))
      return(calendR(
        start_date = min(date), # Custom start date
        end_date = max(date),
        special.days = n.product,
        special.col = c("pink",
                        "lightblue",
                        "lightgreen",
                        "lightsalmon",
                        "brown",
                        "blue",
                        colors())[seq_len(length(unique(n.product)))],
        legend.pos = "right"
      ))
    } else if (y == "preview") {
      rtoi.path <- get_dir(x)
      if (product == "ALL") {
        product <- product(x)
      } else {
        product <- product
      }
      plot.list <- list()
      for (prdct in product) {
        sat.records <- subset(records(x), "product", prdct)
        if (length(sat.records) == 0) {
          if (verbose) message(paste0("No records for product", prdct))
          next
        }
        preview.path <- file.path(rtoi.path,
                                  sat_name(sat.records[1]),
                                  prdct,
                                  "preview")
        dir.create(preview.path, showWarnings = FALSE, recursive = TRUE)
        if (is.null(dates)) {
          date <- unique(dates(sat.records))
        } else {
          date <- unique(dates)
        }
        for (d in date) {
          preview.path.img <- file.path(preview.path,
                                        paste0(format(as.Date(d), "%Y%m%d"),
                                               ".tif"))
          preview.records <- subset(sat.records, "date", as.Date(d))
          if (length(preview.records) == 0) {
            if (verbose) message(paste0("No records for the product ",
                                        prdct,
                                        " and date ",
                                        as.Date(d), "."))
            next
          }
          if (!file.exists(preview.path.img)) {
            for (i in seq_len(length(preview.records))) { # download all preview
              r <- preview.records[i]
              proj_file <- get_preview_proj(r, get_database(x))
              if (!file.exists(proj_file)) {
                dir.create(get_preview_path(r, get_database(x)),
                           showWarnings = FALSE,
                           recursive = TRUE)
                download_preview(r,
                                 tmp_dir = get_database(x),
                                 verbose = verbose)
              }
            }
            all.proj.path <- get_preview_proj(preview.records,
                                              get_database(x))
            tmp.file <- file.path(tempdir(), gsub("\\.tif", "",
                                                 basename(preview.path.img)))
            gdal_utils(
              util = "buildvrt",
              source = all.proj.path,
              destination = tmp.file
            )

            gdal_utils(
              util = "translate",
              source = tmp.file,
              destination = preview.path.img,
              options = c("-of", "GTiff")
            )
            plot.list <- append(plot.list,
                         list(read_stars(preview.path.img,
                                         RasterIO = list(nBufXSize = xsize,
                                                         nBufYSize = ysize))))
          } else {
            plot.list <- append(plot.list,
                                list(read_stars(preview.path.img,
                                    RasterIO = list(nBufXSize = xsize,
                                                    nBufYSize = ysize))))
          }
        }
      }
      if (length(plot.list) == 0) {
        return(message(paste0("No preview is available for this time",
                              " interval and product.")))
      }
      return(genPlotGIS(r = plot.list, region=region(x),...,verbose=verbose))
    } else if (y == "view") {
      if (product == "ALL") {
        stop("view mode requires product argument.")
      }
      switch(variable,
        "rgb" = {
          # load the data
          dirs <- list.dirs(gsub("\\", "/", get_dir(x), fixed = TRUE))
          mosaics.dir <- dirs[grepl("mosaic", dirs)]
          files <- list.files(mosaics.dir, full.names = TRUE)
          files <- files[grepl(product, files)]
          if (length(files) == 0)
            stop(paste0("Plotting requires mosaicked images and ",
                        "there is none. Use the 'dates' argument",
                        " or the 'preview' mode."))
          plot.list <- list()
          for (f in files) {
            debands <- deriveBandsData(product)
            if (!is.null(debands)) {
              plot.list <- append(plot.list,
                                  list(read_rgb(f,
                                                product,
                                                debands$bands,
                                                band_name,
                                                genGetDates(f),
                                                xsize,
                                                ysize)))
            }
          }
        },
        { # load the data
          dirs <- list.dirs(gsub("\\", "/", get_dir(x), fixed = TRUE))
          var.dir <- dirs[grepl("variables", dirs)]
          var.dir <- var.dir[grepl(product, var.dir)]
          files <- list.files(var.dir, full.names = TRUE)
          files <- files[grepl(variable, files)]

          if (length(files) == 0) warning(paste0("Plotting requires mosaicked",
                                              " images, and there is none."))
          if (length(files) > 1) {
            warning(paste0("More than one record for the same variable ",
                           "and product, plotting the first one."))
            files <- files[1]
          }
          plot.list <- read_variables(files,
                                      product,
                                      variable,
                                      NULL,
                                      xsize,
                                      ysize)
        }
      )
    } else {
      stop(paste0("plot needs a parameter 'y' with one of ",
                  "the following values: 'view', 'preview', or 'dates'."))
    }

    # plot
    genPlotGIS(r = plot.list, region=region(x), ...,verbose=verbose)
  }
)

#' @rdname plot
#' @aliases plot,records
setMethod(
  f = "plot",
  signature = c("records"),
  function(x, y, verbose = FALSE, ...) {
    # load the data
    if (length(x) > 0) {
      if (length(x) > 5)
        message("It may take a while to download the previews.")
      img.list <- list()
      lname <- c()
      for (i in seq_len(length(x))) {
        r <- x[i]
        p.url <- get_preview(r)
        na <- names(r)
        pre_dir <- file.path(tempdir(), get_dir(r), "rgt_preview")
        dir.create(pre_dir, showWarnings = FALSE, recursive = TRUE)
        pre.file <- file.path(pre_dir, names(r))
        if (verbose) message(paste0("Preview downloaded: ", pre.file))
        if (!file.exists(pre.file)) {
          con <- connection$getApi(get_api_name(r))
          con$pictureDownload(p.url, pre.file)
        }
        img <- suppressWarnings(rast(pre.file))
        ext(img) <- ext(r)
        crs(img) <- st_crs(crs(r))$proj4string
        img.list <- c(img.list, img)
        lname <- c(lname, paste0(sat_name(r), "_", dates(r)))
      }

      # plot
      genPlotGIS(r = img.list, ..., verbose=verbose)
    } else {
      message("Empty records.")
    }
  }
)


#' @rdname plot
#' @aliases plot,rtoi,missing
setMethod(
  f = "plot",
  signature = c("rtoi", "missing"),
  function(x, y, verbose = FALSE, ...) {
    # load the data
    y <- "view"
    plot(x, y, dates = dates(x)[1], verbose, ...)
  }
)

read_variables <- function(zip.file, product, var.name, date, xsize, ysize) {
  tif.files <- file.path("/vsizip",
                         zip.file,
                         utils::unzip(zip.file, list = TRUE)$Name)
  if (!is.null(date)) {
    tif.files <- tif.files[grepl(format(date, "%Y%j"), tif.files)]
    n <- paste0(product, "_", var.name, "_", date)
  } else {
    n <- gsub("\\.tif", "", basename(tif.files))
  }
  if (length(tif.files) == 0) {
    return(NULL)
  }
  rasterio <- list(nBufXSize = xsize, nBufYSize = ysize)
  stars.list <- lapply(tif.files, read_stars,
                       normalize_path = FALSE,
                       RasterIO = rasterio,
                       proxy = FALSE)
  stars.list <- do.call(c, stars.list)
  raster.list <- stars.list#as(stars.list, "Raster")
  names(raster.list) <- n
  return(raster.list) # TODO change to stars
}
#' @importFrom terra stretch as.matrix rast
#' @importFrom methods as
#' @importFrom stars read_stars
read_rgb <- function(files.p,
                     product,
                     bands,
                     band_name = c("red", "green", "blue"),
                     date,
                     xsize,
                     ysize) {
  files.p <- file.path("/vsizip",
                       files.p,
                       utils::unzip(files.p, list = TRUE)$Name)

  rasterio <- list(nBufXSize = xsize, nBufYSize = ysize)
  tryCatch(
    {
      red <- read_stars(files.p[grepl(bands[band_name[1]],
                                      files.p,
                                      ignore.case = TRUE)][1],
                        normalize_path = FALSE,
                        RasterIO = rasterio,
                        proxy = FALSE)
      green <- read_stars(files.p[grepl(bands[band_name[2]],
                                        files.p,
                                        ignore.case = TRUE)][1],
                          normalize_path = FALSE,
                          RasterIO = rasterio,
                          proxy = FALSE)
      blue <- read_stars(files.p[grepl(bands[band_name[3]],
                                       files.p,
                                       ignore.case = TRUE)][1],
                         normalize_path = FALSE,
                         RasterIO = rasterio,
                         proxy = FALSE)
    },
    warning = function(cond) {
      warning(cond)
      return(NULL)
    }
  )

  # stretch only the data using raster
  red[[1]] <- as.matrix(stretch(rast(red[[1]])))
  green[[1]] <- as.matrix(stretch(rast(green[[1]])))
  blue[[1]] <- as.matrix(stretch(rast(blue[[1]])))

  aux <- merge(c(red, green, blue))
  # aux<-as(aux,"Raster")
  names(aux) <- paste0(product, "_", date)
  return(aux)
}


#' @importFrom tmap tm_facets tm_graticules tm_grid tm_compass tm_scale_bar
#' @importFrom tmap tm_fill tm_polygons tm_borders tm_shape tmap_arrange
#' @importFrom terra project minmax
genPlotGIS <- function(r,
                       region,
                       ...,
                       breaks,
                       labels,
                       zlim,
                       layout,
                       proj,
                       nbreaks = 40,
                       nlabels = 10,
                       as.grid = TRUE,
                       compass.rm = FALSE,
                       scale.bar.rm = FALSE,
                       verbose=FALSE) {
  args <- list(...)

  # r and region projection management
  if (inherits(r, "list")) {
    if (inherits(r[[1]], "SpatRaster")) {
      if (!missing(proj)) {
        r <- lapply(r, project, crs = proj)
        if (!missing(region)) {
          region <- transform_multiple_proj(region, proj4 = crs(r[[1]]))
        }
      }
    } else if (inherits(r[[1]], "stars")) {

    } else {
      stop(paste0("genPlotGIS only supports stars, ",
                  "."))
    }
  } else if (inherits(r, "SpatRaster")) {
    if (!missing(proj)) {
      r <- project(r, crs = proj)
      if (!missing(region)) {
        region <- transform_multiple_proj(region,
                                          proj4 = crs(r))
      }
    }
  } else if (inherits(r, "stars")) {

  } else {
    stop(paste0("genPlotGIS only supports stars, ",
                "."))
  }

  # layout preconfigured arguments
  tm_layout_args<-initialize.tm.layout(names(r),...)

  if (!missing(layout)) {
    lyt <- tm_facets(ncol = layout[2], nrow = layout[1])
  } else {
    lyt <- NULL
  }

  # create grid
  tm_graticules_args <- create.tm.grid(...)
  if (as.grid) {
    tm_layout_args$between.margin <- -.1
    if (!("labels.space.x" %in% names(tm_layout_args))) {
      tm_graticules_args$labels.space.x <- .10
    }
    if (!("labels.space.y" %in% names(tm_layout_args))) {
      tm_graticules_args$labels.space.y <- .10
    }
  }
  grid <-do.call(tm_graticules, tm_graticules_args)

  # compass arguments and preconfigured assignation
  if (!compass.rm) {
    compass <- create.compass(...)
  } else {
    compass <- NULL
  }


  # scale bar arguments and preconfigured assignation
  if (!scale.bar.rm) {
    scale.bar <- create.scale.bar()
  } else {
    scale.bar <- NULL
  }

  if (!missing(region)) {
    reg <-tm.add.region(region,...)
  } else {
    reg <- NULL
  }

  if (inherits(r, "list")) {
    ####################################################
    # RGB plot
    ####################################################
    maplist <- lapply(
      r, function(shp, compass, scale.bar, grid, reg) {
        tm_layout_args$panel.labels <- names(shp)
        return(do.call(tm_layout, tm_layout_args) +
                 tm_shape(shp = shp, frame = TRUE) +
                 tm_rgb() +
                 compass +
                 scale.bar +
                 grid +
                 reg)
      },
      compass, scale.bar, grid, reg
    )
    # tmap_arrange argumentsd o.call(tm_layout,tm_layout_args)
    tmap_arrange_args <- names(formals(tmap_arrange))
    tmap_arrange_args <-
      unique(tmap_arrange_args[!(tmap_arrange_args %in% "...")])
    names(tmap_arrange_args) <-
      paste0("tmap.arrange.", tmap_arrange_args)
    tm_tmap_arrange_args <-
      args[names(args) %in% names(tmap_arrange_args)]
    names(tm_tmap_arrange_args) <-
      tmap_arrange_args[names(tm_tmap_arrange_args)]

    if (!("asp" %in% tm_tmap_arrange_args)) {
      tm_tmap_arrange_args$asp <- NA
    }

    if (missing(layout)) {
      if (length(r) > 1) {
        tm_tmap_arrange_args$ncol <- ceiling(sqrt(length(r)))
      } else {
        tm_layout_args$panel.labels <- names(r[[1]])
        return(do.call(tm_layout, tm_layout_args) +
                 tm_shape(shp = r[[1]], frame = TRUE) +
                 tm_rgb() +
                 compass +
                 scale.bar +
                 grid +
                 reg)
      }
    } else {
      tm_tmap_arrange_args$nrow <- layout[1]
      tm_tmap_arrange_args$ncol <- layout[2]
    }

    return(do.call(tmap_arrange, c(maplist, tm_tmap_arrange_args)))
  }

  ####################################################
  # Stack plot
  ####################################################
  # default label and breaks for the raster
  if (missing(zlim)) {
    if (inherits(r, "stars")) {
      if(length(names(r))>1){
        min.vector<-st_apply(merge(r), MARGIN = 3, min, na.rm = TRUE)[[1]]
        max.vector<-st_apply(merge(r), MARGIN = 3, max, na.rm = TRUE)[[1]]
      }else{
        min.vector<-st_apply(r, MARGIN = 2, min, na.rm = TRUE)[[1]]
        max.vector<-st_apply(r, MARGIN = 2, max, na.rm = TRUE)[[1]]
      }
      min.vector[!min.vector> -Inf]<-NA
      lower <- min(min.vector,na.rm = TRUE)
      max.vector[!max.vector< Inf]<-NA
      upper <- max(max.vector,na.rm = TRUE)
      if(verbose){
        message(paste0("lower value",lower))
        message(paste0("upper value",upper))
      }
    } else {
      #lower <- min(minValue(r))
      #upper <- maxxmaxValue(r))
      mm <- minmax(r)

      min.vector<-mm[1,]
      min.vector[!min.vector> -Inf]<-NA
      lower <- min(min.vector,na.rm = TRUE)
      max.vector<-mm[1,]
      max.vector[!max.vector> -Inf]<-NA
      upper <- max(max.vector,na.rm = TRUE)

      if(verbose){
        message(paste0("lower value",lower))
        message(paste0("upper value",upper))
      }
    }
  } else {
    if ((class(zlim) != "numeric") & (length(zlim) != 0)) {
      stop(paste0("zlim must be a vector of length 2 specifying",
                  " the upper and lower boundaries of the legend."))
    }
    lower <- min(zlim)
    upper <- max(zlim)
  }


  nbreaks <- nbreaks - 2
  if (missing(breaks)) {
    breaks <- c(-Inf,
                seq(from = lower,
                          to = upper,
                          by = ((upper - lower) / nbreaks))
                ,Inf)
  }
  if (missing(labels)) {
    labels <- c("", as.character(round(breaks[-c(1, length(breaks))],
                                       digits = 2)))
    if (length(labels) > nlabels) {
      labels <- rep("", length(labels))
      labels[c(seq(1, length(labels),
                   as.integer(length(labels) / nlabels)),
               length(labels))] <-
        as.character(round(seq(from = lower,
                               to = upper,
                               by = ((upper - lower) / nlabels)),
                           digits = 2))
    }
  }

  # raster default arguments
  shape_r_args <- names(formals(tm_shape))
  shape_r_args <- shape_r_args[!(shape_r_args %in% c("..."))]
  names(shape_r_args) <- paste0("tm.shape.r.", shape_r_args)
  tm_shape_r_args <- args[names(args) %in% names(shape_r_args)]
  names(tm_shape_r_args) <- shape_r_args[names(tm_shape_r_args)]
  tm_shape_r_args$shp <- r

  raster_r_args <- names(formals(tm_raster))
  names(raster_r_args) <- paste0("tm.raster.r.", raster_r_args)
  tm_raster_r_args <- args[names(args) %in% names(raster_r_args)]
  names(tm_raster_r_args) <- raster_r_args[names(tm_raster_r_args)]
  if (!("col" %in% names(tm_raster_r_args))) {
    tm_raster_r_args$col <- names(r)
  }
  if (!("breaks" %in% names(tm_raster_r_args))) {
    tm_raster_r_args$breaks <- breaks
  }
  if (!("labels" %in% names(tm_raster_r_args))) {
    tm_raster_r_args$labels <- labels
  }
  if (!("legend.reverse" %in% names(tm_raster_r_args))) {
    tm_raster_r_args$legend.reverse <- TRUE
  }
  if (!("title" %in% names(tm_raster_r_args))) {
    tm_raster_r_args$title <- ""
  }
  tm_raster_r_args$midpoint <- NA
  # Base tmap
  return(do.call(tm_shape, tm_shape_r_args) +
           do.call(tm_raster, tm_raster_r_args) + # raster conf
           do.call(tm_layout, tm_layout_args) + # layout
           compass + # the compass
           reg + # region
           scale.bar + # scale
           grid +
           lyt)
}


initialize.tm.layout<-function(panel.names,...){
  args<-list(...)
  tm_layout_args <- args[names(args) %in% names(formals(tm_layout))]
  if (!("legend.bg.color" %in% names(tm_layout_args))) {
    tm_layout_args$legend.bg.color <- "white"
  }
  if (!("panel.show" %in% names(tm_layout_args))) {
    tm_layout_args$panel.show <- TRUE
  }
  if (!("panel.labels" %in% names(tm_layout_args))) {
    tm_layout_args$panel.labels <- panel.names
  }
  if (!("legend.outside" %in% names(tm_layout_args))) {
    tm_layout_args$legend.outside <- TRUE
  }
  if (!("legend.outside.size" %in% names(tm_layout_args))) {
    tm_layout_args$legend.outside.size <- 0.08
  }
  if (!("legend.outside.position" %in% names(tm_layout_args))) {
    tm_layout_args$legend.outside.position <- "right"
  }
  if (!("frame" %in% names(tm_layout_args))) {
    tm_layout_args$frame <- TRUE
  }
  tm_layout_args
}


create.tm.grid<-function(...){
  args<-list(...)
  graticules_args <- c(names(formals(tm_graticules)), names(formals(tm_grid)))
  names(graticules_args) <- paste0("tm.graticules.", graticules_args)
  tm_graticules_args <- args[names(args) %in% names(graticules_args)]
  names(tm_graticules_args) <- graticules_args[names(tm_graticules_args)]
  if (!("lines" %in% names(tm_graticules_args))) {
    tm_graticules_args$lines <- FALSE
  }
  tm_graticules_args
}

create.compass<-function(...){
  args<-list(...)
  compass_args <- names(formals(tm_compass))
  names(compass_args) <- paste0("tm.compass.", compass_args)
  tm_compass_args <- args[names(args) %in% names(compass_args)]
  names(tm_compass_args) <- compass_args[names(tm_compass_args)]
  if (!("type" %in% names(tm_compass_args))) {
    tm_compass_args$type <- "arrow"
  }
  if (!("position" %in% names(tm_compass_args))) {
    tm_compass_args$position <- c("right", "top")
  }
  if (!("size" %in% names(tm_compass_args))) {
    tm_compass_args$size <- 2
  }
  if (!("show.labels" %in% names(tm_compass_args))) {
    tm_compass_args$show.labels <- 0
  }
  do.call(tm_compass, tm_compass_args)
}

create.scale.bar<-function(...){
  args<-list(...)
  scale_bar_args <- names(formals(tm_scale_bar))
  names(scale_bar_args) <- paste0("tm.scale.bar.", scale_bar_args)
  tm_scale_bar_args <- args[names(args) %in% names(scale_bar_args)]
  names(tm_scale_bar_args) <- scale_bar_args[names(tm_scale_bar_args)]
  if (!("position" %in% names(tm_scale_bar_args))) {
    tm_scale_bar_args$position <- c("left", "bottom")
  }
  if (!(any(c("text.size", "size") %in% names(tm_scale_bar_args)))) {
    tm_scale_bar_args$text.size <- 0.8
  }
  do.call(tm_scale_bar, tm_scale_bar_args)
}

tm.add.region<-function(region,...){
  args<-list(...)
  # region default arguments
  shape_region_args <- names(formals(tm_shape))
  shape_region_args <- shape_region_args[!(shape_region_args %in% "...")]
  names(shape_region_args) <- paste0("tm.shape.region.", shape_region_args)
  tm_shape_region_args <- args[names(args) %in% names(shape_region_args)]
  names(tm_shape_region_args) <-
    shape_region_args[names(tm_shape_region_args)]
  tm_shape_region_args$shp <- region


  polygon_region_args <- c(names(formals(tm_polygons)),
                           names(formals(tm_fill)),
                           names(formals(tm_borders)))
  polygon_region_args <- unique(polygon_region_args[!(polygon_region_args
                                                      %in% "...")])
  names(polygon_region_args) <- paste0("tm.polygon.region.",
                                       polygon_region_args)
  tm_polygon_region_args <- args[names(args) %in%
                                   names(polygon_region_args)]
  names(tm_polygon_region_args) <-
    polygon_region_args[names(tm_polygon_region_args)]
  if (!("alpha" %in% names(tm_polygon_region_args))) {
    tm_polygon_region_args$alpha <- 0
  }
  if (!("lwd" %in% names(tm_polygon_region_args))) {
    tm_polygon_region_args$lwd <- 1
  }

  return(do.call(tm_shape, tm_shape_region_args) +
    do.call(tm_polygons, tm_polygon_region_args))
}
