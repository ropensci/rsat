#' Computes a remote sensing index from an \code{rtoi}
#'
#' Combines the bands from multispectral satellite products through simple
#' math to highlight a process or material in the image.
#'
#' The package contemplates some pre-defined indexes, which can be displayed
#' using the \code{show_variables()} function. To compute one of those, write
#' its name in the \code{variable} argument. Custom indexes can be
#' supplied through the \code{fun} argument. The function should use the
#' name of the bands as inputs (red, green, blue, nir, swir1, or swir2) and
#' return a single element. For instance, the Normalized Difference Snow
#' Index would be;
#'
#' NDSI = function(green, swir1){
#' ndsi <- (green - swir1)/(green + swir1)
#' return(ndsi)
#' }
#'
#' @param x an \code{rtoi} as the source of images.
#' @param product the name of the product from which the index is computed.
#' @param dates a vector of dates being considered (optional).
#' @param fun a \code{function} that computes the remote sensing index.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @param variable the name of the variable.
#' @param suppressWarnings evaluates its expression in a context that ignores all warnings.
#' @param ... additional argument for variable deriving
#'
#' @return nothing. The derived variables will be save in the hard drive.
#' Use get_stars to get the variables.
#'
#' @include rtoi.R
#' @export
#' @examples
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
#' rsat_list_data(pamplona)
#' # show prefedined varibles
#' show_variables()
#' rsat_derive(pamplona, "NDVI", product = "mod09ga")
#' # now NDVI is processed
#' rsat_list_data(pamplona)
#'
#' # ad-hoc variable
#' NDSI = function(green, swir1){
#' ndsi <- (green - swir1)/(green + swir1)
#' return(ndsi)
#' }
#' rsat_derive(pamplona, "NDSI", product = "mod09ga",fun=NDSI)
#' # now NDVI is processed
#' rsat_list_data(pamplona)
#' plot(pamplona, product="mod09ga",variable="NDSI")
setGeneric("rsat_derive", function(x,
                              variable,
                              ...) {
  standardGeneric("rsat_derive")
})

#' @rdname rsat_derive
#' @aliases rsat_derive,rtoi,character
#' @importFrom zip zip_list
setMethod("rsat_derive",
  signature = c("rtoi", "character"),
  function(x,
           variable,
           product,
           dates,
           fun,
           overwrite = FALSE,
           verbose = FALSE,
           suppressWarnings = TRUE,
           ...) {
    if (missing(product)) {
      p <- unique(product(records(x)))
      if (length(p) != 1) {
        stop(paste0("Your rtoi has more than one product, ",
                    "use 'product' argument to specify the ",
                    "product to derive variables."))
      } else {
        product <- p
      }
    }
    if (missing(fun)) {
      fun <- get_var_fun(variable)
    }
    # product =
    rtoi_products <- basename(list.dirs(list.dirs(get_dir(x),
                                                  recursive = FALSE),
                                        recursive = FALSE))
    if (!any(grepl(product, rtoi_products))) {
      message(paste0("Product not mosaicked, mosaic the product ",
                     "from which you will derive variables."))
      stop(paste0("\nAvailable products: ", paste(rtoi_products,
                                                  collapse = ", "), "."))
    }

    bdata <- deriveBandsData(product)
    bands <- bdata$bands
    additional.sizes <- bdata$additional.sizes

    #############################################
    # Dir creation
    #############################################
    mdir <- get_mosaic_dir(x, product)
    out.dir <- file.path(get_var_dir(x, product), variable)
    dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

    #############################################
    # Image processing
    #############################################
    images <- list.files(mdir, full.names = TRUE)
    images <- images[grepl("\\.zip$", images)]
    zip.file <- paste0(out.dir, ".zip")

    for (i in images) {
      message(paste0("Processing image ", basename(i), "."))
      # layers<-file.path("/vsizip",i,zip_list(i)$filename)
      layers <- file.path("/vsizip", i, utils::unzip(i, list = TRUE)$Name)

      for (size in additional.sizes) {
        file.name <- paste0(variable, "_",
                            format(genGetDates(i), "%Y%j"),
                            size,
                            ".tif")
        out.file <- file.path(out.dir, file.name)
        layer.size <- layers[grepl(size, layers, fixed = TRUE)]

        if (!(file.exists(zip.file) &&
              (file.name %in% utils::unzip(zip.file, list = TRUE)$Name)) ||
            overwrite) {
          result <- deriveVariables(bands,
                                    layers = layer.size,
                                    fun,
                                    verbose = verbose,
                                    i = i, ...)
          if (!is.null(result)) {
            if(suppressWarnings){
              suppressWarnings(writeRaster(result, out.file, overwrite = overwrite))
            }else{
              writeRaster(result, out.file, overwrite = overwrite)
            }

            # write_stars(result,out.file,update=overwrite)
            add2rtoi(out.file, zip.file)
          }
        } else {
          message(paste0("File already exists! file: ", out.file))
        }
      }
    }
    unlink(out.dir, recursive = TRUE)
    rtoi_size_cal(x)
  }
)

deriveBandsData <- function(product) {
  #############################################
  # Bands data
  #############################################

  # sentinel-2(lvl1) product
  if (product %in% "S2MSI1C") {
    return(list(
      bands = variables$bands[["Sentinel-2"]],
      additional.sizes = ""
    ))
    # sentinel-2(lvl2) product
  } else if (product %in% c("S2MSI2A", "S2MS2Ap")) {
    return(list(
      bands = variables$bands[["Sentinel-2"]],
      additional.sizes = c("_10m", "_20m", "_60m")
    ))
    # sentinel-3 product
  } else if (grepl("SY_2_SYN___", product)) {
    return(list(
      bands = variables$bands$`SY_2_SYN___`,
      additional.sizes = ""
    ))
    # landsat-8 product
  } else if (grepl("LANDSAT_8_C1", product)) {
    return(list(
      bands = variables$bands$ls8,
      additional.sizes = ""
    ))
    # landsat-7 product
    # }else if(grepl("LANDSAT_7_C1",product)){
    #  return(list(bands<-variables$bands$ls7,
    #              additional.sizes=""))
    # landsat-7 product
  } else if (grepl("LANDSAT_ETM_C1", product)) {
    return(list(bands <- variables$bands$ls7,
      additional.sizes = ""
    ))
    # landsat-5 product
  } else if (grepl("LANDSAT_TM_C1", product)) {
    return(list(
      bands = variables$bands$ls5,
      additional.sizes = ""
    ))
    # mod09 product
  } else if (any(grepl(tolower(substr(product, 1, 5)), c("mod09",
                                                         "myd09")))) {
    return(list(
      bands = variables$bands$mod09ga,
      additional.sizes = ""
    ))
    # mcd43a4
  } else if (grepl("mcd43a4", product)) {
    return(list(
      bands = variables$bands$mcd43a4,
      additional.sizes = ""
    ))
  } else {
    warning(paste0("Product '", product, "' not supported."))
    return(NULL)
  }
}


#' @importFrom methods formalArgs
#' @importFrom terra rast
deriveVariables <- function(bands,
                            layers,
                            fun,
                            verbose = FALSE,
                            i = NULL, ...) {
  bjump <- FALSE
  result <- NULL
  funargs <- formalArgs(fun)
  funString <- "result<-fun("
  # band load and asignation
  funargs <- formalArgs(fun)
  for (arg in funargs) {
    band <- bands[names(bands) %in% arg]
    if (length(band) != 0) {
      band <- layers[grepl(band, layers, ignore.case = TRUE)]
    }
    if (length(band) == 0) {
      if (verbose) warning(paste0("Error reading band ", arg))
      next
    }
    band <- gsub("\\", "/", band, fixed = TRUE)
    if (verbose) message(paste0("Reading band: ",
                                paste0(arg, "<-rast('", band, "')")))
    # eval(parse( text=paste0(arg, "<-read_stars('",
    #                         band,
    #                         "',normalize_path = FALSE)")))

    eval(parse(text = paste0(arg, "<-rast('", band, "')")))
    funString <- paste0(funString, arg, "=", arg, ",")
  }
  # arguments asignation
  arguments <- as.list(match.call())
  arguments <- arguments[names(arguments) %in% funargs &
    (!names(arguments) %in% names(layers))]
  for (arg in names(arguments)) {
    funString <- paste0(funString, arg, "=function.arg$", arg, ",")
  }
  # complete the function
  funString <- paste0(substr(funString, 1, nchar(funString) - 1), ")")

  # if(verbose){message(paste0("Function for evaluation: \n",funString))}
  tryCatch(
    {
      eval(parse(text = funString))
      return(result)
    },
    error = function(e) {
      if (verbose) {
        message(e)
        message(paste0("Band not found for image ",
                       i,
                       ". Check the mosaic of this image."))
      }
    }
  )
  return(result)
}
