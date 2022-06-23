#' List the information available for an \code{rtoi}
#'
#' Displays the existing products, bands,
#' and processing levels for a given \code{rtoi}
#'
#' @param x an \code{rtoi} object.
#' @param ... additional arguments.
#'
#' @return a \code{data.frame} of the available information.
#' @export
#' @importFrom utils unzip
#'
#' @examples
#' \dontrun{
#' file.copy(from=system.file("ex/Navarre",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#'
#' # load example rtoi
#' navarre <- read_rtoi(file.path(tempdir(),"Navarre"))
#'
#' print(navarre)
#'
#' # print empty rtoi
#' rsat_list_data(navarre)
#'
#' file.copy(from=system.file("ex/Pamplona",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#'
#' # load example rtoi
#' pamplona <- read_rtoi(file.path(tempdir(),"Pamplona"))
#'
#' print(pamplona)
#'
#' rtoi.data <- rsat_list_data(pamplona)
#' # print mosaicked bands
#' print(rtoi.data)
#'
#' # print mosaicked bands + derived NDVI
#' file.copy(from=system.file("ex/PamplonaDerived",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#'
#' # load example rtoi
#' pamplona.derived <- read_rtoi(file.path(tempdir(),"PamplonaDerived"))
#' rsat_list_data(pamplona.derived)
#' }
setGeneric("rsat_list_data", function(x,
                                 ...) {
  standardGeneric("rsat_list_data")
})
#' @rdname rsat_list_data
#' @aliases rsat_list_data,rtoi
setMethod("rsat_list_data",
  signature = c("rtoi"),
  function(x, ...) {
    allfiles <- normalizePath(list.files(get_dir(x), full.names = TRUE),"/")
    #allfiles <- gsub("\\", "/", allfiles, fixed = TRUE)
    allfiles <- allfiles[!grepl("\\.rtoi$", allfiles)]
    allfiles <- allfiles[!grepl("region$", allfiles)]

    # satellite
    allvariables <- unlist(lapply(allfiles, function(af) {
      # product
      lapply(list.files(af, full.names = TRUE), function(p) {
        data.list <- list()
        # product
        allproducts <- list.files(p, full.names = TRUE)
        # remove preview
        allproducts <- allproducts[!grepl("preview", allproducts)]

        if (any(grepl("mosaic", allproducts))) {
          f <- list.files(allproducts[grepl("mosaic", allproducts)],
                          full.names = TRUE)[1]
          if(is.na(f)) return(NULL)
          vars <- gsub("\\s*(\\d{7}_)", "", utils::unzip(f, list = TRUE)$Name)
          vars <- gsub("\\.tif$", "", vars)
          f <- gsub(paste0(get_dir(x), "/"), "", f,fixed=TRUE)
          f <- gsub("\\.zip", "", f)
          dl <- do.call(rbind, lapply(vars, function(x, y) c(y, x),
                                      unlist(strsplit(f, "/"))))
          n.col <- ncol(dl)
          dl <- dl[, c(n.col - 4, n.col - 3, n.col - 2, n.col)]
          data.list <- rbind(data.list, dl)
          allproducts <- allproducts[!grepl("mosaic", allproducts)]
        }
        if (any(grepl("CloudMask", allproducts))) {
          vars <- gsub("\\.zip$", "", allproducts[grepl("CloudMask",
                                                        allproducts)])
          vars <- unlist(strsplit(gsub(paste0(get_dir(x), "/"),
                                       "",
                                       vars),
                                  "/"))
          n.col <- length(vars)
          vars <- c(vars[n.col - 2:1], "", vars[n.col])
          data.list <- rbind(vars, data.list)
          allproducts <- allproducts[!grepl("CloudMask", allproducts)]
        }
        full.names <- list.files(allproducts, full.names = TRUE)
        full.names <- gsub(paste0(get_dir(x), "/"), "", full.names,fixed=TRUE)
        full.names <- gsub("\\.zip", "", full.names)
        dl <- do.call(rbind, strsplit(full.names, "/"))
        #if(!is.null(dl))
        #  dl[, 1] <- ""
        n.col <- ncol(dl)
        dl <- dl[, c(n.col - 3, n.col - 2, n.col-1, n.col)]
        data.list <- rbind(dl, data.list)
        df <- as.data.frame(data.list)
        colnames(df) <- c("satellite", "product", "stage", "variable")
        row.names(df) <- NULL
        return(df)
      })
    }), recursive = FALSE)
    if(is.null(allvariables)){
      message("There is no processed images in the rtoi.")
      return(NULL)
    }
    #as.data.frame(do.call(rbind, allvariables))
    #TODO change this and make it work well
    tmp <- sapply(as.data.frame(do.call(rbind, allvariables)), function(x) unlist(x))
    rownames(tmp) <- NULL
    df <- as.data.frame(tmp)
    row.names(df) <- NULL
    df
  }
)



setGeneric("read_rtoi_dir", function(x,
                                     rtoi_dir) {
  standardGeneric("read_rtoi_dir")
})
setMethod("read_rtoi_dir",
  signature = c("character", "character"),
  function(x, rtoi_dir) {
    if (x[3] == "mosaic") {
      mos.zip <- list.files(file.path(rtoi_dir, x[1], x[2], x[3]),
                            full.names = TRUE)
      # bands<-zip_list(mos.zip[1])$filename
      bands <- unzip(mos.zip[1], list = TRUE)$Name
      bands <- bands[grepl(x[4], bands)]
      return(file.path("/vsizip", mos.zip, bands))
    }
    if (x[4] == "CloudMask") {
      mos.zip <- file.path(rtoi_dir, x[1], x[2], paste0(x[4], ".zip"))
      # return(file.path("/vsizip",mos.zip,zip_list(mos.zip)$filename))
      return(file.path("/vsizip", mos.zip,
                       unzip(mos.zip, list = TRUE)$Name))
    }

    mos.zip <- file.path(rtoi_dir,
                         paste0(paste(unlist(x), collapse = "/"),
                                ".zip")
                         )
    # return(file.path("/vsizip",mos.zip,zip_list(mos.zip)$filename))
    return(file.path("/vsizip", mos.zip,
                     unzip(mos.zip, list = TRUE)$Name))
  }
)
