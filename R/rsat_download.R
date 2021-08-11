#' Download the images from a \code{records} or an \code{rtoi} object
#'
#' The function saves the raw images in the database or the specified directory.
#' It skips the images that already exist in the database or directory.
#'
#' @param x a \code{records} or an \code{rtoi} object.
#' @param db_path path to the database. By default, the path
#' is defined by the \code{rtoi}.
#' @param out.dir path where the outputs are stored when using a \code{records}.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @param test.mode logical argument. If \code{TRUE}, the function gets test
#' data from github.
#' @param ... additional arguments.
#'
#' @include rtoi.R records.R
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
#' )
#'
#' # Landsat-5
#' sat_search(
#'   region = navarre,
#'   product = "LANDSAT_TM_C1",
#'   dates = as.Date("1988-08-01") + seq(1, 35)
#' )
#' rsat_download(navarre)
#' }
#' @export
setGeneric("rsat_download", function(x, ...) {
  standardGeneric("rsat_download")
})

#' @rdname rsat_download
#' @aliases rsat_download,rtoi
setMethod(
  f = "rsat_download",
  signature = c("rtoi"),
  function(x, db_path, verbose = FALSE, test.mode = FALSE, ...) {
    if (missing(db_path)) {
      if (x$db_path == "") {
        stop(paste0("db_path not defined in rtoi. Define db_path or",
                    " use records with out.dir argument."))
      } else {
        rsat_download(x = records(x),
                 out.dir = x$db_path,
                 test.mode = test.mode,
                 verbose = verbose,...)
      }
    } else {
      x$db_path <- db_path
      rsat_download(x = records(x),
               out.dir = x$db_path,
               test.mode = test.mode,
               verbose = verbose,
               ...)
    }
  }
)

#' @rdname rsat_download
#' @aliases rsat_download,records
setMethod(
  f = "rsat_download",
  signature = c("records"),
  function(x, out.dir, verbose = FALSE, test.mode = FALSE, ...) {
    # dates<-as.Date("2016-01-25")
    # out.dir<-"E:/testnewpackage"
    ordered <- FALSE
    ordered.list <- new("records")
    if (missing(out.dir)) stop("out.dir needed for image downloading.")
    # petitions for order images
    message("Checking records for long term access data.")
    for (i in rev(seq_len(length(x)))) {
      out.name <- file.path(out.dir, get_file_path(x[i]))
      if (get_order(x[i]) & !file.exists(out.name)) {
        if (grepl("^Landsat", sat_name(x[i]))&!test.mode) {
          # ls order petition
          con <- connection$getApi(api_name = get_api_name(x[i]))
          if (i == length(x)) {
            con$espaOrderImage(names(x[i]), verbose = verbose)
          } else {
            con$espaOrderImage(names(x[i]),
                               update.orders = FALSE,
                               verbose = verbose)
          }
          ordered.list <- c(ordered.list, x[i])
          x <- x[-i]
        } else if (grepl("^Sentinel", sat_name(x[i]))&!test.mode) {
          # sentinel petition
          # con<-connection$getApi(api_name = get_api_name(x[i]))
          # if(con$scihubIsLTA(get_download(x[i]))){#is lta?
          #   message(paste0("Ordering ",names(x[i])," image."))
          #   if((con$secureDownload(get_download(x[i]),
          #                          file.path(tempdir(),
          #                                    "tmpImg")))%in%c(500,403,500)){
          #     message("Error ordering the image, try in other moment.")
          #   }else{
          #     ordered.list<-c(ordered.list,x[i])
          #     x<-x[-i]
          #   }
          # }else{
          #   get_order(x)[i]<-FALSE
          # }
        }
      }
    }
    # download images without orders

    for (i in seq_len(length(x))) {
      out.name <- file.path(out.dir, get_file_path(x[i]))
      dir.create(dirname(out.name), showWarnings = FALSE, recursive = TRUE)
      if (!file.exists(out.name)) {
        if (!test.mode) {
          con <- connection$getApi(api_name = get_api_name(x[i]))
          message(paste0("Downloading ", names(x[i]), " image."))
          con$secureDownload(get_download(x[i]), out.name)
        }else{
          con <- connection$getApi(api_name = get_api_name(x[i]))
          message(paste0("Downloading ", names(x[i]), " image."))
          con$pictureDownload(get_download(x[i]), out.name)
        }
      } else {
        message(paste0(names(x[i]), " already in your database."))
      }
    }



    # after download all data without order check and download ordered data
    while (length(ordered.list) > 0) {
      for (i in rev(seq_len(length(ordered.list)))) {
        out.name <- file.path(out.dir, get_file_path(ordered.list[i]))
        dir.create(dirname(out.name),
                   showWarnings = FALSE,
                   recursive = TRUE)
        if (!file.exists(out.name)) {
          if (grepl("^Landsat", sat_name(ordered.list[i]))) {
            con <- connection$getApi(api_name = get_api_name(ordered.list[i]))
            con$espaGetOrders(verbose = verbose)
            if (con$espaDownloadsOrders(names(ordered.list[i]),
                                        out.name,
                                        verbose = verbose)) {
              ordered.list <- ordered.list[-i]
            }
          } else if (grepl("^Sentinel", sat_name(ordered.list[i]))) {
            # sentinel petition
            con <- connection$getApi(api_name = get_api_name(ordered.list[i]))
            html <- con$secureCall(gsub("/$value", "",
                                        get_download(ordered.list[i]),
                                        fixed = TRUE))
            html <- paste(html, collapse = "\n ")
            if (gsub(".*d:Online>", "", gsub("</d:Online.*", "", html)) ==
                "true"){ # is lta?
              message(paste0("Downloading ",
                             names(ordered.list[i]),
                             " image."))
              con$secureDownload(get_download(ordered.list[i]), out.name)
              ordered.list <- c(ordered.list, ordered.list[i])
            }
          }
        } else {
          message(paste0(names(ordered.list[i]), " already in your database."))
          ordered.list <- ordered.list[-i]
        }
      }
      if (length(ordered.list) > 0) {
        message("Waiting for ordered images.")
        Sys.sleep(10)
      }
    }
  }
)
