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
#' # create a copy of navarre in temp file
#' file.copy(from=system.file("ex/Navarre",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#'
#' # load example rtoi
#' navarre <- read_rtoi(file.path(tempdir(),"Navarre"))
#'
#' # assign the path of the database
#' set_database(file.path(tempdir(),"DATABASE"))
#' rsat_download(navarre)
#'
#' rcrds <-  records(navarre)
#'
#' rsat_download(rcrds)
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
      if (get_database(x) == "") {
        stop(paste0("db_path not defined in rtoi. Define db_path or",
                    " use records with out.dir argument."))
      } else {
        rsat_download(x = records(x),
                 out.dir = get_database(x),
                 test.mode = test.mode,
                 verbose = verbose,...)
      }
    } else {
      set_database(x,db_path)
      rsat_download(x = records(x),
               out.dir = get_database(x),
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
    args <- list(...)

    ordered <- FALSE
    ordered.list <- new("records")
    if (missing(out.dir)){
      if(get_database()==""){
        stop("out.dir or global environment database needed for image downloading.")
      }else{
        out.dir<-get_database()
      }
    }
    # petitions for order images
    message("Checking records for long term access data.")
    for (i in rev(seq_len(length(x)))) {
      out.name <- file.path(out.dir, get_file_path(x[i]))
      if (get_order(x[i]) & !file.exists(out.name)) {
        if (grepl("^Landsat", sat_name(x[i]))&!test.mode) {
          # ls order petition
          if("product"%in%names(args)){
            product<-args$product
          }else{
            product<-"sr"
          }
          con <- connection$getApi(api_name = get_api_name(x[i]))
          if (i == length(x)) {
            con$espaOrderImage(names(x[i]), verbose = verbose,product=product)
          } else {

            con$espaOrderImage(names(x[i]),
                               update.orders = FALSE,
                               verbose = verbose,
                               product=product)
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
