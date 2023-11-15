#' Download the images from a \code{records} or an \code{rtoi} object
#'
#' The function saves the raw images in the database or the specified directory.
#' It skips the images that already exist in the database or directory.
#'
#' @param x a \code{records} or an \code{rtoi} object.
#' @param db_path path to the database. By default, the path
#' is defined by the \code{rtoi}.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @param parallel logical argument. If \code{TRUE}, the function downloads from
#' multiples APIs in parallel.
#' @param ... additional arguments.
#' @return nothing. Downloads the images into your database
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
  function(x, db_path, verbose = FALSE, ...) {
    if (missing(db_path)){
      db_path <- get_database(x)
    }

    rsat_download(x = records(x),
                  db_path = db_path,
                  verbose = verbose,
                  ...)
  }
)

#' @rdname rsat_download
#' @aliases rsat_download,records
#' @importFrom parallel mclapply
setMethod(
  f = "rsat_download",
  signature = c("records"),
  function(x, db_path, verbose = FALSE,parallel=FALSE, ...) {
    args <- list(...)

    if (missing(db_path)){
        db_path <- get_database(x)
        if(db_path==""){
          stop("db_path or global environment database needed for image downloading.")
        }
    }
    #filter records
    usgs <- x[get_api_name(x)%in%"usgs"]
    x <- x[!(get_api_name(x)%in%"usgs")]
    dataspace <- x[get_api_name(x)%in%"dataspace"]
    x <- x[!(get_api_name(x)%in%"dataspace")]
    lpdaac <- x[get_api_name(x)%in%"lpdaac"]
    x <- x[!(get_api_name(x)%in%"lpdaac")]
    # run download
    if(parallel){
      functions_list <- list(
        list(func = connection$getApi("lpdaac")$download_lpdaac_records,
             args = list(lpdaac_records=lpdaac,db_path=db_path,verbose=verbose,...)),
        list(func = connection$getApi("dataspace")$dataspace_download_records,
             args = list(records=dataspace,db_path=db_path,verbose=verbose,...)),
        list(func = connection$getApi("usgs")$espa_order_and_download,
             args = list(usgs=usgs,db_path=db_path,verbose=verbose,...))
      )
      mclapply(functions_list, function(entry) {
        do.call(entry$func, entry$args)
      }, mc.cores = 3)
    }else{
      if(length(usgs)>0){
        espa.orders <- connection$getApi("usgs")$order_usgs_records(usgs,
                                                                    db_path,
                                                                    verbose,
                                                                    ...)
      }
      if(length(lpdaac)>0){
        connection$getApi("lpdaac")$download_lpdaac_records(lpdaac,
                                                            db_path,
                                                            verbose,...)
      }
      if(length(dataspace)>0){
        connection$getApi("dataspace")$dataspace_download_records(dataspace,
                                                                  db_path,
                                                                  verbose,...)
      }
      if(length(usgs)>0){
        connection$getApi("usgs")$download_espa_orders(espa.orders,
                                                       db_path,
                                                       verbose,...)
      }
    }
  }
)






