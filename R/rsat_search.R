#' Search satellite images
#'
#' Search satellite images concerning a particular location, data product, and
#' date interval. The function returns a \code{records} object if the
#' \code{region} is a \code{sf}. If an \code{rtoi} is used, the
#' function returns nothing and the records are added to the \code{rtoi}.
#'
#' MODIS images are found through the
#' \href{https://lpdaacsvc.cr.usgs.gov/services/inventory}{NASA Common
#'  Metadata Repository}
#' (CMR). The inventory of MODIS products can be found
#' \href{https://modis.gsfc.nasa.gov/data/dataprod}{here}.
#' The catalog shows the product short names and detailed information.
#' MODIS surface reflectance products are named `mod09ga' and `myd09ga' for
#' Terra and Aqua satellites. By the time \code{rsat} is
#' released, NASA carries out the maintenance of its website on Wednesdays,
#' which may cause an error when connecting to their server.
#'
#' We use \href{http://scihub.copernicus.eu}{ESA's powered API} (`SciHub') to
#' find Sentinel images. The catalog of Sentinel-2 and -3 products can be found
#' \href{https://sentinel.esa.int/web/sentinel/missions/sentinel-2/data-products}{here}
#' and
#' \href{https://sentinels.copernicus.eu/web/sentinel/missions/sentinel-3/data-products}{here},
#' respectively. Sentinel-2 and -3 surface reflectance product names are
#' referred to as `S2MSI2A' and `SY_2_SYN___'.
#'
#' Landsat images are accessed via the
#' \href{https://m2m.cr.usgs.gov/}{Machine-to-Machine API}.
#' Details about the Landsat products can be found
#' \href{https://www.usgs.gov/core-science-systems/nli/landsat/product-information}{here}.
#' The names of Landsat products are `LANDSAT_TM_C1', `LANDSAT_ETM_C1', and
#' `LANDSAT_8_C1' for missions 4-5, 7, and 8.
#'
#' @param region a \code{Spatial*}, \code{Raster*}, \code{sf} or \code{rtoi}
#' class objects defining the region of interest.
#' @param product a character vector of product names.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @param test.mode logical argument. If \code{TRUE}, the function gets test
#' data from github.
#' @param ... additional arguments for searching
#'
#' @export
#' @include search_sen.R search_ls.R search_mod.R
#' @examples
#' \dontrun{
#' library(rsat)
#' set_credentials("username", "password")
#'
#' # search navarre images using sf
#' record.list <- sat_search(
#'   region = ex.navarre,
#'   product = "mod09ga",
#'   dates = as.Date("2011-01-01") + seq(1, 10, 1)
#' )
#'
#' # creating a new rtoi
#' rtoi.path <- tempdir()
#' navarre <- new_rtoi(
#'   "Navarre", # name of the region
#'   ex.navarre, # sf of the region
#'   rtoi.path
#' ) # path for the rtoi
#'
#' # see the number of records in navarre
#' print(navarre)
#'
#' # search modis images using rtoi
#' sat_search(
#'   region = navarre,
#'   product = "mod09ga",
#'   dates = as.Date("2011-01-01") + seq(1, 10, 1)
#' )
#'
#' # see the number of records in navarre
#' print(navarre)
#'
#' # search landsat images using rtoi
#' sat_search(
#'   region = navarre,
#'   product = "LANDSAT_8_C1",
#'   dates = as.Date("2016-01-01") + seq(1, 30, 1)
#' )
#'
#' # see the number of records in navarre
#' print(navarre)
#'
#' # search sentinel-2 (level 1 and level 2) images using rtoi
#' sat_search(
#'   region = navarre,
#'   product = c("S2MSI1C", "S2MSI2A"),
#'   dates = as.Date("2016-01-01") + seq(1, 30, 1)
#' )
#'
#' # see the number of records in navarre
#' print(navarre)
#'
#' # search sentinel-3 level-2 images using rtoi
#' sat_search(
#'   region = navarre,
#'   product = "OL_2_LFR___",
#'   dates = as.Date("2019-01-01") + seq(1, 2, 1)
#' )
#'
#' # search sentinel-1 level-2 images using rtoi
#' sat_search(
#'   region = navarre,
#'   product = "GRD",
#'   dates = as.Date("2019-01-01") + seq(1, 2, 1)
#' )
#'
#' print(navarre)
#'
#' # get all records from rtoi
#' navarre.records <- records(navarre)
#'
#' print(navarre.records)
#' }
setGeneric("rsat_search", function(region, product, ...) {
  standardGeneric("rsat_search")
})

#' @rdname rsat_search
#' @aliases rsat_search,rtoi,character
setMethod(
  f = "rsat_search",
  signature = c("rtoi", "character"),
  function(region, product, verbose = FALSE, test.mode = FALSE,...) {
    searchres <- new("records")
    for (s in product) {
      if (tolower(substr(s, 1, 3)) %in% c("mod", "myd", "mcd")) {
        message(paste0("Searching scenes in the ", s, " product..."))
        aux <- mod_search(region(region),
                          product = s,
                          verbose = verbose,
                          test.mode = test.mode, ...)
        records.summary(aux)
        searchres <- c(searchres,aux)
      } else if (grepl("LANDSAT", s)) {
        message(paste0("Searching scenes in the ", s, " product..."))
        aux <- ls_search(region(region),
                         product = s,
                         verbose = verbose,
                         test.mode = test.mode, ...)
        records.summary(aux)
        searchres <- c(searchres,aux)
      } else if (s %in% unlist(SENPRODUCTS)) {
        message(paste0("Searching scenes in the ", s, " product..."))
        aux <- sen_search(region(region),
                          product = s,
                          verbose = verbose,
                          test.mode = test.mode, ...)
        records.summary(aux)
        searchres <- c(searchres,aux)
      } else {
        warning(paste0("Satellite not supported, only modis, ",
                       "landsat and sentinel related products."))
      }
    }
    if (length(searchres) != 0) {
      records(region) <- unique(c(records(region), searchres))
    }

  }
)

#' @rdname rsat_search
#' @aliases rsat_search,sf,character
setMethod(
  f = "rsat_search",
  signature = c("sf", "character"),
  function(region, product, verbose = FALSE, test.mode = FALSE,...) {
    searchres <- new("records")
    for (s in product) {
      if (tolower(substr(s, 1, 3)) %in% c("mod", "myd")) {
        aux <- mod_search(region,
                          product = s,
                          verbose = verbose,
                          test.mode = test.mode,
                          ...)
        records.summary(aux)
        searchres <- c(searchres,aux)
      } else if (grepl("LANDSAT", s)) {
        auxs <- ls_search(region,
                          product = s,
                          verbose = verbose,
                          test.mode = test.mode,
                          ...)
        records.summary(aux)
        searchres <- c(searchres,aux)
      } else if (s %in% unlist(SENPRODUCTS)) {
        aux <- sen_search(region,
                          product = s,
                          verbose = verbose,
                          test.mode = test.mode,
                          ...)
        records.summary(aux)
        searchres <- c(searchres,aux)
      } else {
        warning(paste0("Satellite not supported, only modis, ",
                       "landsat and sentinel related products."))
      }
    }
    return(searchres)
  }
)

records.summary<-function(r){
  if(length(r)>0){
    message(paste0("Attached ",length(r)," new entries of product ",product(r)[1]," to your rtoi."))
  }else{
    message(paste0("No entry found for product ",product(r),"."))
  }
}
