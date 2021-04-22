#' Testing function
#'
#' Function used for testing some internal functions in continious integration.
#'
#' @return NULL
#' @export
#'
#' @examples
test_function<-function(){
  lsGetPathRow("LC8200030")
  lsGetDates ("LC80390222013076EDC00")
  lsGetDates ("LC80390222013076EDC00",format="%Y%j")
  json_list<-NULL
  json_list$a<-list(b=1)
  toEspaJSON(json_list, is.array = c("products", "inputs"))

  f<-list.files(file.path(system.file (package = 'rsat'),
                          "data"),recursive=TRUE,full.names = TRUE)
  md5<-md5sum(f[1])
  genCheckMD5(f[1],md5[[1]])
  genCheckMD5(f[1],paste0(md5[[1]],"1"))

  variables$EVI(1,2,3)
  variables$NBR(1,2)
  variables$NBR2(1,1)
  variables$NDMI(1,2)
  variables$RGB(raster(matrix(1)),
                raster(matrix(1)),
                raster(matrix(1)))
  variables$SAVI(1,2)


  new_extent_crs()
  new_extent_crs(1,2,1,1,1)
  new_extent_crs(0,2,1,1,1)
}
