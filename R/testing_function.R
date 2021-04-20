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
}
