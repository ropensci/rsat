#' Testing function
#'
#' Function used for testing some internal functions in continuous integration.
#'
#' @return NULL
#' @export
#' @importFrom utils data
#' @examples
#' test_function()
test_function<-function(){
  ex.navarre<-rsat::ex.navarre

  lsGetPathRow("LC8200030")
  lsGetDates ("LC80390222013076EDC00")
  lsGetDates ("LC80390222013076EDC00",format="%Y%j")
  json_list<-NULL
  json_list$a<-list(b=1)
  toEspaJSON(json_list, is.array = c("products", "inputs"))

  f<-list.files(system.file (package = 'rsat'),
                recursive=TRUE,full.names = TRUE)[1]
  md5<-md5sum(f[1])
  genCheckMD5(f[1],md5[[1]],verbose=TRUE)
  genCheckMD5(f[1],paste0(md5[[1]],"1"),verbose=TRUE)

  variables$EVI(1,2,3)
  variables$NBR(1,2)
  variables$NBR2(1,1)
  variables$NDMI(1,2)
  variables$RGB(raster(matrix(1)),
                raster(matrix(1)),
                raster(matrix(1)))
  variables$SAVI(1,2)
  variables$MSAVI2(1,2)
  variables$NDWI(1,2)


  print(new_extent_crs())
  print(new_extent_crs(1,2,1,1,1))
  new_extent_crs(0,2,1,1,1)

  mod_query(server="aa",
            product="",
            collection=6,
            dates=as.Date("2021-11-01"),
            lonlat=c(1,2),
            resType="url")

  mod_query(server="aa",
            product="",
            collection=6,
            startDate=as.Date("2021-11-01"),
            endDate=as.Date("2021-11-01"),
            lonlat=c(1,2),
            resType="url")


  mod_query(server="aa",
            product="",
            collection=6,
            dates=as.Date("2021-11-01"),
            extent=extent(1,1,1,1),
            resType="url")

  mod_query(server="aa",
            product="",
            collection=6,
            startDate=as.Date("2021-11-01"),
            endDate=as.Date("2021-11-01"),
            extent=extent(1,1,1,1),
            resType="url")

  mod_query(server="aa",
            product="",
            collection=6,
            startDate=as.Date("2021-11-01"),
            endDate=as.Date("2021-11-01"),
            extent=extent(1,1,1,1),
            resType="url")

  mod_query(server="aa",
            product="",
            collection=6,
            startDate=as.Date("2021-11-01"),
            endDate=as.Date("2021-11-01"),
            region=ex.navarre,
            resType="url")

  mod_query(server="aa",
            product="",
            collection=6,
            startDate=as.Date("2021-11-01"),
            endDate=as.Date("2021-11-01"),
            region=ex.navarre,
            resType="url")
  ls_query(server="a",
           datasetName="a",
           startDate=as.Date("2021-11-01"),
           endDate=as.Date("2021-11-01"),
           sf.obj=ex.navarre,
           apiKey="a",
           cloudCover=c(0,20))
  sen_query("aa","aa",verbose=TRUE,
            startDate=as.Date("2021-11-01"),
            platform="a",
            extent=extent(1,1,1,1),
            lonlat=c(1,1),
            relativeorbit="",
            cloudCover=c(0,20),
            qformat="json")

  new("api")
  new_record(sat = "character",
             name = "character",
             date = as.Date("2021-11-01"),
             product = "character",
             download = "character",
             file_path = "character",
             path =2,
             row = 1,
             tileid = "character",
             preview = "character",
             api_name = "character",
             order = FALSE
  )
}
