test_that("download test", {
  rcds.df<-as.data.frame(read.csv("https://unai-perez.github.io/rsat-test/downloadImg/records.csv"))
  rcds.df$date<-as.Date(rcds.df$date,"%Y-%m-%d")
  rcds.df$path<-as.numeric(rcds.df$path)
  rcds.df$row<-as.numeric(rcds.df$row)
  rcds<-as.records(rcds.df)

  set_credentials("username", "password")
  download(rcds,out.dir=tempdir(),test.mode=T)

  data(ex.navarre)
  rtoi.path <- tempdir()
  show_variables()

  # path where downloads are stored
  db.path <- file.path(tempdir(),"Database")
  navarre <- new_rtoi(
    "Navarre",
    ex.navarre,
    rtoi.path,
    db.path
  )
  records(navarre)<-rcds
  mosaic(navarre)
  plot(navarre, "view", product = unique(product(navarre))[1])
  plot(navarre, "view", product = unique(product(navarre))[2])
  plot(navarre, "view", product = unique(product(navarre))[3])

  list_data(navarre)

  derive(navarre,product="LANDSAT_8_C1_lvl2",variable="NDVI")
  derive(navarre,product="mod09ga",variable="NDVI")
  derive(navarre,product="S2MSI2A",variable="NDVI")
  plot(navarre,"view",variable="NDVI",product = unique(product(navarre))[1])
  plot(navarre,"view",variable="NDVI",product = unique(product(navarre))[2])
  #plot(navarre,"view",variable="NDVI",product = unique(product(navarre))[3])# derive with s2
  navarre
  list_data(navarre)
  get_raster(navarre,p="LANDSAT_8_C1_lvl2",v="NDVI")
  get_stars(navarre,p="LANDSAT_8_C1_lvl2",v="NDVI")
})
