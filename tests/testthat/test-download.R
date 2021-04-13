test_that("download test", {
  rcds.df<-as.data.frame(read.csv("https://unai-perez.github.io/rsat-test/downloadImg/records.csv"))
  rcds.df$date<-as.Date(rcds.df$date,"%Y-%m-%d")
  rcds.df$path<-as.numeric(rcds.df$path)
  rcds.df$row<-as.numeric(rcds.df$row)
  rcds<-as.records(rcds.df)

  # rcds@download<-
  #          c("https://github.com/unai-perez/unai-perez.github.io/raw/master/rsat-test/downloadImg/MOD09GA.A2021061.h17v04.006.2021063025039.hdf",
  #            "https://github.com/unai-perez/unai-perez.github.io/raw/master/rsat-test/downloadImg/LC08_L1GT_062213_20210303_20210312_01_T2.tar.gz",
  #            "https://github.com/unai-perez/unai-perez.github.io/raw/master/rsat-test/downloadImg/S2B_MSIL2A_20210321T104639_N0214_R051_T30TXM_20210321T134504.zip")
  # write.csv(as.data.frame(rcds),"D:/OneDrive - UPNA/GitHub/unai-perez.github.io/rsat-test/downloadImg/records.csv",row.names =F)
  set_credentials("username", "password")
  download(rcds,out.dir=tempdir(),test.mode=T)

  data(ex.navarre)
  rtoi.path <- tempdir()
  show_variables()

  # path where downloads are stored
  #db.path <- file.path(tempdir(), "DATABASE")
  db.path <- file.path("E:/TESTDATABASE/Database")
  navarre <- new_rtoi(
    "Navarre2",
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
  #plot(navarre,"view",variable="NDVI",product = unique(product(navarre))[3])
  navarre
  list_data(navarre)
  get_raster(navarre,p="LANDSAT_8_C1_lvl2",v="NDVI")
  get_stars(navarre,p="LANDSAT_8_C1_lvl2",v="NDVI")
})
