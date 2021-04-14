test_that("download test", {
  rcds.df<-as.data.frame(read.csv("https://github.com/unai-perez/unai-perez.github.io/raw/master/rsat-test/downloadImg/records.csv"))
  rcds.df$date<-as.Date(rcds.df$date,"%Y-%m-%d")
  rcds.df$path<-as.numeric(rcds.df$path)
  rcds.df$row<-as.numeric(rcds.df$row)
  rcds<-as.records(rcds.df)
  rcds

  dir.create(file.path(tempdir(),"Database"))
  set_credentials("username", "password")
  tryCatch({
    download(rcds[1],out.dir=file.path(tempdir(),"Database"),test.mode=T)
  }, error = function(e) {
    print(e)
  })

  download(rcds[2:3],out.dir=file.path(tempdir(),"Database"),test.mode=T)

  data(ex.navarre)
  rtoi.path <- tempdir()
  show_variables()

  # path where downloads are stored
  db.path <- file.path(tempdir(),"Database")
  unlink(file.path(rtoi.path,"Navarre_download"),recursive = T)
  navarre <- new_rtoi(
    "Navarre_download",
    ex.navarre,
    rtoi.path,
    db.path
  )
  records(navarre)<-rcds

  tryCatch({
    download(rcds[1],out.dir=file.path(tempdir(),"Database"),test.mode=T)
    mosaic(navarre)
    plot(navarre, "view", product = unique(product(navarre))[1])
  }, error = function(e) {
    mosaic(navarre)
  })

  plot(navarre, "view", product = unique(product(navarre))[2])
  plot(navarre, "view", product = unique(product(navarre))[3])

  print(navarre)
  tryCatch({
    list_data(navarre)
  }, error = function(e) {
  })

  derive(navarre,product="LANDSAT_8_C1_lvl2",variable="NDVI")
  derive(navarre,product="mod09ga",variable="NDVI")
  derive(navarre,product="S2MSI2A",variable="NDVI")


  tryCatch({
    plot(navarre,"view",variable="NDVI",product = "mod09ga")
  }, error = function(e) {
  })
  plot(navarre,"view",variable="NDVI",product = unique(product(navarre))[2])

  #plot(navarre,"view",variable="NDVI",product = unique(product(navarre))[3])# derive with s2
  navarre
  tryCatch({
    list_data(navarre)
    get_raster(navarre,p="LANDSAT_8_C1_lvl2",v="NDVI")
    get_stars(navarre,p="LANDSAT_8_C1_lvl2",v="NDVI")
  }, error = function(e) {
  })

  unlink(file.path(rtoi.path,"Navarre_download"),recursive = T)
})
