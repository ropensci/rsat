test_that("download test", {
  data(ex.navarre)
  rcds.df<-as.data.frame(read.csv(paste0("https://github.com/unai-perez/",
    "unai-perez.github.io/raw/master/rsat-test/downloadImg/records.csv")))
  rcds.df$date<-as.Date(rcds.df$date,"%Y-%m-%d")
  rcds.df$path<-as.numeric(rcds.df$path)
  rcds.df$row<-as.numeric(rcds.df$row)
  rcds<-as.records(rcds.df)
  rcds

  dir.create(file.path(tempdir(),"Database"))
  set_credentials("username", "password")

  data(ex.navarre)
  rtoi.path <- tempdir()

  # path where downloads are stored
  db.path <- file.path(tempdir(),"Database")
  unlink(file.path(rtoi.path,"Navarre_download"),recursive = T)
  tryCatch({
    navarre <- new_rtoi(
      "Navarre_download",
      ex.navarre,
      rtoi.path,
      db.path
    )
  }, error = function(e) {
    print(e)
  })
  navarre <- read_rtoi(file.path(rtoi.path,"Navarre_download"))


  set_credentials("rsat.package", "UpnaSSG.2021")
  tryCatch({
    rsat_search(
      region = navarre,
      product = c("mod09ga"),
      dates = as.Date("2021-03-01") + seq(1, 2),
      verbose=TRUE
    )
    rsat_download(navarre,test.mode=T)
  }, error = function(e) {
    print(e)
  })

  tryCatch({
    r<-rsat_search(
      region = ex.navarre,
      product = c("LANDSAT_8_C1"),
      dates = as.Date("2021-03-01") + seq(1, 20),
      verbose=TRUE
    )
    records(navarre)<-c(records(navarre),r[1])
    rsat_download(r[1],out.dir=file.path(tempdir(),"Database"))
  }, error = function(e) {
    print(e)
  })

  # tryCatch({
  #   r<-sat_search(
  #     region = ex.navarre,
  #     product = c("S2MSI2A"),
  #     dates = as.Date("2021-03-01") + seq(1, 20),
  #     verbose=TRUE
  #   )
  #   records(navarre)<-c(records(navarre),r[1])
  #   download(r[1],out.dir=file.path(tempdir(),"Database"))
  # }, error = function(e) {
  #   print(e)
  # })

  records(navarre)<-rcds
  rsat_download(rcds[2:3],out.dir=file.path(tempdir(),"Database"),test.mode=T)
  #records(navarre)<-rcds

  tryCatch({
    rsat_mosaic(navarre)
    plot(navarre, "view", product = unique(product(navarre))[1])
  }, error = function(e) {
    rsat_mosaic(navarre)
  })
  plot(navarre, "view", product = unique(product(navarre))[2])
  plot(navarre, "view", product = unique(product(navarre))[3])

  print(navarre)
  tryCatch({
    rsat_list_data(navarre)
  }, error = function(e) {
  })

  rsat_derive(navarre,product="LANDSAT_8_C1_lvl2",variable="NDVI")
  rsat_derive(navarre,product="mod09ga",variable="NDVI")

  rsat_derive(navarre, "NDVI", product = "S2MSI2A",fun= function(red, blue) {
    ndvi <- (blue - red) / (blue + red)
    return(ndvi)
  })


  tryCatch({
    rsat_list_data(navarre)[rsat_list_data(navarre)$variable=="NDVI",]
  }, error = function(e) {
    print(e)
  })

  tryCatch({
    rsat_derive(navarre,product="mod09ga",variable="NDVI")
    suppressWarnings(plot(navarre,"view",
                          variable="NDVI",
                          product = "mod09ga"))
    suppressWarnings(rsat_smoothing_images(navarre,"IMA",
                                      variable="NDVI",
                                      product = "mod09ga",
                                      test.mode=TRUE))
  }, error = function(e) {
    print(e)
  })

  #plot(navarre,"view",variable="NDVI",product = unique(product(navarre))[2])
  navarre
  tryCatch({
    rsat_list_data(navarre)
  }, error = function(e) {
    print(e)
  })

  tryCatch({
    rsat_get_raster(navarre,p="LANDSAT_8_C1_lvl2",v="NDVI")
    rsat_get_stars(navarre,p="LANDSAT_8_C1_lvl2",v="NDVI")
  }, error = function(e) {
    print(e)
  })

  tryCatch({
    rsat_cloud_mask(navarre,products="mod09ga")
    rsat_cloud_mask(navarre,products="S2MSI2A")

    rsat_list_data(navarre)
    rsat_get_raster(navarre,p="mod09ga",v="CloudMask")
    rsat_get_raster(navarre,p="S2MSI2A",v="CloudMask")
    rsat_cloud_mask(navarre,products="LANDSAT_8_C1_lvl2")
  }, error = function(e) {
    print(e)
  })

  suppressWarnings(test_function())

  unlink(file.path(rtoi.path,"Navarre_download"),recursive = T)
})
