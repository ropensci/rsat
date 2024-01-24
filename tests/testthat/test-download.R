test_that("download test", {
  data(ex.navarre)
  rcds.df<-as.data.frame(read.csv(text = '"sat","name","date","product","path","row","tileid","download","file_path","preview","api_name","order","EPSG","xmin","ymin","xmax","ymax"
"Modis","MOD09GA.A2021061.h17v04.006.2021063025039",2021-03-02,"mod09ga",17,4,"","https://e4ftl01.cr.usgs.gov//DP109/MOLT/MOD09GA.006/2021.03.02/MOD09GA.A2021061.h17v04.006.2021063025039.hdf","Modis/mod09ga/MOD09GA.A2021061.h17v04.006.2021063025039.hdf","https://unai-perez.github.io/rsat-test/previewImg/BROWSE.MOD09GA.A2021061.h17v04.006.2021063025039.1.jpg","nasa_inventory",FALSE,54008,-1114607.45209148,4425384.39406524,-64.4970120822515,5535855.37722017
"Landsat-8","LC08_L1GT_062213_20210303_20210312_01_T2",2021-03-03,"LANDSAT_8_C1_lvl2",62,213,"","https://github.com/unai-perez/unai-perez.github.io/raw/master/rsat-test/download/LC082000302020030701T1-SC20210408154403.tar.gz","Landsat-8/LANDSAT_8_C1_lvl2/LC08_L1GT_062213_20210303_20210312_01_T2.tar.gz","https://unai-perez.github.io/rsat-test/previewImg/LC08_L1GT_199030_20210303_20210311_01_T2.jpg","ESPA",TRUE,4326,-2.85461,40.69499,-0.0597,42.82014
"Sentinel-2","S2B_MSIL2A_20210321T104639_N0214_R051_T30TXM_20210321T134504",2021-03-21,"S2MSI2A",51,0,"30TXM","https://github.com/unai-perez/unai-perez.github.io/raw/master/rsat-test/download/S2B_MSIL2A_20210321T104639_N0214_R051_T30TXM_20210321T134504.zip","Sentinel-2/S2MSI2A/S2B_MSIL2A_20210321T104639_N0214_R051_T30TXM_20210321T134504.zip","https://unai-perez.github.io/rsat-test/previewImg/S2B_MSIL2A_20210321T104639_N0214_R051_T30TXM_20210321T134504-ql.jpg","scihub",FALSE,4326,-1.5993042,41.4363363800955,-0.44979858,42.438125424303'))

  rcds.df$date<-as.Date(rcds.df$date,"%Y-%m-%d")
  rcds.df$path<-as.numeric(rcds.df$path)
  rcds.df$row<-as.numeric(rcds.df$row)
  rcds<-as.records(rcds.df)
  rcds

  dir.create(file.path(tempdir(),"Database"), showWarnings = FALSE)
  set_credentials("username", "password")

  data(ex.navarre)
  rtoi.path <- tempdir()

  # path where downloads are stored
  db.path <- file.path(tempdir(),"Database")
  unlink(file.path(rtoi.path,"Navarre_download"),recursive = TRUE)
  navarre <- new_rtoi(
    "Navarre_download",
    ex.navarre,
    rtoi.path,
    db.path
  )
  testthat::expect_equal(dir.exists(file.path(rtoi.path,"Navarre_download")), TRUE)

  #read rtoi
  navarre.back <- read_rtoi(file.path(rtoi.path,"Navarre_download"))
  testthat::expect_equivalent(navarre, navarre.back)

  set_credentials("rsat.package", "UpnaSSG.2021")
  tryCatch({
    rsat_search(
      region = navarre,
      product = c("mod09ga"),
      dates = as.Date("2021-03-01") + seq(1, 2),
      verbose=TRUE
    )
    testthat::expect_equal(length(records(navarre)), 2)
  }, error = function(e) {
    print(e)
  })


  tryCatch({
    r<-rsat_search(
      region = ex.navarre,
      product = c("landsat_ot_c2_l2"),
      dates = as.Date("2021-03-01") + seq(1, 20),
      verbose=TRUE
    )
    testthat::expect_equal(length(r), 8)
    records(navarre)<-c(records(navarre),r[1])
    testthat::expect_equal(length(records(navarre)), 3)
  }, error = function(e) {
    print(e)
  })

  records(navarre)<-rcds



  tryCatch({
    # load example rtoi
    file.copy(from=system.file("ex/Navarre",package="rsat"),
              to=tempdir(),
              recursive = TRUE)

    navarre <- read_rtoi(file.path(tempdir(),"Navarre"))

    set_database(file.path(tempdir(),"Database"))
    print(navarre)

    # plot the calendar
    plot(navarre, "dates")

    # replace with your own "username" and "password"
    set_credentials("rsat.package", "UpnaSSG.2021")

    # plot the quicklook images before the download
    # needs credentials to download preview images
    plot(navarre,"preview",dates=as.Date("2020-01-24"))

    # select partially cloud free
    rcds <- records(navarre)
    rcds <- rcds[dates(rcds) %in% as.Date(c("20210310", "20210313"), "%Y%m%d")]
    records(navarre) <- rcds

    plot(navarre, "preview")

    file.copy(from=system.file("ex/Pamplona",package="rsat"),
              to=tempdir(),
              recursive = TRUE)
    # plot already mosaicked rtoi ("view" mode)
    pamplona <- read_rtoi(file.path(tempdir(),"Pamplona"))

    rsat_list_data(pamplona)

    # plot can compute the rgb image on the fly from mosaicek bands
    plot(pamplona, "view", product="mod09ga")

    # plot on the fly with false color
    plot(pamplona, "view",
         product = "mod09ga",
         band_name = c("nir", "red", "green"))

    file.copy(from=system.file("ex/PamplonaDerived",package="rsat"),
              to=tempdir(),
              recursive = TRUE)
    # plot already mosaicked rtoi ("view" mode)
    pamplona.derived <- read_rtoi(file.path(tempdir(),"PamplonaDerived"))

    data<-rsat_list_data(pamplona.derived)
    res<- structure(list(satellite = c("Modis", "Modis", "Modis", "Modis",
                                       "Modis", "Modis", "Modis", "Modis", "Modis", "Modis", "Modis",
                                       "Modis", "Modis", "Modis", "Modis", "Modis", "Modis", "Modis",
                                       "Modis", "Modis", "Modis", "Modis", "Modis"), product = c("mod09ga",
                                                                                                 "mod09ga", "mod09ga", "mod09ga", "mod09ga", "mod09ga", "mod09ga",
                                                                                                 "mod09ga", "mod09ga", "mod09ga", "mod09ga", "mod09ga", "mod09ga",
                                                                                                 "mod09ga", "mod09ga", "mod09ga", "mod09ga", "mod09ga", "mod09ga",
                                                                                                 "mod09ga", "mod09ga", "mod09ga", "mod09ga"), stage = c("variables",
                                                                                                                                                        "mosaic", "mosaic", "mosaic", "mosaic", "mosaic", "mosaic", "mosaic",
                                                                                                                                                        "mosaic", "mosaic", "mosaic", "mosaic", "mosaic", "mosaic", "mosaic",
                                                                                                                                                        "mosaic", "mosaic", "mosaic", "mosaic", "mosaic", "mosaic", "mosaic",
                                                                                                                                                        "mosaic"), variable = c("NDVI", "MODIS_Grid_1km_2D_num_observations_1km",
                                                                                                                                                                                "MODIS_Grid_1km_2D_state_1km_1", "MODIS_Grid_1km_2D_SensorZenith_1",
                                                                                                                                                                                "MODIS_Grid_1km_2D_SensorAzimuth_1", "MODIS_Grid_1km_2D_Range_1",
                                                                                                                                                                                "MODIS_Grid_1km_2D_SolarZenith_1", "MODIS_Grid_1km_2D_SolarAzimuth_1",
                                                                                                                                                                                "MODIS_Grid_1km_2D_gflags_1", "MODIS_Grid_1km_2D_orbit_pnt_1",
                                                                                                                                                                                "MODIS_Grid_1km_2D_granule_pnt_1", "MODIS_Grid_500m_2D_num_observations_500m",
                                                                                                                                                                                "MODIS_Grid_500m_2D_sur_refl_b01_1", "MODIS_Grid_500m_2D_sur_refl_b02_1",
                                                                                                                                                                                "MODIS_Grid_500m_2D_sur_refl_b03_1", "MODIS_Grid_500m_2D_sur_refl_b04_1",
                                                                                                                                                                                "MODIS_Grid_500m_2D_sur_refl_b05_1", "MODIS_Grid_500m_2D_sur_refl_b06_1",
                                                                                                                                                                                "MODIS_Grid_500m_2D_sur_refl_b07_1", "MODIS_Grid_500m_2D_QC_500m_1",
                                                                                                                                                                                "MODIS_Grid_500m_2D_obscov_500m_1", "MODIS_Grid_500m_2D_iobs_res_1",
                                                                                                                                                                                "MODIS_Grid_500m_2D_q_scan_1")), class = "data.frame", row.names = c(NA,
                                                                                                                                                                                                                                                     -23L))
    testthat::expect_equal(data, res)

    # plot derived variables
    plot(pamplona.derived, "view",
         product = "mod09ga",
         variable = "NDVI")

    # Set the max and min value in plot
    plot(pamplona.derived,"view",
         variable="NDVI",
         product="mod09ga",
         zlim=c(0,1))
  }, error = function(e) {

  })

  tryCatch({
    file.copy(from=system.file("ex/PamplonaDerived",package="rsat"),
              to=tempdir(),
              recursive = TRUE)

    # load example rtoi
    pamplona.derived <- read_rtoi(file.path(tempdir(),"PamplonaDerived"))

    # print available variables
    rsat_list_data(pamplona.derived)

    # get RasterStack from raster package
    suppressWarnings(mod.ndvi.raster <-
                       rsat_get_raster(pamplona.derived, "mod09ga", "NDVI"))
    plot(mod.ndvi.raster)

    # get spatraster from terra package
    mod.ndvi.rast <- rsat_get_SpatRaster(pamplona.derived, "mod09ga", "NDVI")
    plot(mod.ndvi.rast)
    testthat::expect_equal(as.integer(sum(terra::values(mod.ndvi.rast))),as.integer(2667.518))

    # get stars from stars package
    suppressWarnings(mod.ndvi.stars <-
                       rsat_get_stars(pamplona.derived, "mod09ga", "NDVI"))
    plot(mod.ndvi.stars)

    ## get any band in rtoi
    # list available data
    rsat_list_data(pamplona.derived)
    # select band 1: MODIS_Grid_500m_2D_sur_refl_b01_1
    mod.ndvi.rast <- rsat_get_SpatRaster(pamplona.derived,
                                         "mod09ga",
                                         "MODIS_Grid_500m_2D_sur_refl_b01_1")
    testthat::expect_equal(as.integer(sum(terra::values(mod.ndvi.rast))),80746650000)
    plot(mod.ndvi.rast)
  }, error = function(e) {
    print(e)
  })

  tryCatch({
    # create a copy of pamplona in temp file
    file.copy(from=system.file("ex/Pamplona",package="rsat"),
              to=tempdir(),
              recursive = TRUE)

    # load example rtoi
    pamplona <- read_rtoi(file.path(tempdir(),"Pamplona"))

    rsat_cloudMask(pamplona)

    rsat_list_data(pamplona)
  }, error = function(e) {
    print(e)
  })

  unlink(file.path(rtoi.path,"Navarre_download"),recursive = T)
})
