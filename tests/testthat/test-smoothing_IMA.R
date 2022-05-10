test_that("smoothing IMA", {

  # load an example of NDVI time series in Navarre
  data(ex.ndvi.navarre)

  # smoothin and fill all the time series
  tiles.mod.ndvi.filled <- rsat_smoothing_images(rast(ex.ndvi.navarre),
                                            method = "IMA",
                                            only.na = TRUE,
                                            Img2Fill =c(1),
                                            predictSE = FALSE
  )


  tiles.mod.ndvi.filled <- suppressWarnings(rsat_smoothing_images(rast(ex.ndvi.navarre),
                                                             method = "IMA",
                                                             Img2Fill =c(1),
                                                             only.na = TRUE,
                                                             predictSE = TRUE
  ))


})
