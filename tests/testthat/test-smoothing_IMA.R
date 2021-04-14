test_that("smoothing IMA", {

  # load an example of NDVI time series in Navarre
  data(ex.ndvi.navarre)

  # smoothin and fill all the time series

  tiles.mod.ndvi.filled <- smoothing_images(ex.ndvi.navarre,
                                            method = "IMA",
                                            only.na = TRUE
  )


})
