test_that("smoothing_IMA", {
  if(require(terra)){
    library(rsat)
    # load an example of NDVI time series in Navarre
    data(ex.ndvi.navarre)

    # smoothin and fill all the time series
    tiles.mod.ndvi.filled <- rsat_smoothing_images(terra::rast(ex.ndvi.navarre),
                                                   method = "IMA",
                                                   only.na = TRUE,
                                                   Img2Fill =c(1),
                                                   predictSE = FALSE
    )
    testthat::expect_equal(6202.961, round(sum(values(tiles.mod.ndvi.filled)),3))

    tiles.mod.ndvi.filled <- rsat_smoothing_images(terra::rast(ex.ndvi.navarre),
                                                               method = "IMA",
                                                               Img2Fill =c(1),
                                                               only.na = TRUE,
                                                               predictSE = TRUE
    )
    testthat::expect_equal(6067.635, round(sum(values(tiles.mod.ndvi.filled)),3))
  }
})
