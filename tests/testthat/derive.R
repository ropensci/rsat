test_that("derive", {
  tryCatch({
    # create a copy of pamplona in temp file
    file.copy(from=system.file("ex/Pamplona",package="rsat"),
              to=tempdir(),
              recursive = TRUE)

    # load example rtoi
    pamplona <- read_rtoi(file.path(tempdir(),"Pamplona"))

    rsat_list_data(pamplona)
    # show prefedined varibles
    show_variables()
    rsat_derive(pamplona, "NDVI", product = "mod09ga")
    # now NDVI is processed
    rsat_list_data(pamplona)

    # ad-hoc variable
    NDSI = function(green, swir1){
      ndsi <- (green - swir1)/(green + swir1)
      return(ndsi)
    }
    rsat_derive(pamplona, "NDSI", product = "mod09ga",fun=NDSI)
    # now NDVI is processed
    rsat_list_data(pamplona)
    plot(pamplona, product="mod09ga",variable="NDSI")
  }, error = function(e) {
    print(e)
  })
})
