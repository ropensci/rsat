test_that("records test", {
  print("Testing records...")
  data(ex.navarre)

  # credential test
  print_credentials()

  set_credentials("username", "password")
  res <- structure(c("lpdaac", "usgs", "dataspace", "earthdata", "earthdata",
                     "dataspace", "username", "username", "username", "password",
                     "password", "password"), dim = 3:4, dimnames = list(NULL, c("Api_name",
                                                                                 "Portal", "Username", "Password")))
  credentials<-print_credentials()

  testthat::expect_equal(print_credentials(), res)

  set_credentials("dataspace","pass","dataspace")
  res <- structure(c("lpdaac", "usgs", "dataspace", "earthdata", "earthdata",
                     "dataspace", "username", "username", "dataspace", "password",
                     "password", "pass"), dim = 3:4, dimnames = list(NULL, c("Api_name",
                                                                             "Portal", "Username", "Password")))

  credentials <- print_credentials()
  testthat::expect_equal(print_credentials(), res)

  print(credentials)

  tryCatch({
  rcds<-rsat_search(
    region = ex.navarre,
    product = c("mod09ga",# "LANDSAT_8_C1",
                "S2MSI2A"),
    dates = as.Date("2021-03-01") + seq(1, 20),
    verbose=TRUE
  )

  dates(rcds)
  print(rcds)
  print(rcds[1])

  rcds1 <- subset(rcds, "product", unique(product(rcds))[1])[1]
  rcds2 <- subset(rcds, "product", unique(product(rcds))[2])[1]
 # rcds3 <- subset(rcds, "product", unique(product(rcds))[3])[1]

  rcds.subset <- c(rcds1, rcds2)#, rcds3)

  rcds.subset@preview<-paste0(
    "https://unai-perez.github.io/rsat-test/previewImg/",
    c("BROWSE.MOD09GA.A2021061.h17v04.006.2021063025039.1.jpg",
      "LC08_L1GT_199030_20210303_20210311_01_T2.jpg",
      "S2B_MSIL2A_20210321T104639_N0214_R051_T30TXM_20210321T134504-ql.jpg"))

  plot(rcds.subset,"preview")

  rsat_preview(rcds.subset,1)
  rsat_preview(rcds.subset,dates(rcds.subset[1]))
  }, error = function(e) {
    print(e)
  })
})
