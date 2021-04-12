test_that("records test", {
  data(ex.navarre)

  # credential test
  print_credentials()

  set_credentials("username", "password")

  set_credentials("username","password","scihub")

  credentials<-print_credentials()

  print(credentials)

  rcds<-sat_search(
    region = ex.navarre,
    product = c("mod09ga", "LANDSAT_8_C1","S2MSI2A"),
    dates = as.Date("2021-03-01") + seq(1, 20),
    verbose=TRUE,
    test.mode=TRUE
  )

  print(rcds)


})
