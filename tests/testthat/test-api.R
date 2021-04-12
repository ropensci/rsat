test_that("api test", {
  data(ex.navarre)

  set_credentials("username","password")

  records<-sat_search(
    region = ex.navarre,
    product = c("mod09ga", "LANDSAT_8_C1","S2MSI2A"),
    dates = as.Date("2021-03-01") + seq(1, 20),
    verbose=TRUE,
    test.mode=TRUE
  )

  rtoi.path <- tempdir()

  show_variables()

  # path where downloads are stored
  db.path <- file.path(tempdir(), "DATABASE")
  navarre <- new_rtoi(
    "Navarre",
    ex.navarre,
    rtoi.path,
    db.path
  )

  # search mod09ga products
  sat_search(
    region = navarre,
    product = c("mod09ga", "LANDSAT_8_C1", "S2MSI2A"),
    dates = as.Date("2021-03-01") + seq(1, 20),
    verbose=T,
    test.mode=TRUE
  )

  print(navarre)
  navarre
})
