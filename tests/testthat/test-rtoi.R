test_that("records test", {
  print("Testing rtoi...")
  data(ex.navarre)
  set_credentials("username", "password")
  rtoi.path <- tempdir()

  show_variables()

  # path where downloads are stored
  db.path <- file.path(tempdir(), "DATABASE")
  unlink(file.path(rtoi.path,"Navarre_rtoi"),recursive = T)

  navarre.create <- new_rtoi(
    "Navarre_rtoi",
    ex.navarre,
    rtoi.path,
    db.path
  )

  navarre <- read_rtoi(file.path(rtoi.path,"Navarre_rtoi"))
  testthat::expect_equivalent(navarre.create, navarre)
  testthat::expect_equivalent(region(navarre),region(navarre.create))

  #######################################################
  # Searching usgs
  #######################################################
  tryCatch({
  # search mod09ga products
  rsat_search(
    region = navarre,
    product = c("mod09ga"),
    dates = as.Date("2021-03-01") + seq(1, 2),
    verbose=TRUE
  )
  testthat::expect_equal(length(records(navarre)),2)

  rsat_products()
  rsat_search(
    region = navarre,
    product = c("S2MSI1C"),
    dates = as.Date("2021-03-01") + seq(1, 2),
    verbose=TRUE
  )
  testthat::expect_equal(length(records(navarre)),6)

  rcds <- records(navarre)
  rcds1 <- subset(rcds, "product", unique(product(rcds))[1])
  rcds2 <- subset(rcds, "product", unique(product(rcds))[2])

  testthat::expect_equal(length(rcds1),2)
  testthat::expect_equal(length(rcds2),4)
  testthat::expect_equal(length(rcds1[1]),1)

  rcds.subset <- c(rcds1, rcds2)
  testthat::expect_equal(length(rcds.subset),6)

  # records convertion
  df <- as.data.frame(rcds1)

  result<-structure(list(sat = c("Modis", "Modis"), name = c("MOD09GA.A2021061.h17v04.061.2021063030716",
                                                             "MOD09GA.A2021062.h17v04.061.2021065061324"), date = structure(c(18688,
                                                                                                                              18689), class = "Date"), product = c("mod09ga", "mod09ga"), path = c(17,
                                                                                                                                                                                                   17), row = c(4, 4), tileid = c("", ""), download = c("https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/MOD09GA.061/MOD09GA.A2021061.h17v04.061.2021063030716/MOD09GA.A2021061.h17v04.061.2021063030716.hdf",
                                                                                                                                                                                                                                                        "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/MOD09GA.061/MOD09GA.A2021062.h17v04.061.2021065061324/MOD09GA.A2021062.h17v04.061.2021065061324.hdf"
                                                                                                                                                                                                   ), file_path = c("Modis/mod09ga/MOD09GA.A2021061.h17v04.061.2021063030716.hdf",
                                                                                                                                                                                                                    "Modis/mod09ga/MOD09GA.A2021062.h17v04.061.2021065061324.hdf"
                                                                                                                                                                                                   ), preview = c("https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-public/MOD09GA.061/MOD09GA.A2021061.h17v04.061.2021063030716/BROWSE.MOD09GA.A2021061.h17v04.061.2021063030716.1.jpg",
                                                                                                                                                                                                                  "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-public/MOD09GA.061/MOD09GA.A2021062.h17v04.061.2021065061324/BROWSE.MOD09GA.A2021062.h17v04.061.2021065061324.1.jpg"
                                                                                                                                                                                                   ), api_name = c("lpdaac", "lpdaac"), order = c(FALSE, FALSE),
                         EPSG = c(54008, 54008), xmin = c(-1114607.45209148, -1114607.45209148
                         ), ymin = c(4425384.39406491, 4425384.39406491), xmax = c(-64.497012082233,
                                                                                   -64.497012082233), ymax = c(5535855.37721878, 5535855.37721878
                                                                                   )), class = "data.frame", row.names = c(NA, -2L))

  testthat::expect_equal(df,result)

  rcds <- as.records(df)
  testthat::expect_equal(rcds,rcds1)

  print(navarre)
  navarre

  plot(navarre,"preview")

  #plot(navarre,"dates")


  # set_database(navarre,get_database(navarre))
  # region(navarre)<-region(navarre)
  # dates(navarre)
  # product(navarre)
  # sat_name(navarre)
  #
  #
  # plot(navarre,"view",product="mod09ga")
  #
  #
  # rsat_preview(navarre,dates(navarre)[2])
  # }, error = function(e) {
  #   print(e)
  },error = function(e) {
    print("Offline services...")
  })
  #unlink(file.path(rtoi.path,"Navarre_rtoi"),recursive = T)
})
