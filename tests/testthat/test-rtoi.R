test_that("records test", {
  print("Testing rtoi...")
  data(ex.navarre)
  set_credentials("username", "password")
  rtoi.path <- tempdir()

  show_variables()

  # path where downloads are stored
  db.path <- file.path(tempdir(), "DATABASE")
  unlink(file.path(rtoi.path,"Navarre_rtoi"),recursive = T)
  navarre <- new_rtoi(
    "Navarre_rtoi",
    ex.navarre,
    rtoi.path,
    db.path
  )

  navarre <- read_rtoi(file.path(rtoi.path,"Navarre_rtoi"))

  #######################################################
  # Searching errors
  #######################################################

  # search mod09ga products
  sat_search(
    region = navarre,
    product = c("mod09ga", "LANDSAT_8_C1", "S2MSI2A"),
    dates = as.Date("2021-03-01") + seq(1, 20),
    verbose=TRUE,
    test.mode=TRUE
  )

  rcds <- records(navarre)
  rcds1 <- subset(rcds, unique(product(rcds))[1], "product")[1]
  rcds2 <- subset(rcds, unique(product(rcds))[2], "product")[1]
  rcds3 <- subset(rcds, unique(product(rcds))[3], "product")[1]
  rcds.subset <- c(rcds1, rcds2, rcds3)
  rcds.subset@preview<-paste0(
    "https://unai-perez.github.io/rsat-test/previewImg/",
    c("BROWSE.MOD09GA.A2021061.h17v04.006.2021063025039.1.jpg",
      "LC08_L1GT_199030_20210303_20210311_01_T2.jpg",
      "S2B_MSIL2A_20210321T104639_N0214_R051_T30TXM_20210321T134504-ql.jpg"))
  records(navarre)<-rcds.subset

  # records convertion
  as.records(as.data.frame(rcds.subset))

  print(navarre)
  navarre
  plot(navarre,"preview")
  plot(navarre,"dates")


  set_database(navarre,get_database(navarre))
  region(navarre)<-region(navarre)
  rename(navarre,names(navarre))
  dates(navarre)
  product(navarre)
  sat_name(navarre)

  tryCatch({
    plot(navarre,"view")
  }, error = function(e) {
    print(e)
  })

  preview(navarre,dates(navarre)[2])

  unlink(file.path(rtoi.path,"Navarre_rtoi"),recursive = T)
})
