test_that("search test", {
   set_credentials("rgistools","EspacialUPNA88")
   rtoi.path <- tempdir()

   show_variables()

   # path where downloads are stored
   db.path <- file.path(tempdir(),"DATABASE")
   navarre<-new_rtoi("Navarre",
                     ex.navarre,
                     rtoi.path,
                     db.path)



    # search mod09ga products
    sat_search(region=navarre,
               product=c("mod09ga","LANDSAT_8_C1","S2MSI2A"),
               dates=as.Date("2021-03-01")+seq(1,20))
    plot(navarre, y="dates")
    # plot the quicklook images before the download


    # select partially cloud free
    rcds <-  records(navarre)
    rcds <- records(navarre)[c(1,2)]
    dates(rcds)

    rcds <-  records(navarre)

    rcds1 <- subset(rcds,unique(product(rcds))[1],"product")[1]
    rcds2 <- subset(rcds,unique(product(rcds))[2],"product")[1]
    rcds3 <- subset(rcds,unique(product(rcds))[3],"product")[1]
    records(navarre)<-c(rcds1,rcds2,rcds3)
    plot(c(rcds1,rcds2,rcds3))
    plot(navarre, y="preview")
    preview(navarre,dates(navarre)[1])
    preview(navarre,dates(navarre)[2])
    preview(navarre,dates(navarre)[3])
    download(navarre)
    mosaic(navarre)
    # plot mosaicked images
    plot(navarre,"view", product=unique(product(navarre))[1])
    #plot(navarre,"view", product=unique(product(navarre))[2])
    #plot(navarre,"view", product=unique(product(navarre))[3])

    # plot with false color
    derive(navarre,product="mod09ga","NDVI")

    plot(navarre,"view",variable="NDVI",product="mod09ga")
    cloud_mask(navarre)
    r.stack<-get_raster(navarre,"mod09ga","NDVI")
    get_stars(navarre,"mod09ga","NDVI")
    get_stars(navarre,"mod09ga","NDVI")
    tiles.mod.ndvi.filled  <- smoothing_images(ex.ndvi.navarre,
                                               method="IMA",
                                               only.na=TRUE)
})
