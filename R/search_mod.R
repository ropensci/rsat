setGeneric("mod_query", function(server,
                                 product,
                                 collection,
                                 dates,
                                 startDate,
                                 endDate,
                                 lonlat,
                                 extent,
                                 region,
                                 resType,
                                 ...) {
  standardGeneric("mod_query")
})
setMethod(f="mod_query",
          signature = c("character","character","numeric","Date","missing","missing","numeric","missing","missing","character"),
          function(server,product,collection,dates,lonlat,resType,...){
            return(paste0(server,
                       "?product=",product,
                       "&version=",collection,
                       "&latitude=",lonlat[2],
                       "&longitude=",lonlat[1],
                       "&return=",resType,
                       "&date=",format(min(dates),"%Y-%m-%d"),
                       ",",format(max(dates),"%Y-%m-%d")))
          }
)
setMethod(f="mod_query",
          signature = c("character","character","numeric","missing","Date","Date","numeric","missing","missing","character"),
          function(server,product,collection,startDate,endDate,lonlat,resType,...){
            return(paste0(server,
                       "?product=",product,
                       "&version=",collection,
                       "&latitude=",lonlat[2],
                       "&longitude=",lonlat[1],
                       "&return=",resType,
                       "&date=",format(startDate,"%Y-%m-%d"),
                       ",",format(endDate,"%Y-%m-%d")))
          }
)

setMethod(f="mod_query",
          signature = c("character","character","numeric","Date","missing","missing","missing","ANY","missing","character"),
          function(server,product,collection,dates,extent,resType,...){
            stopifnot(class(extent(extent))=="Extent")
            return(paste0(server,
                       "?product=",product,
                       "&version=",collection,
                       "&bbox=",paste0(c(extent),collapse = ","),
                       "&return=",resType,
                       "&date=",format(min(dates),"%Y-%m-%d"),
                       ",",format(max(dates),"%Y-%m-%d")))
          }
)
setMethod(f="mod_query",
          signature = c("character","character","numeric","missing","Date","Date","missing","ANY","missing","character"),
          function(server,product,collection,startDate,endDate,extent,resType,...){
            stopifnot(class(extent(extent))=="Extent")
            return(paste0(server,
                       "?product=",product,
                       "&version=",collection,
                       "&bbox=",paste0(c(extent),collapse = ","),
                       "&return=",resType,
                       "&date=",format(startDate,"%Y-%m-%d"),
                       ",",format(endDate,"%Y-%m-%d")))
          }
)
setMethod(f="mod_query",
          signature = c("character","character","numeric","Date","missing","missing","missing","missing","ANY","character"),
          function(server,product,collection,dates,region,resType,...){
            region<-transform_multiple_proj(region, proj4=st_crs(4326))
            return(paste0(server,
                       "?product=",product,
                       "&version=",collection,
                       "&bbox=",paste0(st_bbox(region),collapse = ","),
                       "&return=",resType,
                       "&date=",format(min(dates),"%Y-%m-%d"),
                       ",",format(max(dates),"%Y-%m-%d")))
          }
)
setMethod(f="mod_query",
          signature = c("character","character","numeric","missing","Date","Date","missing","missing","ANY","character"),
          function(server,product,collection,startDate,endDate,region,resType,...){
            region<-transform_multiple_proj(region, proj4=st_crs(4326))
            return(paste0(server,
                       "?product=",product,
                       "&version=",collection,
                       "&bbox=",paste0(st_bbox(region),collapse = ","),
                       "&return=",resType,
                       "&date=",format(startDate,"%Y-%m-%d"),
                       ",",format(endDate,"%Y-%m-%d")))
          }
)


#' @import XML
setGeneric("mod_search", function(region,
                                  ...) {
  standardGeneric("mod_search")
})
setMethod(f="mod_search",
          signature = c("ANY"),
          function(region,
                   collection=6,
                   ...){
            args<-list(...)
            con <- connection$getApi("nasa_inventory")
            query <- mod_query(server =  con$getServer(),
                               #product = "mod09ga",
                               collection = collection,
                               #dates = dates,
                               region = region,
                               resType="url",#)
                               ...)
            if("verbose"%in%names(args)) message(query)
            res.download <- con$simpleCall(query)
            res.download <- xmlRoot(xmlNativeTreeParse(res.download))
            res.download <- xmlSApply(res.download,
                                      function(x) xmlSApply(x,xmlValue))

            res.preview <- con$simpleCall(gsub("url","browseurl",query))
            res.preview <- xmlRoot(xmlNativeTreeParse(res.preview))
            res.preview <- xmlSApply(res.preview,
                                     function(x) xmlSApply(x,xmlValue))


            pr<-modGetPathRow(res.download)
            pt<-as.numeric(substr(pr,2,3))
            rw<-as.numeric(substr(pr,5,6))
            bounds<-c()
            for(n in paste0("h:",pt," v:",rw)){
              bounds<-rbind(bounds,st_bbox(extent(st_transform(mod.tiles[mod.tiles$Name==n,],crs=st_crs("ESRI:54008")#"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"#st_crs(54008)
                                                               ))))
            }

            nlen<-length(res.download)
            prdc<-list(...)$product
            img.name<-gsub("\\.hdf","",basename(res.download))
            return(new_record(sat = rep("Modis",nlen),
                              name = img.name,
                              date = modGetDates(res.download),
                              product= rep(prdc,nlen),
                              download = res.download,
                              file_path = file.path("Modis",prdc,paste0(img.name,".hdf")),
                              path = as.numeric(substr(pr,2,3)),
                              row = as.numeric(substr(pr,5,6)),
                              tileid = rep("",nlen),
                              preview = res.preview,
                              api_name = rep("nasa_inventory",nlen),
                              order = rep(FALSE,nlen),
                              extent_crs = new("extent_crs",
                                               EPSG=rep(54008,nlen),
                                               #EPSG=st_crs("ESRI:54008"),
                                               xmin=bounds[,"xmin"],
                                               ymin=bounds[,"ymin"],
                                               xmax=bounds[,"xmax"],
                                               ymax=bounds[,"ymax"]))
                   )
          }
)




