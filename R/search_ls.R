#' @import rjson
#'
setGeneric("ls_query", function(server,
                                datasetName,
                                startDate,
                                endDate,
                                sf.obj,
                                apiKey,
                                ...) {
  standardGeneric("ls_query")
})
setMethod(f="ls_query",
          signature = c("character","character","Date","Date","sf","character"),
          function(server,datasetName,startDate,endDate,sf.obj,apiKey,...){
            args<-list(...)
            lsquery<-NULL
            #temporal filter
            lsquery$datasetName<- datasetName#'LANDSAT_8_C1'
            lsquery$temporalFilter<-list("startDate"=format(startDate,"%d-%m-%Y"),
                                         "endDate"=format(endDate,"%d-%m-%Y"))
            #spatial filter
            lsquery$spatialFilter<-list("filterType"='mbr',
                                        "lowerLeft"=list("latitude"=st_bbox(sf.obj)[["ymin"]],
                                                         "longitude"=st_bbox(sf.obj)[["xmin"]]),
                                        "upperRight"=list("latitude"=st_bbox(sf.obj)[["ymax"]],
                                                          "longitude"=st_bbox(sf.obj)[["xmax"]]))
            if("cloudCover"%in%names(args)){
              if(length(args$cloudCover)==2&&class(args$cloudCover)=="numeric"){
                lsquery$minCloudCover<-min(args$cloudCover)
                lsquery$maxCloudCover<-max(args$cloudCover)
                if("includeUnknownCloudCover"%in%names(args)){
                  lsquery$includeUnknownCloudCover<-args$includeUnknownCloudCover
                }else{
                  lsquery$includeUnknownCloudCover<-"true"
                }
              }else{stop("cloudCover must be a numeric argument")}
            }


            #additional criteria
            lsquery$maxResults<-50000
            lsquery$startingNumber<-1
            lsquery$sortOrder<-"ASC"
            lsquery$apiKey<-apiKey
            return(paste0(server,'/search?jsonRequest=',toJSON(lsquery)))
          }
)


setGeneric("ls_search", function(region,
                                 product,
                                 ...) {
  standardGeneric("ls_search")
})
setMethod(f="ls_search",
          signature = c("ANY","character"),
          function(region,
                   product,
                   startDate,
                   endDate,
                   dates,
                   logoout=TRUE,
                   lvl=1,
                   ...){
            if(!missing(dates)){
              startDate<-min(dates)
              endDate<-max(dates)
            }
            con <- connection$getApi("earthexplorer")
            attempts<-5
            repeat{
              query <- ls_query(server=con$api_server,
                                datasetName=product,
                                startDate=startDate,
                                endDate=endDate,
                                sf.obj=region,
                                apiKey=con$api_key,
                                ...)
              jsonres<-fromJSON(con$autoCall(query))
              attempts<-attempts-1
              if(jsonres$error!="Could not find api key"){
                break
              }
              if(attempts==-1){stop("Could not find api key")}
              con$loginEEApiKey()
            }

            if(jsonres$data$numberReturned==0) return(new("records"))
            ##################################################################################################
            res.df<-data.frame(t(sapply(jsonres$data$results,c)))



            satid<-basename(dirname(unlist(res.df$metadataUrl)[1]))#12864 for landsat 8

            #boundaries for previsualization
            bounds<-lapply(unlist((res.df["sceneBounds"])),function(x){return(as.numeric(unlist(strsplit(x,","))))})
            bounds<-t(sapply(bounds,c))
            rownames(bounds)<-NULL
            colnames(bounds)<-c("LongitudeMin","LatitudeMin","LongitudeMax","LatitudeMax")

            switch(lvl,
                   "1"={
                     api_name="earthexplorer"
                     img.name<-unlist(res.df$entityId)
                     nlen<-length(img.name)
                     download_url=file.path(con$server,"download",satid,unlist(res.df$entityId),"STANDARD/EE/")
                     pr<-lsGetPathRow(img.name)
                     path = as.numeric(substr(pr,1,3))
                     row = as.numeric(substr(pr,4,6))
                     d<-lsGetDates(img.name)
                     order = rep(FALSE,nlen)
                     fe<-".tar.gz"
                   },
                   "2"={
                     api_name="ESPA"
                     img.name<-unlist(res.df$displayId)
                     nlen<-length(img.name)
                     download_url=paste0(connection$getApi("ESPA")$api_server,'/available-products/', res.df$displayId)
                     pr<-gsub(".*_\\s*(\\d{6})_.*","\\1",img.name)
                     path = as.numeric(substr(pr,1,3))
                     row = as.numeric(substr(pr,4,6))
                     d<-as.Date(gsub(".*?\\s*(\\d{8}).*","\\1",img.name),"%Y%m%d")
                     order = rep(TRUE,nlen)
                     fe<-".tar.gz"
                   })

            ############################################################################################
            #if(logout){logoutEEAPI(verbose)}
            #file extension
            if(startsWith(product,"LANDSAT_8")){
              sat<-"ls8"
            }else if(startsWith(product,"LANDSAT_7")){
              sat<-"ls7"
            }else{
              sat<-"ls"
              fe<-".tar.gz"
            }


            return(new_record(sat = rep(sat,nlen),
                              name = img.name,
                              date = d,
                              product= rep(paste0(product,"_lvl",lvl),nlen),
                              download = download_url,
                              file_path=file.path(sat,paste0(product,"_lvl",lvl),paste0(img.name,fe)),
                              path = path,
                              row = row,
                              tileid = rep("",nlen),
                              preview =unlist(res.df$browseUrl),
                              api_name = rep(api_name,nlen),
                              order = order,
                              extent_crs = new("extent_crs",
                                               EPSG=rep(4326,nlen),
                                               xmin=bounds[,"LongitudeMin"],
                                               ymin=bounds[,"LatitudeMin"],
                                               xmax=bounds[,"LongitudeMax"],
                                               ymax=bounds[,"LatitudeMax"])
                              ))
          }
)



