setGeneric("sen_query", function(server,
                                 product,
                                ...) {
  standardGeneric("sen_query")
})
#' @importFrom utils URLencode
setMethod(f="sen_query",
          signature = c("character","character"),
          function(server,product,...){
            arg<-list(...)
            query<-file.path(server,"search?q=")
            if(!"verbose"%in%names(arg)){
              arg$verbose=FALSE
            }
            #add ingestion date to query
            if("startDate"%in%names(arg)){
              if(arg$verbose)
                message("Adapting dates.")
              startDate<-paste0(format(arg$startDate,"%Y-%m-%d"),"T00:00:00.000Z")
              if(is.null(arg$endDate)){
                endDate<-"NOW"
              }else{
                endDate<-paste0(format(arg$endDate,"%Y-%m-%d"),"T23:59:59.999Z")
              }
              query<-paste0(query,"beginposition:[",startDate," TO ",endDate,"]")
            }
            if("platform"%in%names(arg)){
              if(arg$verbose)
                message("Adding platform name.")
              query<-paste0(query," AND platformname:",arg$platform)
            }
            if("extent"%in%names(arg)){
              stopifnot(class(extent(arg$extent))=="Extent")
              if(arg$verbose)
                message("Adding query extent.")
              ext<-extent(arg$extent)
              query<-paste0(query," AND footprint:",'"',"intersects(POLYGON((",ext@xmin," ",ext@ymin,","
                          ,ext@xmin," ",ext@ymax,","
                          ,ext@xmax," ",ext@ymax,","
                          ,ext@xmax," ",ext@ymin,","
                          ,ext@xmin," ",ext@ymin,")))",'"')

            }
            if("lonlat"%in%names(arg)){
              if(arg$verbose){
                message(print("Adding query intersects"))
              }
              if(!length(arg$lonlat)==2){
                stop("The intersects argument is not a longitude/latitude valid location.")
              }
              query<-paste0(query," AND footprint:",'"',"intersects(",arg$lonlat[1],", ",arg$lonlat[2],")",'"')
            }
            if("region"%in%names(arg)){
              if(arg$verbose){
                message(print("Adding query region"))
              }
              arg$region<-transform_multiple_proj(arg$region, proj4=st_crs(4326))
              ext<-st_bbox(arg$region)
              query<-paste0(query," AND footprint:",'"',"intersects(POLYGON((",ext$xmin," ",ext$ymin,","
                          ,ext$xmin," ",ext$ymax,","
                          ,ext$xmax," ",ext$ymax,","
                          ,ext$xmax," ",ext$ymin,","
                          ,ext$xmin," ",ext$ymin,")))",'"')
            }
            if(!missing(product)){
              if(arg$verbose){
                message("Added product type.")
              }
              query<-paste0(query," AND producttype:",product)
            }else{
              stop("product must to be defined for sentinel image searching.")
            }
            if("relativeorbit"%in%names(arg)){
              if(arg$verbose){
                message("Added relative orbit number type.")
              }
              query<-paste0(query," AND relativeorbitnumber:",arg$relativeorbit)
            }
            if("timeliness"%in%names(arg)){
              if(arg$verbose){
                message("Added timeliness.")
              }
              url<-paste0(url,' AND timeliness:"',arg$timeliness,'"')
            }
            if("cloudCover"%in%names(arg)){
              if(arg$verbose){
                message("Added cloud cover percentage.")
              }
              query<-paste0(query," AND cloudcoverpercentage:[",min(arg$cloudCover)," TO ",max(arg$cloudCover),"]")
            }
            if("qformat"%in%names(arg)){
              query<-paste0(query,"&format=",arg$qformat)
            }else{
              query<-paste0(query,"&format=json")
            }
            query<-paste0(query,"&rows=100")
            return(URLencode(query))
          }
)




setGeneric("sen_search", function(region,
                                  product,
                                  ...) {
  standardGeneric("sen_search")
})
setMethod(f="sen_search",
          signature = c("ANY","character"),
          function(region,
                   product,
                   dates,
                   startDate,
                   endDate,
                   verbose=FALSE,
                   ...){
            if(!missing(dates)){
              startDate<-min(dates)
              endDate<-max(dates)
            }

            if(product%in%SENPRODUCTS$`Sentinel-5`){
              apiname<-"scihubs5p"
            }else{
              apiname<-"scihub"
            }
            con <- connection$getApi(apiname)

            query <- sen_query(server =  con$getServer(),
                               product = product,
                               startDate = startDate,
                               endDate = endDate,
                               region = region,
                               verbose=verbose,
                               ...)

            res.download <- fromJSON(con$secureCall(query))
            ndownload<-as.numeric(res.download$feed$`opensearch:totalResults`)
            if(!is.na(ndownload)&ndownload>0){
              name<-c()
              download<-c()
              bounds<-c()
              dates<-c()
              path<-c()
              tileid<-c()
              sat<-c()
              for(img in res.download$feed$entry){
                name<-c(name,img$title)
                download<-c(download,img$link[[1]]$href)
                dates<-c(dates,as.Date(img$date[[1]]$content))
                for(s in img$str){
                  if(s$name=="footprint"){
                    coords<-as.numeric(unlist(strsplit(gsub("  "," ",gsub(","," ",gsub("\\)","",gsub(".*\\(","",s$content))))," ")))
                    nc<-length(coords)
                    x<-coords[seq(1,nc,2)]
                    y<-coords[seq(2,nc,2)]
                  }
                  if(s$name=="tileid"){
                    tileid<-c(tileid,s$content)
                  }
                  if(s$name=="platformname"){
                    sat<-c(sat,s$content)
                  }
                }

                pth<-unlist(img$int[[2]])
                if(length(pth)==2){
                  path<-c(path,as.numeric(pth[2]))
                }else{
                  path<-c(path,as.numeric(pth))
                }


                bounds<-rbind(bounds,c(xmin=min(x),ymin=min(y),xmax=max(x),ymax=max(y)))
              }
              nlen=length(name)
              if(is.null(tileid)){
                tileid<-rep("",nlen)
              }

              #TODO define order by product
              #order = con$scihubIsLTA(download)
              order = rep(FALSE,nlen)


              records<-new_record(sat = sat,
                                  name = name,
                                  date = as.Date(dates),
                                  product= rep(product,nlen),
                                  download = download,
                                  file_path = file.path(sat,product,paste0(name,".zip")),
                                  path = path,
                                  row = rep(0,nlen),
                                  tileid = tileid,
                                  preview = gsub('$value',"Products('Quicklook')/$value",download,	fixed = TRUE),
                                  api_name = rep(apiname,nlen),
                                  order = order,
                                  extent_crs = new("extent_crs",
                                                   EPSG=rep(4326,nlen),
                                                   xmin=bounds[,"xmin"],
                                                   ymin=bounds[,"ymin"],
                                                   xmax=bounds[,"xmax"],
                                                   ymax=bounds[,"ymax"]))

              if(ndownload>100){
                dt<-dates(records)
                mn.date<-min(dt)
                if(sum(dt==mn.date)==100){
                  stop("\nSpatial regions composed by 100 or more tiles are not supported!
           Try the search using a smaller region.")
                }
                if(sum(dt==mn.date)>50){
                  mn.date=mn.date-1
                }
                records<-c(records,sen_search(region=region,
                                              product=product,
                                              startDate=startDate,
                                              endDate=mn.date,
                                              verbose=verbose,
                                              ...))

              }
              return(records)
            }else{
              message("There is no images for your region and periods.")
            }

          }
)


