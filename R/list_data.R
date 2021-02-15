#' Lists all the variables in rtoi
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#' @import zip
#'
#' @examples
setGeneric("list_data", function(x,
                                 ...) {
  standardGeneric("list_data")
})
setMethod("list_data",
          signature = c("rtoi"),
          function(x,...){
            allfiles<-list.files(get_dir(x),full.names = TRUE)
            allfiles<-allfiles[!grepl("\\.rtoi$",allfiles)]

            #satellite
           allvariables<-unlist(lapply(allfiles, function(af){
              #product
              lapply(list.files(af,full.names = TRUE), function(p){
                data.list<-list()
                #product
                allproducts<-list.files(p,full.names = TRUE)
                #remove preview
                allproducts<-allproducts[!grepl("preview",allproducts)]

                if(any(grepl("mosaic",allproducts))){
                  f<-list.files(allproducts[grepl("mosaic",allproducts)],full.names = TRUE)[1]
                  vars<-gsub("\\s*(\\d{7}_)", "", utils::unzip(f,list=TRUE)$Name)
                  vars<-gsub("\\.tif$","",vars)
                  f<-gsub(paste0(get_dir(x),"/"),"",f)
                  f<-gsub("\\.zip","",f)
                  data.list<-c(data.list,lapply(vars,function(x,y)c(y,x),unlist(strsplit(f,"/"))[-4]))
                  allproducts<-allproducts[!grepl("mosaic",allproducts)]
                }
                if(any(grepl("CloudMask",allproducts))){
                  vars<-gsub("\\.zip$","",allproducts[grepl("CloudMask",allproducts)])
                  vars<-unlist(strsplit(gsub(paste0(get_dir(x),"/"),"",vars),"/"))
                  vars<-c(vars[1:2],"",vars[3])
                  data.list<-c(data.list,list(vars))
                  allproducts<-allproducts[!grepl("CloudMask",allproducts)]
                }
                full.names<-list.files(allproducts,full.names = TRUE)
                full.names<-gsub(paste0(get_dir(x),"/"),"",full.names)
                full.names<-gsub("\\.zip","",full.names)
                data.list<-c(data.list,strsplit(full.names,"/"))
                df<-do.call(rbind,data.list)
                colnames(df)<-c("satellite","product","stage","variable")
                return(df)
              })
            }),recursive=FALSE)
            as.data.frame(do.call(rbind,allvariables))
          })



setGeneric("read_rtoi_dir", function(x,
                                     rtoi_dir) {
  standardGeneric("read_rtoi_dir")
})
setMethod("read_rtoi_dir",
          signature = c("character","character"),
          function(x,rtoi_dir){
            if(x[3]=="mosaic"){
              mos.zip<-list.files(file.path(rtoi_dir,x[1],x[2],x[3]),full.names = TRUE)
              #bands<-zip_list(mos.zip[1])$filename
              bands<-utils::unzip(mos.zip[1],list=T)$Name
              bands<-bands[grepl(x[4],bands)]
              return(file.path("/vsizip",mos.zip,bands))
            }
            if(x[4]=="CloudMask"){
              mos.zip<-file.path(rtoi_dir,x[1],x[2],paste0(x[4],".zip"))
              #return(file.path("/vsizip",mos.zip,zip_list(mos.zip)$filename))
              return(file.path("/vsizip",mos.zip,utils::unzip(mos.zip,list=T)$Name))
            }

            mos.zip<-file.path(rtoi_dir,paste0(paste(x,collapse = "/"),".zip"))
            #return(file.path("/vsizip",mos.zip,zip_list(mos.zip)$filename))
            return(file.path("/vsizip",mos.zip,utils::unzip(mos.zip,list=T)$Name))
            })

