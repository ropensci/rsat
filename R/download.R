#' Downloads the images from a records/rtoi object
#'
#' @param x a records or an rtoi object.
#' @param db_path path where the images will be stored when using an rtoi.
#' By default, the path is defined by the rtoi.
#' @param out.dir path where the images will be stored when using a records.
#' @param ... additional arguments
#'
#' @include rtoi.R records.R
#' @examples
#' \dontrun{
#' library(rsat)
#'
#' # load navarre sf from the package
#' data(ex.navarre)
#'
#' # set the credentials
#' set_credentials("username","password")
#'
#' # path where the region is stored
#' rtoi.path <- tempdir()
#' # path where downloads are stored
#' db.path <- file.path(tempdir(),"DATABASE")
#' navarre<-new_rtoi("Navarre",
#'                   ex.navarre,
#'                   rtoi.path,
#'                   db.path)
#'
#' #Landsat-5
#' sat_search(region=navarre,
#'           product="LANDSAT_TM_C1",
#'           dates=as.Date("1988-08-01")+seq(1,35))
#' download(navarre)
#' }
#' @export
setGeneric("download", function(x,...) { standardGeneric("download")})

#' @rdname download
#' @aliases download,rtoi
setMethod(f="download",
          signature = c("rtoi"),
          function(x,db_path,...){
            #args<-list(...) remove out.dir from args
            #if("out.dir")
            if(missing(db_path)){
              if(x$db_path==""){
                stop("db_path not defined in rtoi. Define db_path or use records with out.dir argument.")
              }else{
                download(x=records(x),out.dir=x$db_path,...)
              }
            }else{
              x$db_path<-db_path
              download(x=records(x),out.dir=x$db_path,...)
            }
          }
)

#' @rdname download
#' @aliases download,records
setMethod(f="download",
          signature = c("records"),
          function(x,out.dir,...){
            #dates<-as.Date("2016-01-25")
            #out.dir<-"E:/testnewpackage"
            ordered<-FALSE
            ordered.list<-new('records')
            if(missing(out.dir))stop("out.dir needed for image downloading.")
            # petitions for order images
            message("Cheking records for long term access data.")
            for(i in length(x):1){
              out.name<-file.path(out.dir,get_file_path(x[i]))
              if(get_order(x[i])&!file.exists(out.name)){
                if(grepl("^Landsat",sat_name(x[i]))){
                  #ls order petition
                  con<-connection$getApi(api_name = get_api_name(x[i]))
                  con$espaOrderImage(names(x[i]))
                  ordered.list<-c(ordered.list,x[i])
                  x<-x[-i]
                }else if(grepl("^Sentinel",sat_name(x[i]))){
                  #sentinel petition
                  # con<-connection$getApi(api_name = get_api_name(x[i]))
                  # if(con$scihubIsLTA(get_download(x[i]))){#is lta?
                  #   message(paste0("Ordering ",names(x[i])," image."))
                  #   if((con$secureDownload(get_download(x[i]),file.path(tmpDir(),"tmpImg")))%in%c(500,403,500)){
                  #     message("Error ordering the image, try in other moment.")
                  #   }else{
                  #     ordered.list<-c(ordered.list,x[i])
                  #     x<-x[-i]
                  #   }
                  # }else{
                  #   get_order(x)[i]<-FALSE
                  # }
                }else{
                  message(paste0("Product not supported for downloading"))
                }
              }
            }
            # download images without orders
            if(length(x)>0){
              for(i in 1:length(x)){
                out.name<-file.path(out.dir,get_file_path(x[i]))
                dir.create(dirname(out.name), showWarnings = FALSE, recursive = TRUE)
                if(!file.exists(out.name)){
                  if(!get_order(x[i])){
                    con<-connection$getApi(api_name = get_api_name(x[i]))
                    message(paste0("Downloading ",names(x[i])," image."))
                    con$secureDownload(get_download(x[i]),out.name)
                  }
                }else{
                  message(paste0(names(x[i])," already in your database."))
                }
              }
            }


            # after download all data without order check and download ordered data
            while(length(ordered.list)>0){
              for(i in length(ordered.list):1){
                out.name<-file.path(out.dir,get_file_path(ordered.list[i]))
                dir.create(dirname(out.name), showWarnings = FALSE, recursive = TRUE)
                if(!file.exists(out.name)){
                  if(grepl("^Landsat",sat_name(ordered.list[i]))){
                    con$espaGetOrders()
                    if(con$espaDownloadsOrders(names(ordered.list[i]),out.name)){
                      ordered.list <- ordered.list[-i]
                    }
                  }else if(grepl("^Sentinel",sat_name(ordered.list[i]))){
                    #sentinel petition
                    con<-connection$getApi(api_name = get_api_name(ordered.list[i]))
                    html<-con$secureCall(gsub("/$value","",get_download(ordered.list[i]),fixed = TRUE))
                    html<-paste(html,collapse = "\n ")
                    if(gsub(".*d:Online>","",gsub("</d:Online.*","",html))=="true"){#is lta?
                      message(paste0("Downloading ",names(ordered.list[i])," image."))
                      con$secureDownload(get_download(ordered.list[i]),out.name)
                      ordered.list<-c(ordered.list,ordered.list[i])
                    }
                  }
                }else{
                  message(paste0(names(ordered.list[i])," already in your database."))
                  ordered.list <- ordered.list[-i]
                }
              }
              if(length(ordered.list)>0){
                message("Waiting for ordered images.")
                Sys.sleep(10)
              }
            }

          }
)
