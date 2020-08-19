#' Region and Time Of Interest (rtoi)
#'
#' It is a proxy object to store the metadata about the satellite imagery
#' that covers a spatial region over a time period. Images can come from
#' multiple satellite programs and its purpose is to help managing
#' heterogeneous datasets.
#'
#' An \code{rtoi} object manages two main folders called database and rtoi.
#' The database is meant to work as a local, generic, and organized archive
#' of raw satellite images. When the function \code{download} is called
#' with an rtoi, images go to the database.
#' In contrast, the rtoi folder contains just the relevant information for
#' a particular region and time of interest. It contains a \code{region.rtoi}
#' file which saves all the metadata about that region/time of interest
#' and satellite imagery available. Also, when the function \code{mosaic} is
#' run, the relevant images from the database are cropped, mosaicked, and saved
#' in the rtoi folder.
#'
#' @field name a character with the name of the region of interest
#' @field rtoi_path a character with the path to the rtoi folder.
#' @field region an sf with the region of interest.
#' @field records the satellite records available for your region and time of interest.
#' @field db_path a character with the path to the database.
#'
#' @exportClass rtoi
#' @import sf
#' @include records.R
#'
#' @examples
#' \dontrun{
#' data(ex.navarre)
#'
#' # path where the region is stored
#' rtoi.path <- tempdir()
#'
#' # path where downloads are stored
#' db.path <- file.path(tempdir(),"DATABASE")
#' navarre<-new_rtoi(name="Navarre",
#'                   region=ex.navarre,
#'                   rtoi_path=rtoi.path,
#'                   db_path=db.path)
#'
#' print(navarre)
#'
#' navarre.records <- records(navarre)
#'
#' print(navarre.records)
#' }
setRefClass("rtoi",
            # Define the slots
            fields = list(
              name = "character",
              rtoi_path = "character",
              region = "list",
              records = "records",
              db_path = "character",
              size = "numeric"
            )
)

#' Creates a new rtoi object
#'
#' @param name the name of the region of interest.
#' @param region an sf object.
#' @param records a records object.
#' @param db_path the path to the database.
#' @param rtoi_path the path to the rtoi folder.
#'
#' @return the reference of the rtoi object
#' @exportMethod new_rtoi
setGeneric("new_rtoi", function(name, region, rtoi_path, db_path, records) {
  standardGeneric("new_rtoi")
})

#' @rdname new_rtoi
#' @aliases new_rtoi,character,sf,character,character,missing
setMethod("new_rtoi",
          signature(name = "character",
                    region = "sf",
                    rtoi_path = "character",
                    db_path = "character",
                    records = "missing"),
          function(name, region, rtoi_path, db_path) {
            rtoi_path<-file.path(rtoi_path,name)
            if(length(list.files(rtoi_path,pattern="\\.rtoi$"))>0){
              stop("This rtoi already exists, define other name or rtoi_path")
            }
            dir.create(rtoi_path,showWarnings = FALSE)
            newobj=new("rtoi")
            newobj$records<-new("records")
            newobj$name<-name

            newobj$region<-list(region)
            newobj$rtoi_path<-rtoi_path
            newobj$db_path<-db_path
            newobj$size<-0
            write_rtoi(newobj)
            return(newobj)
          })

#' @rdname new_rtoi
#' @aliases character,sf,character,character,records
setMethod("new_rtoi",
          signature(name = "character",
                    region = "sf",
                    rtoi_path = "character",
                    db_path = "character",
                    records = "records"),
          function(name, region, rtoi_path, db_path,records) {

            rtoi_path<-file.path(rtoi_path,name)
            if(length(list.files(rtoi_path,pattern="\\.rtoi$"))>0){
              stop("This rtoi already exists, define other name or rtoi_path")
            }
            newobj=new("rtoi")
            newobj$name<-name
            newobj$region<-list(region)
            newobj$records<-records
            newobj$rtoi_path<-rtoi_path
            newobj$db_path<-db_path
            newobj$size<-0
            write_rtoi(newobj)
            return(newobj)
          })


#' @rdname names-records-method
#' @aliases names,rtoi
#' @include records.R
setMethod("names",
          signature(x = "rtoi"),
          function(x){
            return(x$name)
          })

#' @rdname names-records-method
#' @aliases names<-,rtoi,character
setReplaceMethod(f="names",
                 signature=c("rtoi","character"),
                 definition=function(x, value) {
                   x$name<-value
                   return(x)
                 })

#' @rdname sat_name
#' @aliases sat_name,rtoi
setMethod(f="sat_name",
          signature =c("rtoi"),
          definition = function(x){
            return(list.dirs(get_dir(x),recursive = FALSE,full.names = FALSE))
          })

#' @rdname get_dir
#' @aliases get_dir,rtoi
setMethod("get_dir",
          signature(x = "rtoi"),
          function(x){
            return(x$rtoi_path)
          })

#' @rdname get_dir
#' @aliases get_dir,rtoi,character
setGeneric("get_dir<-",function(x,value)  standardGeneric("get_dir<-"))
setMethod("get_dir<-",
          signature(x = "rtoi",value="character"),
          function(x,value){
            x$rtoi_path<-value
            write_rtoi(x)
            return(x)
          })


setGeneric("get_var_dir",function(x,p)  standardGeneric("get_var_dir"))
setMethod("get_var_dir",
          signature(x = "rtoi"),
          function(x,p){
            dirs<-list.files(list.files(get_dir(x),full.names = TRUE),pattern=p,full.names = TRUE)
            return(file.path(dirs,"variables"))
          })

setMethod("get_mosaic_dir",
          signature=c(x = "rtoi"),
          function(x,p){
            dirs<-list.dirs(get_dir(x),full.names = TRUE,recursive=TRUE)
            dirs<-dirs[grepl(p,basename(dirs))]
            return(file.path(dirs,"mosaic"))
          })

#' Loads into R a time series of images regarding an rtoi, satellite product,
#' and remote sensing index.
#'
#' @param x an rtoi.
#' @param p a character with the name of the satellite data product.
#' @param v a character with the name of the index.
#' @param ... additional arguments.
#'
#' @return a raster stack.
#'
#' @import raster stars
#' @export
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
#' #Landsat-5
#' sat_search(region=navarre,
#'            product="LANDSAT_TM_C1",
#'            dates=as.Date("1988-08-01")+seq(1,35))
#' download(navarre)
#'
#' mosaic(navarre,overwrite=T)
#'
#' derive(navarre,"NDVI",product="LANDSAT_TM_C1")
#' ls6.ndvi<-get_raster(navarre,"LANDSAT_TM_C1","NDVI")
#' }
setGeneric("get_raster",function(x,p,v,...)  standardGeneric("get_raster"))

#' @rdname get_raster
#' @aliases get_raster,rtoi
setMethod("get_raster",
          signature(x = "rtoi"),
          function(x,p,v,...){
            dirs<-get_var_dir(x,p)
            files<-list.files(dirs,recursive = T,pattern = "\\.tif$",full.names = T)
            if(length(files)==0){
              message("There are no images for this product and variable")
            }
            return(stack(files))
          })

#' Loads into R a time series of images regarding an rtoi, satellite product,
#' and remote sensing index.
#'
#' @param x an rtoi.
#' @param p a character with the name of the satellite data product.
#' @param v a character with the name of the index.
#' @param ... additional arguments.
#'
#' @return a stars object.
#'
#' @export
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
#'                   db.path)#'
#' #Landsat-5
#' sat_search(region=navarre,
#'           product="LANDSAT_TM_C1",
#'           dates=as.Date("1988-08-101")+seq(1,35))
#' download(navarre)
#'
#' mosaic(navarre,overwrite=T)
#'
#' derive(navarre,"NDVI",product="LANDSAT_TM_C1")
#' ls6.ndvi<-get_stars(navarre,"LANDSAT_TM_C1","NDVI")
#' }
setGeneric("get_stars",function(x,p,v,...)  standardGeneric("get_stars"))

#' @rdname get_stars
#' @aliases get_stars,rtoi
setMethod("get_stars",
         signature(x = "rtoi"),
         function(x,p,v){
           dirs<-get_var_dir(x,p)
           dirs<-file.path(dirs,v)
           files<-list.files(dirs,recursive = T,pattern = "\\.tif$",full.names = T)
           if(length(files)==0){
             message("There are no images for this product and variable")
           }
           #files<-file.path("/vsizip",files)

           return(unique(product(records(x))))
         })

#' Extracts the path to the database
#'
#' extracts the path to the database from an rtoi.
#'
#' @param x an rtoi object.
#'
#' @export
setGeneric("get_database",function(x)  standardGeneric("get_database"))

#' @rdname get_database
#' @aliases get_database,rtoi
setMethod("get_database",
          signature(x = "rtoi"),
          function(x){
            return(x$db_path)
          })

#' #' @export
#' setGeneric("get_database<-",function(x,value)  standardGeneric("get_database<-"))
#' setMethod("get_database<-",
#'           signature(x = "rtoi",value="character"),
#'           function(x,value){
#'             x$db_path<-value
#'             write_rtoi(x)
#'             return(x)
#'           })

#' set database path
#'
#' set_database set a new database  directory in x.
#'
#' @param x rtoi object
#' @param value character argument. The value for change the database directory of x.
#'
#' @export
setGeneric("set_database",function(x,value)  standardGeneric("set_database"))

#' @rdname set_database
#' @aliases set_database,rtoi
setMethod("set_database",
          signature(x = "rtoi"),
          function(x,value){
            x$db_path<-value
            write_rtoi(x)
          })

#' Extracts region from an rtoi
#'
#' gets the sf that specifies the region of an rtoi.
#'
#' @param x an rtoi object.
#' @param value an sf object to define the region in x.
#'
#' @export
setGeneric("region", function(x) {standardGeneric("region")})
#' @rdname region
#' @aliases region,rtoi
setMethod("region",
          signature(x = "rtoi"),
          function(x){
            return(x$region[[1]])
          })

#' @export
#' @rdname region
#' @aliases region<-
setGeneric("region<-", function(x, value) standardGeneric("region<-"))
#' @rdname region
#' @aliases region<-,rtoi,sf
setMethod(f="region<-",
          signature=c(x = "rtoi",value = "sf"),
          definition=function(x, value) {
            x$region<-list(value)
            write_rtoi(x)
            x
          })

#' Extracts the satellite records
#'
#' returns the object records from an rtoi.
#'
#' @param x an rtoi object
#' @param value a records object to be set to x.
#'
#' @export
setGeneric("records", function(x) {standardGeneric("records")})
#' @rdname records
#' @aliases records,rtoi
setMethod("records",
          signature(x = "rtoi"),
          function(x){
            return(x$records)
          })

#' @export
#' @rdname records
#' @aliases records<-
setGeneric("records<-", function(x, value) standardGeneric("records<-"))
#' @rdname records
#' @aliases records<-,rtoi,records
setMethod(f="records<-",
          signature=c(x = "rtoi",value = "records"),
          definition=function(x, value) {
            x$records<-value
            write_rtoi(x)
            x
          })

# #' @export
#setGeneric("adddates", function(x, dates) {standardGeneric("adddates")})
#' @rdname dates
#' @aliases dates,rtoi
setMethod("dates",
          signature(x = "rtoi"),
          function(x){
            return(unique(dates(records(x))))
          })

#' @export
setGeneric("drop_records", function(x, product, y, ...) {standardGeneric("drop_records")})
setMethod("drop_records",
          signature(x = "rtoi", product="character", y = "Date"),
          function(x,product,y){
            rcds<-records(x)
            rcds<-subset(rcds, subset=product, select="product")
            rcds<-subset(rcds, subset=y, select="date")
            records(x)<-records(x)[!names(records(x))%in%names(rcds)]
            write_rtoi(x)
            #TODO remove mosaic and data for that date
          })

setMethod("drop_records",
          signature(x = "rtoi", product="character", y = "ANY"),
          function(x, product, y, select){
            rcds<-records(x)
            rcds<-subset(rcds, subset=product, select="product")
            rcds<-subset(rcds, subset=y, select=select)
            records(x)<-records(x)[!names(records(x))%in%names(rcds)]
            write_rtoi(x)
            #TODO remove mosaic and data for that date
          })

#' @export
setGeneric("add_records", function(x, y) {standardGeneric("add_records")})
setMethod("add_records",
          signature(x = "rtoi", y = "records"),
          function(x,y){
            records(x)<-c(records(x),y)
            write_rtoi(x)
          })

#' @rdname product
#' @aliases product,rtoi
setMethod("product",
          signature(x = "rtoi"),
          function(x){
            return(unique(product(records(x))))
          })


#' @export
setGeneric("rename", function(x, newname) { standardGeneric("rename")})
setMethod("rename",
          signature(x = "rtoi", newname="character"),
          function(x,newname){

            new.dir<-file.path(dirname(get_dir(x)),newname)
            file.rename(get_rtoi_path(x),file.path(get_dir(x),paste0(newname,".rtoi")))
            names(x)<-newname
            file.rename(get_dir(x),new.dir)
            get_dir(x)<-new.dir
            write_rtoi(x)
          })



setGeneric("rtoi_size_cal", function(x) { standardGeneric("rtoi_size_cal")})
setMethod("rtoi_size_cal",
          signature(x = "rtoi"),
          function(x){
            x$size<-round(((sum(file.info(list.files(get_dir(x), all.files = TRUE, recursive = TRUE,full.names = TRUE))$size)/1024)/1024)/1024,2)
            write_rtoi(x)
          })

setGeneric("rtoi_size", function(x) standardGeneric("rtoi_size"))
setMethod(f="rtoi_size",
          signature=c(x = "rtoi"),
          definition=function(x) {
            x$size
          })

#' Prints the values
#'
#' prints an object and returns it invisibly (via invisible(x)).
#'
#' @param x an object to be printed..
#' @param ... additional arguments.
#'
#' @examples
#' \dontrun{
#' data(ex.navarre)
#' # path where the data will be
#' rtoi.path <- tempdir()
#' # path where downloads are stored
#' db.path <- file.path(tempdir(),"DATABASE")
#' navarre<-new_rtoi("Navarre",
#'                   ex.navarre,
#'                   rtoi.path,
#'                   db.path)
#' print(navarre)
#' }
setMethod("print",
          signature(x = "rtoi"),
          function(x){
            cat(paste0("Name: ",names(x),"\n"))
            cat(paste0(" -N. records: ",length(records(x)),"\n"))
            cat(paste0(" -Products: ",paste0(product(x),collapse = ", "),"\n"))
            cat(paste0(" -Satellites: ",paste(unique(sat_name(records(x))),collapse = ", "),"\n"))
            cat(paste0(" -Dir size: ",rtoi_size(x),"GB\n"))
            d<-dates(records(x))
            if(length(records(x))==0){
              d<-NA
            }
            cat(paste0(" -Dates: from ",min(d)," to ",max(d),"\n"))
            cat(paste0(" -Database dir: ",get_database(x),"\n"))
            cat(paste0(" -rtoi dir: ",get_dir(x),"\n"))
          })

#' @rdname show-records-method
#' @aliases show,rtoi
setMethod("show",
          signature= c("rtoi"),
          function(object){
            print(object)
          })

setGeneric("get_rtoi_path",function(x){standardGeneric("get_rtoi_path")})
setMethod("get_rtoi_path",
          signature= c("rtoi"),
          function(x){
            file.path(get_dir(x),paste0(names(x),".rtoi"))
          })

setGeneric("write_rtoi",function(x){standardGeneric("write_rtoi")})
setMethod("write_rtoi",
          signature= c("rtoi"),
          function(x){
            saveRDS(x, file=get_rtoi_path(x))
          })

#' Reads an rtoi from the hard drive
#'
#' @param path an rtoi object.
#' @param ... additional arguments.
#'
#' @export
#' @examples
#' \dontrun{
#' #' data(ex.navarre)
#' # path where the data will be
#' rtoi.path <- tempdir()
#' # path where downloads are stored
#' db.path <- file.path(tempdir(),"DATABASE")
#' navarre<-new_rtoi("Navarre",
#'                   ex.navarre,
#'                   rtoi.path,
#'                   db.path)
#' rm(navarre)
#'
#' rtoi.path <- file.path(tempdir(),"Navarre")
#' new.navarre <- read_rtoi(rtoi.path)
#' print(new.navarre)
#' }
setGeneric("read_rtoi",function(path,...){standardGeneric("read_rtoi")})
#' @rdname read_rtoi
#' @aliases read_rtoi,character
setMethod("read_rtoi",
          signature= c("character"),
          function(path,...){
            files<-list.files(path,pattern = "\\.rtoi$",full.names = TRUE,...)
            if(length(files)>1){warning("More than one rtoi found! loading the first.")}
            if(length(files)==0)stop("There is no rtoi object in this path.")
            return(readRDS(file=files[1]))
          })
