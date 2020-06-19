#' Region Time Of Interest (rtoi)
#'
#' It is a proxy object for managing an spatial region in large time periods
#' of time and with data from multiple satellites. The rtoi stores all the metadata
#' of a region of interest to process the region.
#'
#' An \code{rtoi} object manages two main folders, the database and the rtoi folder.
#' When \code{download} function is called with an rtoi, all the images will be
#' stored in the database.
#' The rtoi folder includes a \code{region.rtoi} file with all the metadata. Once
#' the \code{mosaic} function is run, multiple folders with the name of the satellites
#' considered in the rtoi apears. Each folder contais a preproceses version of
#' each satellite.
#'
#' @field name character field. Takes the name of the region of interest
#' @field rtoi_path character field. The path where the rtoi will be stored.
#' @field region sf field. An sf with the region of interest
#' @field records records field. The records that presents the images for your
#' region and time of interest.
#' @field db_path character field. The directory where the original version
#' of the images are stored.
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
#' navarre<-new_rtoi("Navarre",
#'                   ex.navarre,
#'                   rtoi.path,
#'                   db.path)
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
              db_path = "character"
            )
)

#' Creates a new rtoi object
#'
#' @param name the name of the region of interes
#' @param sfobj sf object.
#' @param records records object.
#' @param db_path the directory where the database for creating the rtoi will be located.
#' @param rtoi_path the directory where the rtoi will be located.
#'
#' @return the reference of the rtoi object
#' @exportMethod new_rtoi
setGeneric("new_rtoi", function(name, sfobj, rtoi_path, db_path, records) {
  standardGeneric("new_rtoi")
})

#' @rdname new_rtoi
#' @aliases new_rtoi,character,sf,character,character,missing
setMethod("new_rtoi",
          signature(name = "character",
                    sfobj = "sf",
                    rtoi_path = "character",
                    db_path = "character",
                    records = "missing"),
          function(name, sfobj, rtoi_path, db_path) {
            rtoi_path<-file.path(rtoi_path,name)
            if(length(list.files(rtoi_path,pattern="\\.rtoi$"))>0){
              stop("This rtoi already exists, define other name or rtoi_path")
            }
            dir.create(rtoi_path,showWarnings = FALSE)
            newobj=new("rtoi")
            newobj$records<-new("records")
            newobj$name<-name

            newobj$region<-list(sfobj)
            newobj$rtoi_path<-rtoi_path
            newobj$db_path<-db_path
            write_rtoi(newobj)
            return(newobj)
          })

#' @rdname new_rtoi
#' @aliases character,sf,character,character,records
setMethod("new_rtoi",
          signature(name = "character",
                    sfobj = "sf",
                    rtoi_path = "character",
                    db_path = "character",
                    records = "records"),
          function(name, sfobj, rtoi_path, db_path,records) {

            rtoi_path<-file.path(rtoi_path,name)
            if(length(list.files(rtoi_path,pattern="\\.rtoi$"))>0){
              stop("This rtoi already exists, define other name or rtoi_path")
            }
            newobj=new("rtoi")
            newobj$name<-name
            newobj$region<-list(sfobj)
            newobj$records<-records
            newobj$rtoi_path<-rtoi_path
            newobj$db_path<-db_path
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

#' get the raster of a variable from rtoi object
#'
#' @param x the target rtoi
#'
#' @param p character argument. The product name to be extracted.
#' @param v character argument. The derived variable to be extracted.
#' @param ... additional arguments.
#'
#' @return a raster stack with the variable and product defined by the user.
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
#'                   db.path)#'
#' #Landsat-5
#' sat_search(region=navarre,
#'           product="LANDSAT_TM_C1",
#'           dates=as.Date("1988-08-01")+seq(1,35))
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

#' get the raster of a variable from rtoi object
#'
#' @param x the target rtoi
#'
#' @param p character argument. The product name to be extracted.
#' @param v character argument. The derived variable to be extracted.
#' @param ... additional arguments.
#'
#' @return a star object with the variable and product defined by the user.
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

#' Extract database path
#'
#' get_database returns a character with the database of an rtoi.
#'
#' @param x rtoi object
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

#' Extract region
#'
#' region returns an sf with the region of an rtoi.
#'
#' @param x rtoi object
#' @param value sf argument. The value for change the region of x.
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

#' Extract records
#'
#' records returns an records object from an rtoi.
#'
#' @param x rtoi object
#' @param value records argument. The value for change the records of x.
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
            return(dates(records(x)))
          })

# #' @export
#setGeneric("dropdates", function(x, dates) {standardGeneric("dropdates")})

#' @rdname product
#' @aliases product,rtoi
setMethod("product",
          signature(x = "rtoi"),
          function(x){
            return(unique(product(records(x))))
          })


# #' @export
#setGeneric("rename", function(x, newname) { standardGeneric("rename")})
#TODO
# setMethod("rename",
#           signature(x = "rtoi", newname="character"),
#           function(x,newname){
#             x$name<-
#             x$dates<-c(x$dates,dates)
#           })


#' Print Values
#'
#' print prints its argument and returns it invisibly (via invisible(x)). It is a generic function which means that new printing methods can be easily added for new classes.
#'
#' @param x print object.
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
            cat(paste0(" -Dir size: ",round(((sum(file.info(list.files(get_dir(x), all.files = TRUE, recursive = TRUE,full.names = TRUE))$size)/1024)/1024)/1024,2),"GB\n"))
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


setGeneric("write_rtoi",function(x){standardGeneric("write_rtoi")})
setMethod("write_rtoi",
          signature= c("rtoi"),
          function(x){
            saveRDS(x, file=file.path(get_dir(x),paste0(names(x),".rtoi")))
          })

#' Reads an rtoi from hard drive
#'
#' @param path rtoi object.
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
