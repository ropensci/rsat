#' Region and Time Of Interest (\code{rtoi})
#'
#' It is a proxy object to store metadata about satellite imagery
#' covering a spatial region over a time period. Images can come from
#' multiple missions/programs and its purpose is to help managing
#' heterogeneous datasets.
#'
#' An \code{rtoi} object manages two main folders called database and rtoi.
#' The database is meant to work as a local, generic, and organized archive
#' of raw satellite data retrieved with the \code{download()} function.
#' The rtoi folder contains processed information for
#' a particular region and time of interest. When \code{mosaic()}
#' is called, the function crops and mosaics the relevant raw images from
#' the database and saves the results in the rtoi folder. This folder also
#' contains a \code{region.rtoi} file which saves metadata about the
#' region/time of interest and satellite imagery available.
#'
#' @field name a character with the name of the region of interest.
#' @field rtoi_path a character with the path to the rtoi folder.
#' @field region an sf with the region of interest.
#' @field records the satellite records available for
#' your region and time of interest.
#' @field db_path a character with the path to the database.
#'
#' @exportClass rtoi
#' @include records.R
#'
#' @examples
#' data(ex.navarre)
#' ## Create an rtoi with database
#' # path where the region is stored
#' rtoi.path <- tempdir()
#'
#' # path where downloads are stored
#' db.path <- file.path(tempdir(), "DATABASE")
#' navarre <- new_rtoi(
#'   name = "Navarre_rtoi",
#'   region = ex.navarre,
#'   rtoi_path = rtoi.path,
#'   db_path = db.path
#' )
#'
#' print(navarre)
#'
#' ## Create an rtoi without database
#' navarre2 <- new_rtoi(
#'   name = "Navarre_rtoi2",
#'   region = ex.navarre,
#'   rtoi_path = rtoi.path
#' )
#'
#' print(navarre2)
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

#' Creates a new \code{rtoi} object
#'
#' @param name the name of the region of interest.
#' @param region an sf object.
#' @param records a records object.
#' @param db_path the path to the database.
#' @param rtoi_path the path to the \code{rtoi} folder.
#' @param size the size of \code{rtoi} folder. By default,
#' the size is computed from \code{rtoi_path}.
#'
#' @return the reference of the \code{rtoi} object
#' @exportMethod new_rtoi
setGeneric("new_rtoi", function(name,
                                region,
                                rtoi_path,
                                db_path,
                                records,
                                size) {
  standardGeneric("new_rtoi")
})

#' @rdname new_rtoi
#' @aliases new_rtoi,character,sf,character,missing,missing
setMethod(
  "new_rtoi",
  signature(
    name = "character",
    region = "sf",
    rtoi_path = "character",
    db_path = "missing",
    records = "missing",
    size = "missing"
  ),
  function(name, region, rtoi_path) {
    rtoi_path <- file.path(rtoi_path, name)
    if (length(list.files(rtoi_path, pattern = "\\.rtoi$")) > 0) {
      stop(paste0("Trying to write in: ",rtoi_path,". This rtoi already exists. Give it a new name or rtoi_path."))
    }
    dir.create(rtoi_path, showWarnings = FALSE)
    newobj <- new("rtoi")
    newobj$records <- new("records")
    newobj$name <- name
    newobj$region <- list(region)
    newobj$rtoi_path <- rtoi_path
    newobj$db_path <- ""
    newobj$size <- 0
    write_rtoi(newobj)
    return(newobj)
  }
)

#' @rdname new_rtoi
#' @aliases character,sf,character,character
setMethod(
  "new_rtoi",
  signature(
    name = "character",
    region = "sf",
    rtoi_path = "character",
    db_path = "character",
    records = "missing",
    size = "missing"
  ),
  function(name, region, rtoi_path, db_path) {
    newobj <- new_rtoi(name, region, rtoi_path)
    newobj$db_path <- db_path
    write_rtoi(newobj)
    return(newobj)
  }
)


#' @rdname new_rtoi
#' @aliases character,sf,character,character,records
setMethod(
  "new_rtoi",
  signature(
    name = "character",
    region = "sf",
    rtoi_path = "character",
    db_path = "character",
    records = "records",
    size = "missing"
  ),
  function(name, region, rtoi_path, db_path, records) {
    newobj <- new_rtoi(name, region, rtoi_path, db_path, records = records)
    newobj$size <- 0
    write_rtoi(newobj)
    return(newobj)
  }
)

#' @rdname new_rtoi
#' @aliases character,sf,character,character,records,size
setMethod(
  "new_rtoi",
  signature(
    name = "character",
    region = "sf",
    rtoi_path = "character",
    db_path = "character",
    records = "records",
    size = "numeric"
  ),
  function(name, region, rtoi_path, db_path, records, size) {
    newobj <- new_rtoi(name, region, rtoi_path, db_path, records = records)
    newobj$size <- size
    write_rtoi(newobj)
    return(newobj)
  }
)

#' @rdname names
#' @aliases names,rtoi
#' @include records.R
setMethod(
  "names",
  signature(x = "rtoi"),
  function(x) {
    return(x$name)
  }
)

#' @rdname names
#' @aliases names<-,rtoi,character
setReplaceMethod(
  f = "names",
  signature = c("rtoi", "character"),
  definition = function(x, value) {
    x$name <- value
    return(x)
  }
)

#' @rdname sat_name
#' @aliases sat_name,rtoi
setMethod(
  f = "sat_name",
  signature = c("rtoi"),
  definition = function(x) {
    return(unique(sat_name(records(x))))
  }
)

#' @param x .
#'
#' @rdname get_dir
#' @aliases get_dir,rtoi
setMethod(
  "get_dir",
  signature(x = "rtoi"),
  function(x) {
    return(x$rtoi_path)
  }
)


setGeneric("get_dir<-", function(x, value) standardGeneric("get_dir<-"))
setMethod(
  "get_dir<-",
  signature(x = "rtoi", value = "character"),
  function(x, value) {
    x$rtoi_path <- value
    write_rtoi(x)
    return(x)
  }
)

setGeneric("get_var_dir", function(x, p) standardGeneric("get_var_dir"))
setMethod(
  "get_var_dir",
  signature(x = "rtoi"),
  function(x, p) {
    dirs <- list.files(list.files(get_dir(x), full.names = TRUE),
                       pattern = p, full.names = TRUE)
    return(file.path(dirs, "variables"))
  }
)

setMethod("get_mosaic_dir",
  signature = c(x = "rtoi"),
  function(x, p) {
    dirs <- list.dirs(get_dir(x), full.names = TRUE, recursive = TRUE)
    dirs <- dirs[grepl(p, basename(dirs))]
    return(file.path(dirs, "mosaic"))
  }
)

#' Loads into R a time series of images regarding an rtoi, satellite product,
#' and remote sensing index.
#'
#' @param x an rtoi.
#' @param p a character with the name of the satellite data product.
#' @param v a character with the name of the index.
#' @param s a character with the name of the stage wanted.
#' @param ... additional arguments.
#'
#' @return a raster stack.
#' @rdname extract_data
#' @export
#' @examples
#' \dontrun{
#' library(rsat)
#' # load example rtoi
#' file.copy(from=system.file("ex/PamplonaDerived",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#'
#' # load example rtoi
#' pamplona.derived <- read_rtoi(file.path(tempdir(),"PamplonaDerived"))
#'
#' # print available variables
#' rsat_list_data(pamplona.derived)
#'
#' # get RasterStack from raster package
#' suppressWarnings(mod.ndvi.raster <-
#'            rsat_get_raster(pamplona.derived, "mod09ga", "NDVI"))
#' plot(mod.ndvi.raster)
#'
#' # get spatraster from terra package
#' mod.ndvi.rast <- rsat_get_SpatRaster(pamplona.derived, "mod09ga", "NDVI")
#' plot(mod.ndvi.rast)
#'
#' # get stars from stars package
#' suppressWarnings(mod.ndvi.stars <-
#' rsat_get_stars(pamplona.derived, "mod09ga", "NDVI"))
#' plot(mod.ndvi.stars)
#'
#'
#' ## get any band in rtoi
#' # list available data
#' rsat_list_data(pamplona.derived)
#' # select band 1: MODIS_Grid_500m_2D_sur_refl_b01_1
#' mod.ndvi.rast <- rsat_get_SpatRaster(pamplona.derived,
#'                                      "mod09ga",
#'                                      "MODIS_Grid_500m_2D_sur_refl_b01_1")
#' plot(mod.ndvi.rast)
#' }
setGeneric("rsat_get_raster", function(x, p, v, s, ...) standardGeneric("rsat_get_raster"))

#' @rdname extract_data
#' @aliases rsat_get_raster,rtoi
setMethod(
  "rsat_get_raster",
  signature(x = "rtoi"),
  function(x, p, v, s, ...){
    files<-get_processed_files(x, p, v, s, ...)
    return(stack(files))
  }
)

#' @rdname extract_data
#' @export
setGeneric("rsat_get_SpatRaster", function(x, p, v, s, ...) standardGeneric("rsat_get_SpatRaster"))
#' @rdname extract_data
#' @aliases rsat_get_SpatRaster,rtoi
setMethod(
  "rsat_get_SpatRaster",
  signature(x = "rtoi"),
  function(x, p, v, s, ...){
    files<-get_processed_files(x, p, v, s, ...)

    spatras<- rast(files)
    if((!missing(s))&&s=="mosaic"){
      names(spatras)<-paste0(gsub(".tif","",basename(files),fixed =TRUE),"_",
                format(genGetDates(files),"%Y%j"))
    }
    return(spatras)
  }
)

#' @rdname extract_data
#' @export
setGeneric("rsat_get_stars", function(x, p, v, s, ...) standardGeneric("rsat_get_stars"))
#' @rdname extract_data
#' @aliases rsat_get_stars,rtoi
setMethod(
  "rsat_get_stars",
  signature(x = "rtoi"),
  function(x, p, v, ...){
    files<-get_processed_files(x, p, v, s, ...)
    return(st_as_stars(stack(files)))
  }
)

get_processed_files<-  function(x, p, v, s, ...) {
  # layers<-file.path("/vsizip",i,utils::unzip(i,list=T)$Name)
  dirs <- list.files(list.files(get_dir(x), full.names = TRUE),
                     pattern = p, full.names = TRUE)
  files <- list.files(dirs, recursive = TRUE, pattern = "\\.zip$",
                      full.names = TRUE)
  files <- files[grepl(paste0("/",v,"\\.zip"), files)]
  if(missing(s)){
    files <- files[grepl("variables", files)]
  }else{
    files <- files[grepl(s, files)]
  }
  if (length(files) == 0) {
    p.df <- rsat_list_data(x)
    p.df <- p.df[p.df$product == p, ]
    p.df <- p.df[p.df$variable == v, ]

    if (grepl("CloudMask", v)) {
      files <- paste0(paste0(c(get_dir(x),
                               unlist(p.df[1:3])),
                             collapse = "/"), v, ".zip")
      mos.zip <- file.path("/vsizip",
                           files,
                           utils::unzip(files, list = TRUE)$Name)
      return(stack(mos.zip))
    }

    dirs <- paste0(c(get_dir(x), unlist(p.df[1:3])), collapse = "/")
    files <- list.files(dirs,
                        recursive = TRUE,
                        pattern = "\\.zip$",
                        full.names = TRUE)

    mos.zip <- c()
    for (f in files) {
      bnds <- utils::unzip(f, list = TRUE)$Name
      bnds <- bnds[grepl(v, bnds)]
      if (length(bnds) == 1) {
        mos.zip <- c(mos.zip, file.path("/vsizip", f, bnds))
      }
    }
    if (length(mos.zip) == 0)
      message("There are no images for this product and variable.")
  } else {
    if (length(files) == 1) {
      mos.zip <- file.path("/vsizip",
                           files, utils::unzip(files, list = TRUE)$Name)
    }
  }
  return(mos.zip)
}

#' Extracts or assign the path of the database
#'
#' Extracts the path to the database from an rtoi/package environment.
#' If both, environment and rtoi database are defined the rtoi
#' database is used.
#'
#' @param x an rtoi object.
#' @param value character argument. The value for
#' change the database directory of x.
#' @param ... additional arguments.
#' @return the database path of an rtoi
#' @export
#' @rdname get-set_database
#' @examples
#' # load example rtoi
#' file.copy(from=system.file("ex/Navarre",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#'
#' navarre <- read_rtoi(file.path(tempdir(),"Navarre"))
#'
#' # get the databse used by navarre
#' get_database(navarre)
#'
#' # set the a new database path
#' set_database(navarre,"new_path")
#'
#' # get the database used by rsat by default
#' get_database()
#'
#' # set the a new database path for the entire environment
#' set_database("new_path")
setGeneric("get_database", function(x) standardGeneric("get_database"))

#' @rdname get-set_database
#' @aliases get_database,rtoi
setMethod(
  "get_database",
  signature(x = "rtoi"),
  function(x) {
    if(x$db_path=="")
      return(get_database())
    else
      return(x$db_path)
  }
)

#' @rdname get-set_database
setMethod(
  "get_database",
  signature = c(x = "missing"),
  function() {
    return(getRSATOpt("RSAT_DATABASE"))
  }
)

#' @export
#' @rdname get-set_database
setGeneric("set_database", function(x, ...) standardGeneric("set_database"))

#' @rdname get-set_database
setMethod(
  "set_database",
  signature(x = "rtoi"),
  function(x, value) {
    x$db_path <- value
    write_rtoi(x)
  }
)

#' @rdname get-set_database
setMethod(
  "set_database",
  signature(x = "character"),
  function(x) {
    setRSATOpt("RSAT_DATABASE",x)
  }
)

#' Extracts region from an rtoi
#'
#' gets the sf that specifies the region of an rtoi.
#'
#' @param x an rtoi object.
#' @param value an sf object to define the region in x.
#' @return the sf class with the region of an rtoi
#' @export
#' @examples
#' library(rsat)
#' # create a copy of navarre
#' file.copy(from=system.file("ex/Navarre",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#'
#' # load example rtoi
#' navarre <- read_rtoi(file.path(tempdir(),"Navarre"))
#'
#' # get the region from rtoi
#' sf.obj <-  region(navarre)
#' plot(sf.obj)
#'
#' # asign new region value
#' region(navarre)<-NULL
#'
#' region(navarre)<-sf.obj
setGeneric("region", function(x) {
  standardGeneric("region")
})
#' @rdname region
#' @aliases region,rtoi
setMethod(
  "region",
  signature(x = "rtoi"),
  function(x) {
    return(x$region[[1]])
  }
)

#' @export
#' @rdname region
#' @aliases region<-,rtoi
setGeneric("region<-", function(x, value) standardGeneric("region<-"))
#' @rdname region
#' @aliases region<-,rtoi,sf
setMethod(
  f = "region<-",
  signature = c(x = "rtoi", value = "sf"),
  definition = function(x, value) {
    x$region <- list(value)
    write_rtoi(x)
    x
  }
)
#' @rdname region
#' @aliases region<-,rtoi,NULL
setMethod(
  f = "region<-",
  signature = c(x = "rtoi", value = "NULL"),
  definition = function(x, value) {
    x$region <- list(value)
    write_rtoi(x)
    x
  }
)

#' Extracts the satellite records
#'
#' returns the object records from an rtoi.
#'
#' @param x an rtoi object
#' @param value a records object to be set to x.
#' @return a set of records in x rtoi
#' @export
#' @examples
#' #' library(rsat)
#' # create a copy of navarre
#' file.copy(from=system.file("ex/Navarre",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#' # load example rtoi
#' navarre <- read_rtoi(file.path(tempdir(),"Navarre"))
#' print(navarre)
#'
#' rcrds <- records(navarre)
#'
#' records(navarre)<-rcrds[1]
#' print(navarre)
#'
#' records(navarre) <- rcrds
#' print(navarre)
#' unlink(file.path(tempdir(),"Navarre"),recursive=TRUE)
setGeneric("records", function(x) {
  standardGeneric("records")
})
#' @rdname records
#' @aliases records,rtoi
setMethod(
  "records",
  signature(x = "rtoi"),
  function(x) {
    return(x$records)
  }
)

#' @export
#' @rdname records
#' @aliases records<-
setGeneric("records<-", function(x, value) standardGeneric("records<-"))

#' @rdname records
#' @aliases records<-,rtoi,records
setMethod(
  f = "records<-",
  signature = c(x = "rtoi", value = "records"),
  definition = function(x, value) {
    x$records <- value
    write_rtoi(x)
    x
  }
)

#' @rdname dates
setMethod(
  "dates",
  signature(x = "rtoi"),
  function(x) {
    return(unique(dates(records(x))))
  }
)


#' @rdname product
#' @aliases product,rtoi
setMethod(
  "product",
  signature(x = "rtoi"),
  function(x) {
    return(unique(product(records(x))))
  }
)


#' Renames an \code{rtoi}
#'
#' Renames all parameters and folder name of an \code{rtoi}.
#'
#' @param x an rtoi object
#' @param newname a character class to rename the \code{rtoi}.
#' @return nothing. the  changes the internal name of the rtoi
#' @export
#' @examples
#' \dontrun{
#' myrtoi <- read_rtoi("file_path/rtoir_name")
#' rename(myrtoi, "Navarre_BACK")
#' }
setGeneric("rename", function(x, newname) {
  standardGeneric("rename")
})

#' @rdname rename
#' @aliases rename,rtoi,character
setMethod(
  "rename",
  signature(x = "rtoi", newname = "character"),
  function(x, newname) {
    new.dir <- file.path(dirname(get_dir(x)), newname)
    file.rename(get_rtoi_path(x), file.path(get_dir(x),
                                            paste0(newname, ".rtoi")))
    names(x) <- newname
    file.rename(get_dir(x), new.dir)
    get_dir(x) <- new.dir
    write_rtoi(x)
  }
)



setGeneric("rtoi_size_cal", function(x) {
  standardGeneric("rtoi_size_cal")
})
setMethod(
  "rtoi_size_cal",
  signature(x = "rtoi"),
  function(x) {
    x$size <- round(((sum(file.info(list.files(get_dir(x),
                                               all.files = TRUE,
                                               recursive = TRUE,
                                               full.names = TRUE))$size)
                      / 1024) / 1024) / 1024, 2)
    write_rtoi(x)
  }
)

setGeneric("rtoi_size", function(x) standardGeneric("rtoi_size"))
setMethod(
  f = "rtoi_size",
  signature = c(x = "rtoi"),
  definition = function(x) {
    x$size
  }
)

#' Prints the values
#'
#' prints an object and returns it invisibly (via invisible(x)).
#'
#' @param x an object to be printed..
#' @param ... additional arguments.
#' @return prints rtoi metadata
#' @examples
#' \dontrun{
#' library(rsat)
#'
#' # load example rtoi
#' file.copy(from=system.file("ex/Navarre",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#'
#' navarre <- read_rtoi(file.path(tempdir(),"Navarre"))
#'
#' print(navarre)
#'
#' # get records
#' rcrds <- records(navarre)
#'
#' print(rcrds)
#' }
#' @rdname print
setMethod(
  "print",
  signature(x = "rtoi"),
  function(x) {
    cat(paste0("Name: ", names(x), "\n"))
    cat(paste0(" -N. records: ", length(records(x)), "\n"))
    cat(paste0(" -Products: ", paste0(product(x), collapse = ", "), "\n"))
    cat(paste0(" -Satellites: ", paste(sat_name(x), collapse = ", "), "\n"))
    cat(paste0(" -Dir size: ", rtoi_size(x), "GB\n"))
    d <- dates(records(x))
    if (length(records(x)) == 0) {
      d <- NA
    }
    cat(paste0(" -Dates: from ", min(d), " to ", max(d), "\n"))
    cat(paste0(" -Database dir: ", get_database(x), "\n"))
    cat(paste0(" -rtoi dir: ", get_dir(x), "\n"))
  }
)

#' @rdname show
#' @aliases show,rtoi
setMethod("show",
  signature = c("rtoi"),
  function(object) {
    print(object)
  }
)

setGeneric("get_rtoi_path", function(x) {
  standardGeneric("get_rtoi_path")
})
setMethod("get_rtoi_path",
  signature = c("rtoi"),
  function(x) {
    file.path(get_dir(x), paste0(names(x), ".rtoi"))
  }
)

#' @importFrom sf st_write
setGeneric("write_rtoi", function(x, ...) {
  standardGeneric("write_rtoi")
})
setMethod("write_rtoi",
  signature = c("rtoi"),
  function(x, ...) {
    unlink(get_rtoi_path(x))
    args <- list(...)
    if (is.null(args$overwrite)) args$overwrite <- TRUE
    rtoi.names <- names(x$getRefClass()$fields())
    rtoi.names <- rtoi.names[!rtoi.names %in% c("records", "region")]
    # slots
    for (param in rtoi.names) {
      cat(paste0(param, ":", x$field(param)),
          file = get_rtoi_path(x), sep = "\n", append = TRUE)
    }

    # records
    cat("Records:", file = get_rtoi_path(x), sep = "\n", append = TRUE)
    df <- as.data.frame(records(x))
    cat(paste0(names(df), collapse = ","),
        file = get_rtoi_path(x), sep = "\n", append = TRUE)

    df$date <- as.character(dates(records(x)))
    for (i in seq_len(nrow(df))) {
      cat(paste0(df[i, ], collapse = ","),
          file = get_rtoi_path(x), sep = "\n", append = TRUE)
    }

    # sf
    #dir.create(file.path(get_dir(x), "region"), showWarnings = FALSE)
    #st_write(x$region[[1]], dsn = file.path(get_dir(x), "region"),
    #         driver = "ESRI Shapefile",
    #         quiet = TRUE, append = !args$overwrite)
    if(!is.null(region(x))){
      st_write(x$region[[1]],dsn=file.path(get_dir(x), "region"),
               driver="GeoJSON",
               quiet=TRUE,
               delete_dsn=TRUE)
    }else{
      file.remove(file.path(get_dir(x), "region"))
    }
   }
)

#' Reads an rtoi from the hard drive
#'
#' @param path an rtoi object.
#' @param ... additional arguments.
#' @return rtoi object readed from disk.
#' @export
#' @examples
#' library(rsat)
#'
#' # load example rtoi
#' file.copy(from=system.file("ex/Navarre",package="rsat"),
#'          to=tempdir(),
#'          recursive = TRUE)
#'
#' navarre <- read_rtoi(file.path(tempdir(),"Navarre"))
#' print(navarre)
setGeneric("read_rtoi", function(path, ...) {
  standardGeneric("read_rtoi")
})
#' @importFrom sf st_read read_sf
#' @rdname read_rtoi
#' @aliases read_rtoi,character
setMethod("read_rtoi",
  signature = c("character"),
  function(path, ...) {
    files <- list.files(path, pattern = "\\.rtoi$", full.names = TRUE, ...)
    if (length(files) > 1) {
      warning("More than one rtoi found! loading the first one.")
    }
    if (length(files) == 0) stop("There is no rtoi in this path.")
    newobj <- new("rtoi")
    # aux<-readRDS(file=files[1])
    lines <- readLines(files[1])

    # fields
    rtoi.name <- gsub("name:", "", lines[grepl("name:", lines)])
    if (length(rtoi.name) != 0) {
      newobj$name <- rtoi.name
    }

    rtoi.dir <- gsub("rtoi_path:", "", lines[grepl("rtoi_path:", lines)])
    if (path != rtoi.dir) {
      rtoi.dir <- path
    }
    if (length(rtoi.dir) != 0) {
      newobj$rtoi_path <- rtoi.dir
    }

    db_path <- gsub("db_path:", "", lines[grepl("db_path:", lines)])
    if (db_path != "") {
      newobj$db_path <- db_path
    }else{
      newobj$db_path<-""
    }

    size <- as.numeric(gsub("size:", "", lines[grepl("size:", lines)]))
    if (length(size) != 0) {
      newobj$size <- size
    }
    # sf
    poly.path <- file.path(path, "region")
    if(!file.exists(poly.path)){
      region<-NULL
    }else{
      if(identical(list.files(poly.path),character(0))){
        region <- read_sf(poly.path, quiet = TRUE,drivers="GeoJSON")
      }else{
        region <- st_read(poly.path, quiet = TRUE)
        unlink(poly.path,recursive =TRUE)
        st_write(region,
                 dsn=poly.path,
                 driver="GeoJSON",
                 quiet=TRUE,
                 delete_dsn=TRUE)
      }
    }
    newobj$region <- list(region)

    # records
    rcds <- lines[(which(grepl("Records:", lines)) + 1):length(lines)]
    rcds <- strsplit(rcds, ",")
    if (length(rcds) > 1) {
      df.rcds <- as.data.frame(do.call(rbind, rcds[-1]))
      names(df.rcds) <- rcds[[1]]
      records(newobj) <- as.records(df.rcds)
    } else {
      records(newobj) <- new("records")
    }

    return(newobj)
  }
)
