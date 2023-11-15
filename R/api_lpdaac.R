#' @importFrom curl curl new_handle handle_setopt curl_download
#' @importFrom curl curl_fetch_memory handle_setform
#' @importFrom httr POST content_type content authenticate
#' @importFrom httr add_headers content_type
#' @importFrom xml2 read_html xml_attr
#' @importFrom rvest html_nodes
#' @importFrom methods new
#' @importFrom XML xmlToList isXMLString
setGeneric("api_lpdaac",
           function(api_name, request, ...) {
             standardGeneric("api_lpdaac")
           })
setRefClass(
  Class = "api_lpdaac",
  fields = list(
    api_name = "character",
    server = "character",
    api_server = "character",
    data_server = "character",
    username = "character",
    password = "character",
    credentials = "character",
    status = "character",
    order.list = "list"
  ),
  methods = list(
    initialize = function() {
      .self$username <- ""
      .self$password <- ""
      .self$server <- "https://www.earthdata.nasa.gov/"
      .self$api_server <- "https://cmr.earthdata.nasa.gov"
      .self$data_server <- "https://data.lpdaac.earthdatacloud.nasa.gov"
      .self$credentials <- "earthdata"
      .self$status <- "Not checked"
      .self$order.list <- list("order" = c(), "status" = c(), "id" = c())
    },
    ######################################################################
    # Generics
    #####################################################################
    getServer = function() {
      return(.self$api_server)
    },
    checkCredentials = function() {
      if (length(.self$username) == 0) {
        stop("Username must be defined.")
      }
      if (length(.self$password) == 0) {
        stop("Password must be defined.")
      }
    },
    getCredentials = function() {
      return(c(username = .self$username, password = .self$password))
    },
    #####################################################################
    # Platform specific
    #####################################################################
    data_handle = function() {
      c.handle <- new_handle()
      if (.self$username == "" | .self$password == "") {
        stop("Username or password empty. Check your credentials.")
      }
      handle_setopt(c.handle,
                    referer = .self$data_server,
                    useragent = connection$useragent,
                    followlocation = TRUE,
                    autoreferer = TRUE,
                    username = .self$username,
                    password = .self$password,
                    httpauth = 1,
                    ssl_verifyhost = 0,
                    ssl_verifypeer=0
      )
      return(c.handle)
    },
    api_handle = function() {
      c.handle <- new_handle()
      if (.self$username == "" | .self$password == "") {
        stop("Username or password empty. Check your credentials.")
      }
      handle_setopt(c.handle,
                    referer = .self$api_server,
                    useragent = connection$useragent,
                    followlocation = TRUE,
                    autoreferer = TRUE,
                    username = .self$username,
                    password = .self$password,
                    httpauth = 1,
                    ssl_verifyhost = 0,
                    ssl_verifypeer=0
      )
      return(c.handle)
    },
    http_call = function(url,c.handle=new_handle()) {
      con <- curl(url, handle = c.handle)
      tryCatch({
        html <- suppressWarnings(readLines(con))
      },
      error = function(e) {
        close(con)
        stop(e)
      })
      close(con)
      return(html)
    },
    api_call = function(url) {
      c.handle <- api_handle()
      con <- curl(url, handle = c.handle)
      tryCatch({
        html <- suppressWarnings(readLines(con))
      },
      error = function(e) {
        close(con)
        stop(e)
      })
      close(con)
      return(html)
    },
    file_download = function(url,dest_file,handle=new_handle()){
      curl_download(url,
                    destfile = dest_file, handle = handle)
    },
    mod_query = function(product,collection=61,...){
      args=list(...)
      if((!"dates"%in%names(args))&
         ((!"startDate"%in%names(args)|(!"endDate"%in%names(args))))
      )stop("startDate and endDate, or dates argsument need to be defined!")

      if("dates"%in%names(args)){
        stopifnot(class(args$dates)=="Date")
        startDate<-min(args$dates)
        endDate<-max(args$dates)
      }else{
        startDate<-args$startDate
        endDate<-args$endDate
      }
      stopifnot(class(startDate)=="Date")
      stopifnot(class(endDate)=="Date")

      query.server <- paste0(api_server,"/search/granules")

      if("lonlat"%in%names(args)){
        stopifnot(class(args$lonlat)=="numeric")
        stopifnot(length(args$lonlat)==2)
        query<-paste0(query.server,
                      "?short_name=",product,
                      "&point=",args$lonlat[2],",",args$lonlat[1],
                      "&equator_crossing_date=",format(startDate,"%Y-%m-%d"),"T10:00:00Z",
                      ",",format(endDate,"%Y-%m-%d"),"T12:00:00Z")
      }else if("extent"%in%names(args)){
        stopifnot(class(extent(args$extent))=="Extent")
        query<-paste0(query.server,
                      "?short_name=",product,
                      "&bounding_box=",paste0(c(st_bbox(args$extent)),collapse = ","),
                      "&equator_crossing_date=",format(startDate,"%Y-%m-%d"),"T10:00:00Z",
                      ",",format(endDate,"%Y-%m-%d"),"T12:00:00Z")
      }else if("region"%in%names(args)){
        args$region<-transform_multiple_proj(args$region, proj4=st_crs(4326))
        query<-paste0(query.server,
                      "?short_name=",product,
                      "&bounding_box=",paste0(st_bbox(args$region),collapse = ","),
                      "&equator_crossing_date=",format(startDate,"%Y-%m-%d"),"T10:00:00Z",
                      ",",format(endDate,"%Y-%m-%d"),"T12:00:00Z")
      }else{
        stop('Search region not defined.')
      }

      query <- paste0(query,"&page_size=2000")
      if(("verbose"%in%names(args))&&args$verbose){
        message(paste0("Search query: ",query))
      }
      return(query)
    },
    search = function(region,
                      collection = 6,
                      verbose = FALSE,
                      ...) {
      args <- list(...)
      query <- .self$mod_query(
        collection = collection,
        region = region,
        ...
      )

      if (verbose) message(query)

      references <- .self$http_call(query)
      if(!isXMLString(references)){
        message(paste0("Used query: ",query))
        message(paste0("Server response: ",references))
        stop("Error in server request")
      }
      xml.list <- xmlToList(references)

      res.download<-c()
      res.preview<-c()
      for(meta.query in xml.list$references){
        if(verbose) message(paste0("Server meta query: ",meta.query))
        http.meta <- .self$http_call(meta.query$location)
        xml.meta <- xmlToList(http.meta)
        res.preview<-c(res.preview,xml.meta$AssociatedBrowseImageUrls$ProviderBrowseUrl$URL)
        res.download<-c(res.download,xml.meta$OnlineAccessURLs$OnlineAccessURL$URL)
      }

      if(length(res.preview)>length(res.download)){
        if(length(res.preview)%%length(res.download)==0){
          res.preview<-res.preview[seq(1,length(res.preview),length(res.preview)/length(res.download))]
        }else{
          warning("Preview image in the records may be incorrect.")
          res.preview<-res.preview[seq(1,length(res.download),1)]
        }
      }

      pr <- modGetPathRow(res.download)
      pt <- as.numeric(substr(pr, 2, 3))
      rw <- as.numeric(substr(pr, 5, 6))
      bounds <- c()
      mod.tiles.sinusoidal<-st_transform(mod.tiles,crs = st_crs("ESRI:54008"))
      for (n in paste0("h:", pt, " v:", rw)) {
        bounds <- rbind(bounds,
                        st_bbox(
                          mod.tiles.sinusoidal[mod.tiles.sinusoidal$Name == n, ]
                          ))
      }

      nlen <- length(res.download)
      prdc <- list(...)$product
      img.name <- gsub("\\.hdf", "", basename(res.download))

      return(new_record(
        sat = rep("Modis", nlen),
        name = img.name,
        date = modGetDates(res.download),
        product = rep(prdc, nlen),
        download = res.download,
        file_path = file.path("Modis", prdc, paste0(img.name, ".hdf")),
        path = as.numeric(substr(pr, 2, 3)),
        row = as.numeric(substr(pr, 5, 6)),
        tileid = rep("", nlen),
        preview = res.preview,
        api_name = rep("lpdaac", nlen),
        order = rep(FALSE, nlen),
        extent_crs = new("extent_crs",
                         EPSG = rep(54008, nlen),
                         # EPSG=st_crs("ESRI:54008"),
                         xmin = bounds[, "xmin"],
                         ymin = bounds[, "ymin"],
                         xmax = bounds[, "xmax"],
                         ymax = bounds[, "ymax"]
        )
      )
      )
    },
    download_lpdaac_records = function(lpdaac_records,db_path,verbose,...){
      message("Downloading from lpdaac service...")
      for (i in seq_len(length(lpdaac_records))) {
        out.name <- file.path(db_path, get_file_path(lpdaac_records[i]))
        dir.create(dirname(out.name), showWarnings = FALSE, recursive = TRUE)
        if (!file.exists(out.name) || file.size(out.name) == 0) {
          message(paste0("Downloading ", names(lpdaac_records[i]), " image."))
          file_download(get_download(lpdaac_records[i]),
                                      out.name,handle=.self$data_handle())
        } else {
          message(paste0(names(lpdaac_records[i]), " already in your database."))
        }
      }
    },
    getProducts =function(){
      return(list(MODIS_Aqua=c("MYD09A1","MYD09CMG","MYD09GA","MYD09GQ","MYD09Q1","MYD11A1","MYD11A2 ","MYD11B1","MYD11B2","MYD11B3","MYD11C1","MYD11C2","MYD11C3","MYD11_L2","MYD13A1","MYD13A2 ","MYD13A3","MYD13C1","MYD13C2","MYD13Q1","MYD14","MYD14A1","MYD14A2","MYD15A2H","MYD16A2","MYD16A2GF","MYD16A3GF","MYD17A2H","MYD17A2HGF","MYD17A3HGF","MYD21","MYD21A1D","MYD21A1N","MYD21A2","MYD21C1","MYD21C2","MYD21C3","MYD28C2","MYD28C3"),
                  MODIS_Terra=c("MOD09A1","MOD09CMG","MOD09GA","MOD09GQ","MOD09Q1","MOD11A1","MOD11A2 ","MOD11B1","MOD11B2","MOD11B3","MOD11C1","MOD11C2","MOD11C3","MOD11_L2","MOD13A1","MOD13A2 ","MOD13A3","MOD13C1","MOD13C2","MOD13Q1","MOD14","MOD14A1","MOD14A2","MOD15A2H","MOD16A2","MOD16A2GF","MOD16A3GF","MOD17A2H","MOD17A2HGF","MOD17A3HGF","MOD21","MOD21A1D","MOD21A1N","MOD21A2","MOD21C1","MOD21C2","MOD21C3","MOD28C2","MOD28C3")))
    }

  )
)


setMethod(
  "api_lpdaac",
  signature("character","missing"),
  function(api_name) {
    api <- new("api_lpdaac")
    api$api_name <- api_name
    api
  }
)
setMethod(
  "api_lpdaac",
  signature("character", "character"),
  function(api_name, request) {
    api <- new("api_lpdaac")
    api$api_name <- api_name
    api$request <- request
    api
  }
)

#' @rdname print
#' @aliases print,api
setMethod(
  "print",
  signature(x = "api_lpdaac"),
  function(x) {
    cat("Api Name: ", x$api_name, "\n", "Api Server: ", x$api_server)
  }
)
