#' @importFrom curl curl new_handle handle_setopt curl_download handle_setheaders
#' @importFrom curl curl_fetch_memory handle_setform
#' @importFrom httr POST content_type content authenticate
#' @importFrom httr add_headers content_type
#' @importFrom xml2 read_html xml_attr
#' @importFrom rvest html_nodes
#' @importFrom methods new
#' @importFrom utils URLencode
#' @importFrom sf st_as_text st_as_sfc st_bbox st_set_crs st_crs
setGeneric("api_dataspace",
           function(api_name, request, ...) {
             standardGeneric("api_dataspace")
           })
setRefClass(
  Class = "api_dataspace",
  fields = list(
    api_name = "character",
    server = "character",
    api_server = "character",
    username = "character",
    password = "character",
    credentials = "character",
    status = "character",
    token.url="character",
    access.token = "character",
    refresh.token = "character",
    token.time = "POSIXct",
    refresh.token.time ="POSIXct"
  ),
  methods = list(
    initialize = function() {
      .self$username <- ""
      .self$password <- ""
      .self$server <- "https://dataspace.copernicus.eu"
      .self$api_server <- "https://catalogue.dataspace.copernicus.eu/odata/v1"
      .self$credentials <- "dataspace"
      .self$status <- "Not checked"
      .self$token.url <-"https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"
      .self$access.token <- ""
      .self$refresh.token <- ""
      .self$token.time <- as.POSIXct(0)
      .self$refresh.token.time <- as.POSIXct(0)
    },
    #####################################################################
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
    api_handle = function() {
      if (.self$username == "" | .self$password == "") {
        stop("Username or password empty. Check your credentials.")
      }
      c.handle <- new_handle()
      handle_setopt(c.handle,
                    referer = .self$server,
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
    http_call = function(url, c.handle = new_handle()) {
      con <- curl(url, handle = c.handle)
      tryCatch({
        html <- suppressWarnings(readLines(con))
      },
      error = function(e) {
        close(con)
        warning(paste0("Call query: ",url))
        stop(e)
      }
      )
      close(con)
      return(html)
    },
    file_download = function(url,dest_file,handle=new_handle()){
      curl_download(url,
                    destfile = dest_file, handle = handle)
    },
    query_dataspace = function(server, product, ...) {
      arg <- list(...)
      query <- file.path(server, "Products?$filter=")

      if (!"verbose" %in% names(arg)) {
        arg$verbose <- FALSE
      }

      if (!missing(product)) {
        if (arg$verbose) {
          message("Added product type.")
        }
        query <- paste0(query, "Attributes/OData.CSC.StringAttribute/any(att:att/Name eq 'productType' and att/OData.CSC.StringAttribute/Value eq '",product,"')")
      } else {
        stop("product must to be defined for sentinel image searching.")
      }

      # add ingestion date to query
      if ("startDate" %in% names(arg)) {
        if (arg$verbose) {
          message("Adapting dates.")
        }
        startDate <- paste0(format(arg$startDate, "%Y-%m-%d"), "T00:00:00.000Z")
        if (is.null(arg$endDate)) {
          endDate <- paste0(format(Sys.Date(), "%Y-%m-%d"), "T23:59:59.999Z")
        } else {
          endDate <- paste0(format(arg$endDate, "%Y-%m-%d"), "T23:59:59.999Z")
        }
        query <- paste0(query, " and ContentDate/Start gt ", startDate, " and ContentDate/End lt ", endDate)
      }
      if ("region" %in% names(arg)) {
        if (arg$verbose) {
          message("Adding query region")
        }
        arg$region <- transform_multiple_proj(arg$region, proj4 = st_crs(4326))
        query <- paste0(
          query, " and OData.CSC.Intersects(area=geography'SRID=4326;",
          st_as_text(st_as_sfc(st_bbox(st_bbox(arg$region)))),"')")
      }
      if ("cloudCover" %in% names(arg)) {
        if (arg$verbose) {
          message("Added cloud cover percentage.")
        }
        query <- paste0(query, " and Attributes/OData.CSC.DoubleAttribute/any(att:att/Name eq 'cloudCover' and att/OData.CSC.DoubleAttribute/Value le ",sprintf("%05.2f", arg$cloudCover),")")
      }
      query <- paste0(query, "&$expand=Assets&$top=10&$count=True")
      return(URLencode(query))
    },
    search = function(region,
                      product,
                      dates,
                      startDate,
                      endDate,
                      verbose = FALSE,
                      ...) {
      if (!missing(dates)) {
        startDate <- min(dates)
        endDate <- max(dates)
      }

      query <- query_dataspace(
        server = getServer(),
        product = product,
        startDate = startDate,
        endDate = endDate,
        region = region,
        verbose = verbose,
        ...
      )

      if(verbose) message(paste0("Sentinel_query: ",query))

      res.download <- fromJSON(http_call(query))

      nlen<-res.download$`@odata.count`
      if (!is.na(nlen) & nlen > 0) {
        name <- c()
        download <- c()
        preview <- c()
        bounds <- c()
        dates <- c()
        path <- c()
        tileid <- c()
        order <- c()
        nlen <- 0
        for(i in seq(length(SENPRODUCTS))){
          if(product %in% SENPRODUCTS[[i]]){
            sat <- names(SENPRODUCTS)[i]
            break
          }
        }

        repeat{
          for(x in res.download$value){
            name<- c(name,x$Name)
            download <- c(download, paste0("https://zipper.dataspace.copernicus.eu/odata/v1/Products(",x$Id,")/$value"))
            # bounds
            if(verbose){
              message(x$Footprint)
            }
            ft.string <- gsub("geography'", "", x$Footprint)
            ft.string<-unlist(strsplit(ft.string,";"))
            ft.sf <- st_as_sfc(ft.string[2])
            ft.sf <- st_set_crs(ft.sf, st_crs(as.numeric(gsub("SRID=", "", ft.string[1]))))

            bounds <- rbind(bounds,st_bbox(ft.sf))

            dates <- c(dates,as.Date(unlist(strsplit(x$OriginDate,"T"))[1],"%Y-%m-%d"))
            if(length(x$Assets)<1){
              pw <- ""
            }else{
              pw <- x$Assets[[1]]$DownloadLink
            }
            preview <- c(preview,pw)
            order <- c(order,!as.logical(x$Online))
            nlen <- nlen + 1;
          }

          if (!"@odata.nextLink"%in%names(res.download)){break}
          res.download <- fromJSON(http_call(res.download$`@odata.nextLink`))
        }

        if (is.null(tileid)) {
          if (product == "S2MSI2A") {
            tileid <- getTileID_MSIL2A(name)
          } else {
            tileid <- rep("", nlen)
          }
        }
        if(verbose){
          message(paste0("sat: ",length(rep(sat, nlen))))
          message(paste0("name: ",length(name)))
          message(paste0("date: ",length(dates)))
          message(paste0("product: ",length(rep(product, nlen))))
          message(paste0("download: ",length(download)))
          message(paste0("file_path: ",length(file.path(sat, product, paste0(name, ".zip")))))
          message(paste0("path: ",length(rep(0, nlen))) )
          message(paste0("row: ",length(rep(0, nlen))) )
          message(paste0("tileid: ",length(tileid)))
          message(paste0("preview: ",length(preview)))
          message(paste0("api_name ",length(rep("dataspace", nlen))))
          message(paste0("order: ",length(order)))
          message(paste0("bounds: ",nrow(bounds)))
        }
        records <- new_record(
          sat = rep(sat, nlen),
          name = name,
          date = as.Date(dates),
          product = rep(product, nlen),
          download = download,
          file_path = file.path(sat, product, paste0(name, ".zip")),
          path = rep(0, nlen),
          row = rep(0, nlen),
          tileid = tileid,
          preview = preview,
          api_name = rep("dataspace", nlen),
          order = order,
          extent_crs = new("extent_crs",
                           EPSG = rep(4326, nlen),
                           xmin = bounds[, "xmin"],
                           ymin = bounds[, "ymin"],
                           xmax = bounds[, "xmax"],
                           ymax = bounds[, "ymax"]
          )
        )

        return(records)
      } else {
        message("There is no images for your region and periods.")
      }
    },
    dataspace_update_token = function(verbose=FALSE){
      if(verbose){
        message(paste0("Access token: ",.self$access.token))
        message(paste0("Refresh token time: ",.self$refresh.token.time))
        message(paste0("Systime: ",Sys.time()))
      }
      if(.self$access.token=="" | .self$refresh.token.time<Sys.time()){
        dataspace_get_token(verbose)
      }else if(.self$token.time<Sys.time()){
        dataspace_refresh_token(verbose)
      }
      if(verbose){
        message(paste0("Access token: ",.self$access.token))
      }
    },
    dataspace_get_token = function(verbose=FALSE){
      if(verbose){
        message('Getting token')
      }
      body <- list(
        grant_type = "password",
        username = .self$username,
        password = .self$password,
        client_id = "cdse-public"
      )

      .self$token.time<-Sys.time()
      .self$refresh.token.time<-Sys.time()

      post_data <- paste(names(body), sapply(body, URLencode), sep = "=", collapse = "&")

      if(verbose){
        message(paste0('Get token post_data: ',post_data))
      }

      response <- curl_fetch_memory(
        url = .self$token.url,
        handle = new_handle(
          post = TRUE,
          customrequest = "POST",
          httpheader = c('Content-Type' = 'application/x-www-form-urlencoded'),
          postfields = post_data
        )
      )
      res<-rawToChar(response$content)
      if(verbose){
        message(paste0('Get token response: ',res))
      }

      res<-fromJSON(res)
      if(is.null(res$access_token)){
        .self$access.token <- ""
        .self$refresh.token <- ""
        .self$token.time <- as.POSIXct(0)
        .self$refresh.token.time <- as.POSIXct(0)
      }else{
        .self$access.token <- res$access_token
        .self$refresh.token <- res$refresh_token
        .self$token.time <- refresh.token.time+res$expires_in-60
        .self$refresh.token.time <- refresh.token.time+res$refresh_expires_in-60
      }
    },

    dataspace_refresh_token = function(verbose=FALSE){
      if(verbose){
        message('Refreshing token')
      }

      body <- list(
        grant_type = 'refresh_token',
        refresh_token = .self$refresh.token,
        client_id = 'cdse-public'
      )
      if(verbose){
        message(paste0('Refresh token used: ',.self$refresh.token))
      }
      response <- curl_fetch_memory(
        url = .self$token.url,
        handle = curl::new_handle(
          post = TRUE,
          customrequest = 'POST',
          httpheader = c('Content-Type' = 'application/x-www-form-urlencoded'),
          postfields = paste(names(body), sapply(body, URLencode), sep = '=', collapse = '&')
        )
      )
      if(verbose){
        message(paste0('Refresh token response: ',response$content))
      }
      res<-fromJSON(rawToChar(response$content))
      if(is.null(res$access_token)){
        .self$access.token <- ""
        .self$refresh.token <- ""
        .self$token.time <- as.POSIXct(0)
        .self$refresh.token.time <- as.POSIXct(0)
      }else{
        .self$access.token <- res$access_token
        .self$refresh.token <- res$refresh_token
        .self$token.time <- refresh.token.time+res$expires_in-60
        .self$refresh.token.time <- refresh.token.time+res$refresh_expires_in-60
      }

    },
    dataspace_download_records = function(records,db_path,verbose=FALSE){
      message("Downloading from datasepace platform...")
      for (i in seq_len(length(records))) {
        out.name <- file.path(db_path, get_file_path(records[i]))
        dir.create(dirname(out.name), showWarnings = FALSE, recursive = TRUE)
        if (!file.exists(out.name) || file.size(out.name) == 0) {
          message(paste0("Downloading ", names(records[i]), " image."))
          dataspace_download(get_download(records[i]),
                             out.path=out.name,
                             verbose=verbose)
        } else {
          message(paste0(names(records[i]), " already in your database."))
        }
      }
    },
    dataspace_download = function(url,out.path,verbose=FALSE){
      dataspace_update_token(verbose)

      if(verbose){
        message(paste0('Access token: ',access.token))
        message(paste0('Download url: ',url))
      }
      handle <- new_handle()
      handle_setheaders(handle,
                        "Content-Type" = "application/json",
                        "Authorization" = paste('Bearer', access.token)
      )
      response <- curl_download(
        url = url,
        handle = handle,
        destfile = out.path
      )
    },
    getProducts=function(){
      return(list(
        "Sentinel-1" = c("SLC", "GRD", "OCN"),
        "Sentinel-2" = c("S2MSI2A",
                         "S2MSI1C",
                         "S2MS2Ap"),
        "Sentinel-3" = c("SR_1_SRA___",
                         "SR_1_SRA_A",
                         "SR_1_SRA_BS",
                         "SR_2_LAN___",
                         "OL_1_EFR___",
                         "OL_1_ERR___",
                         "OL_2_LFR___",
                         "OL_2_LRR___",
                         "SL_1_RBT___",
                         "SL_2_LST___",
                         "SY_2_SYN___",
                         "SY_2_V10___",
                         "SY_2_VG1___",
                         "SY_2_VGP___"),
        "Sentinel-5" = c("L1B_IR_SIR",
                         "L1B_IR_UVN",
                         "L1B_RA_BD1",
                         "L1B_RA_BD2",
                         "L1B_RA_BD3",
                         "L1B_RA_BD4",
                         "L1B_RA_BD5",
                         "L1B_RA_BD6",
                         "L1B_RA_BD7",
                         "L1B_RA_BD8",
                         "L2__AER_AI",
                         "L2__AER_LH",
                         "L2__CH4",
                         "L2__CLOUD_",
                         "L2__CO____",
                         "L2__HCHO__",
                         "L2__NO2___",
                         "L2__NP_BD3",
                         "L2__NP_BD6",
                         "L2__NP_BD7",
                         "L2__O3_TCL",
                         "L2__O3____",
                         "L2__SO2___")
      ))
    }
  )
)

setMethod(
  "api_dataspace",
  signature("character","missing"),
  function(api_name) {
    api <- new("api_dataspace")
    api$api_name <- api_name
    api
  }
)
setMethod(
  "api_dataspace",
  signature("character", "character"),
  function(api_name, request) {
    api <- new("api_dataspace")
    api$api_name <- api_name
    api$request <- request
    api
  }
)

#' @rdname print
#' @aliases print,api
setMethod(
  "print",
  signature(x = "api_dataspace"),
  function(x) {
    cat("Api Name: ", x$api_name, "\n", "Api Server: ", x$api_server)
  }
)
