#' @importFrom curl curl new_handle handle_setopt curl_download
#' @importFrom curl curl_fetch_memory handle_setform
#' @importFrom httr POST content_type content authenticate
#' @importFrom httr add_headers content_type
#' @importFrom xml2 read_html xml_attr
#' @importFrom rvest html_nodes
#' @importFrom methods new
#' @importFrom rjson fromJSON toJSON
setGeneric("api_usgs",
           function(api_name, request, ...) {
             standardGeneric("api_usgs")
           })
setRefClass(
  Class = "api_usgs",
  fields = list(
    api_name = "character",
    server = "character",
    espa_server = "character",
    m2m_server = "character",
    username = "character",
    password = "character",
    request = "character",
    credentials = "character",
    status = "character",
    order.list = "list",
    api_key = "character"
  ),
  methods = list(
    initialize = function() {
      .self$username <- ""
      .self$password <- ""
      .self$request <- "rsat request"
      .self$server <- "https://usgs.gov"
      .self$espa_server <- "https://espa.cr.usgs.gov/api/v1"
      .self$m2m_server <- "https://m2m.cr.usgs.gov/api/api/json/stable"
      .self$credentials <- "earthdata"
      .self$status <- "Not checked"
      .self$api_key <- ""
      .self$order.list <- list("order" = c(), "status" = c(), "id" = c())
    },
    ######################################################################
    # Generics
    #####################################################################
    checkCredentials = function() {
      if (length(.self$username) == 0) {
        stop("Username must be defined.")
      }
      if (length(.self$password) == 0) {
        stop("Password must be defined.")
      }
    },
    request = function(request) {
      .self$request <- request
    },
    getCredentials = function() {
      return(c(username = .self$username, password = .self$password))
    },
    http_call = function(url, c.handle = new_handle()) {
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
    espa_handle = function() {
      c.handle <- new_handle()
      if (.self$username == "" | .self$password == "") {
        stop("Username or password empty. Check your credentials.")
      }
      handle_setopt(c.handle,
                    referer = .self$espa_server,
                    useragent = connection$useragent,
                    followlocation = TRUE,
                    autoreferer = TRUE,
                    username = .self$username,
                    password = .self$password,
                    #httpauth = 1,
                    ssl_verifyhost = 0,
                    ssl_verifypeer=0
      )
      return(c.handle)
    },
    ##########################
    # Landsat query
    ###########################
    ls_query=function(server, datasetName, startDate, endDate, sf.obj, apiKey, ...) {
      args <- list(...)
      lsquery <- NULL
      # temporal filter
      lsquery$datasetName <- datasetName #' LANDSAT_8_C1'
      # additional criteria
      lsquery$maxResults <- "50000"
      lsquery$startingNumber <- "1"
      lsquery$sortDirection <- "ASC"

      lsquery$sceneFilter <- NULL

      # spatial filter
      lsquery$sceneFilter$acquisitionFilter <- list(
        "start" = format(startDate, "%Y-%m-%d"),
        "end" = format(endDate, "%Y-%m-%d")
      )
      lsquery$sceneFilter$spatialFilter <- list(
        "filterType" = "mbr",
        "lowerLeft" = list(
          "latitude" = st_bbox(sf.obj)[["ymin"]],
          "longitude" = st_bbox(sf.obj)[["xmin"]]
        ),
        "upperRight" = list(
          "latitude" = st_bbox(sf.obj)[["ymax"]],
          "longitude" = st_bbox(sf.obj)[["xmax"]]
        )
      )
      if ("cloudCover" %in% names(args)) {
        if (length(args$cloudCover) == 2 && inherits(args$cloudCover,"numeric")){
          lsquery$sceneFilter$cloudCoverFilter <- NULL
          lsquery$sceneFilter$cloudCoverFilter$min <- min(args$cloudCover)
          lsquery$sceneFilter$cloudCoverFilter$max <- max(args$cloudCover)
          if ("includeUnknownCloudCover" %in% names(args)) {
            lsquery$sceneFilter$cloudCoverFilter$includeUnknownCloudCover <-
              args$includeUnknownCloudCover
          } else {
            lsquery$sceneFilter$cloudCoverFilter$includeUnknownCloudCover <-
              "true"
          }
        } else {
          stop("cloudCover must be a numeric argument")
        }
      }

      return(list(url = file.path(server, "scene-search"),
                  json = toJSON(lsquery)))
    },
    search=function(region,
                    product,
                    startDate,
                    endDate,
                    dates,
                    logoout = TRUE,
                    lvl = 2,
                    verbose = FALSE,
                    ...) {
      if (!missing(dates)) {
        startDate <- min(dates)
        endDate <- max(dates)
      }

      if (.self$getApiKey() == "") {
        .self$loginUSGSApiKey(verbose = verbose)
      }
      attempts <- 5

      repeat{
        query <- ls_query(
          server = .self$m2m_server,
          datasetName = product,
          startDate = startDate,
          endDate = endDate,
          sf.obj = st_transform(region, st_crs(4326)),
          ...
        )
        if(verbose) message(paste0("Landsat_query: ",query))

        jsonres <- .self$postApiUSGS(query$url, query$json, .self$getApiKey())
        attempts <- attempts - 1
        if (is.null(jsonres$errorCode)) {
          break
        } else {
          warning(jsonres$errorCode)
        }
        if (attempts == -1) {
          warning(paste0("Cannot perform Landsat search, check your ",
                         "credentials and/or the api status: ",
                         "https://m2m.cr.usgs.gov/api/docs/json/"))
          return(new("records"))
        }
        .self$loginUSGSApiKey(verbose = verbose)
      }

      if ((is.null(jsonres$data$recordsReturned)) ||
          jsonres$data$recordsReturned == 0) {
        return(new("records"))
      }
      #################################################################
      # res.df<-data.frame(t(sapply(jsonres$data$results,c)))
      json_file <- lapply(jsonres$data$results, function(x) {
        # x[sapply(x, is.null)] <- NA
        x[vapply(x, is.null,FUN.VALUE = logical(1))] <- NA
        unlist(x)
      })
      res.df <- as.data.frame(do.call(rbind, json_file))

      bounds <- res.df[, grepl("spatialBounds", names(res.df))]
      bounds <- bounds[, -c(1)]
      bounds <- t(apply(bounds, 1, function(x) {
        x <- as.numeric(x)
        longitudes <- x[seq(1, 8, 2)]
        latitudes <- x[seq(2, 8, 2)]
        return(c(min(longitudes), min(latitudes),
                 max(longitudes),
                 max(latitudes)))
      }))
      rownames(bounds) <- NULL
      colnames(bounds) <- c("LongitudeMin",
                            "LatitudeMin",
                            "LongitudeMax",
                            "LatitudeMax")
      switch(lvl,
             "1" = {
               api <- "usgs"
               img.name <- unlist(res.df$entityId)
               nlen <- length(img.name)
               dataset.data <- .self$getUSGSdatasetID(product, verbose = verbose)
               download_url <- paste0(m2m_server$server, "/download/",
                                      dataset.data$datasetId, "/",
                                      unlist(res.df$entityId), "/EE/")
               pr <- lsGetPathRow(img.name)
               path <- as.numeric(substr(pr, 1, 3))
               row <- as.numeric(substr(pr, 4, 6))
               d <- lsGetDates(img.name)
               order <- rep(FALSE, nlen)
               fe <- ".tar.gz"
             },
             "2" = {
               api <- "usgs"
               img.name <- unlist(res.df$displayId)
               nlen <- length(img.name)
               download_url <- paste0(.self$espa_server,
                                      "/available-products/", res.df$displayId)
               pr <- gsub(".*_\\s*(\\d{6})_.*", "\\1", img.name)
               path <- as.numeric(substr(pr, 1, 3))
               row <- as.numeric(substr(pr, 4, 6))
               d <- as.Date(gsub(".*?\\s*(\\d{8}).*", "\\1", img.name), "%Y%m%d")
               order <- rep(TRUE, nlen)
               fe <- ".tar.gz"
             }
      )

      #######################################################################
      # if(logout){logoutEEAPI(verbose)}
      # file extension
      if (tolower(product)%in% getProducts()['Landsat-8']) {
        sat <- "Landsat-8"
      } else if (tolower(product)%in%getProducts()['Landsat-7']) {
        sat <- "Landsat-7"
      } else if(tolower(product)%in%getProducts()['Landsat_1-5']){
        sat <- "Landsat_1-5"
      }else{
        # for example landsat_ard_tile_c2, landsat_ard_tile_files_c2
        sat <- "Landsat"
        fe <- ".tar.gz"
      }


      return(new_record(
        sat = rep(sat, nlen),
        name = img.name,
        date = d,
        product = rep(paste0(product),# "_lvl", lvl)
                      nlen),
        download = download_url,
        file_path = file.path(sat, paste0(product),# "_lvl", lvl),
                              paste0(img.name, fe)),
        path = path,
        row = row,
        tileid = rep("", nlen),
        preview = unlist(res.df$browse.browsePath),
        api_name = rep(api, nlen),
        order = order,
        extent_crs = new("extent_crs",
                         EPSG = rep(4326, nlen),
                         xmin = bounds[, "LongitudeMin"],
                         ymin = bounds[, "LatitudeMin"],
                         xmax = bounds[, "LongitudeMax"],
                         ymax = bounds[, "LatitudeMax"]
        )
      ))
    },
    ###############################################################
    # Login m2m usgs API
    ###############################################################
    getApiKey = function() {
      if (.self$api_key == "") {
        .self$loginUSGSApiKey()
      }
      .self$api_key
    },
    setApiKey =function(api_key,verbose = FALSE){
      if(is.null(api_key)){
        .self$api_key<-""
      }else{
        .self$api_key<-api_key
      }
    },
    loginUSGSApiKey = function(verbose = FALSE) {
      jsonquery <- list(
        "username" = .self$username,
        "password" = .self$password,
        "authType" = "EROS",
        "catalogId" = "EE"
      )
      post.res <- POST(
        url = paste0(.self$m2m_server, "/login"),
        body = paste0(toJSON(jsonquery)),
        content_type("application/x-www-form-urlencoded; charset=UTF-8")
      )
      res <- content(post.res)
      if (!is.null(res$errorCode)) {
        stop(res$errorMessage)
      }
      if (verbose) message(paste0("Logged into EE API. Api key:",res$data))
      .self$setApiKey(res$data,verbose)
      if(is.null(res$data)) stop("API key is null.")
    },
    postApiUSGS = function(url, body, key) {
      names(key) <- "X-Auth-Token"
      post.res <- POST(
        url = url,
        body = body,
        content_type("application/json"),
        add_headers(key)
      )
      if (post.res$status_code == 200) {
        return(content(post.res))
      }
      return(list(errorCode = paste0("Error in Earth Explorer api ",
                                     "connection. HTTP ",
                                     post.res$status_code, ".")))
    },
    getUSGSdatasetID = function(product, verbose = FALSE) {
      url <- paste0(.self$m2m_server, "/dataset")
      key <-
      names(key) <- "X-Auth-Token"
      body <- paste0('{"datasetName":"', product, '"}')
      post.res <- POST(
        url = url,
        body = body,
        content_type("application/json"),
        add_headers(key) # ,
        # authenticate(user="user",#change this
        #             password="pass",
        #             type = "basic")
      )
      return(content(post.res)$data)
    },
    postDownloadUSGS = function(url, verbose = FALSE) {
      key <- .self$getApiKey()
      names(key) <- "X-Auth-Token"
      body <- paste0("{}")
      post.res <- POST(
        url = url,
        body = body,
        content_type("application/json"),
        add_headers(key) # ,
        # authenticate(user="user",#change this
        #             password="pass",
        #             type = "basic")
      )
      return(content(post.res)$data)
    },
    logoutUSGSAPI = function() {
      jsonquery <- list("apikey" = .self$getApiKey())
      if (!is.null(jsonquery$apikey)) {
        post.res <- POST(
          url = paste0(.self$m2m_server, "/logout"),
          body = URLencode(paste0("jsonRequest=", toJSON(jsonquery))),
          content_type("application/x-www-form-urlencoded; charset=UTF-8")
        )
        res <- content(post.res)
        if (res$error != "") {
          message("Logged out from USGS API.")
          .self$setApiKey("",verbose)
        } else {
          message("You are not logged in USGS API.")
          stop(res$error)
        }
      } else {
        message("You are not logged in USGS API.")
      }
    },
    ###############################################################
    # ESPA Connections
    ###############################################################
    espaOrderImage = function(img_name,
                              product = "sr",
                              update.orders = TRUE,
                              verbose = FALSE,
                              ...) { # c("sr","source_metadata")
      if (length(img_name) > 1)
        stop("Only one image is supported for each ESPA order.")
      if (update.orders) {
        .self$espaGetOrders(verbose)
      }
      if (!img_name %in% .self$order.list$id) {
        url.products <- paste0(.self$espa_server,
                               "/available-products/",
                               img_name)
        if(verbose) message(paste0("Products url: ",url.products))
        json_data <- rjson::fromJSON(paste(.self$http_call(url.products,espa_handle()),
                                           collapse = ""))
        if(verbose){message(paste0("ESPA response product: \n",json_data))}
        json_data2 <- unlist(json_data, recursive = TRUE)
        products <- json_data2[grepl("products", names(json_data2))]
        if (length(products) == 0) {
          warning(paste0("Defined products are not available for image ",
                         img_name))
          warning(paste0("Products ", paste(json_data2, collapse = ", ")))
        }
        if (any(!(product %in% products))) {
          product <- product[product %in% products]
          if (length(product) == 0) {
            warning(paste0("Defined products are not available for image ",
                           img_name))
            warning(paste0("Products ",
                           paste(json_data2, collapse = ", ")))
          }
        }

        # create the query
        json_data[[1]]$products <- product # c("sr","source_metadata")#product
        json_post <- list(
          projection = list(lonlat = NA),
          format = "gtiff",
          resampling_method = "cc",
          note = .self$request
        )
        json_post <- append(json_post, json_data)
        query <- toEspaJSON(json_post)

         if(verbose){message(paste0("ESPA query: \n",query))}
        res <- POST(paste0(.self$espa_server, "/order"),
                    authenticate(.self$username, .self$password),
                    body = as.character(query)
        )
        if (verbose) {
          message(paste0("Order response:",res$status))
        }
        message(paste0(img_name, " image ordered!"))
        if(verbose){message(paste0("ESPA Order: \n",res))}
      } else {
        message(paste0("Alredy ordered image. Name: ", img_name))
      }
    },
    espaGetOrders = function(verbose = FALSE) {
      .self$espaUpdateOrderStatus()
      r <- curl_fetch_memory(paste0(.self$espa_server, "/list-orders"),
                             espa_handle())
      newOrders <- fromJSON(rawToChar(r$content))
      dates <- as.Date(gsub(".*\\s*(\\d{8}).*", "\\1", newOrders), "%m%d%Y")
      newOrders <- newOrders[Sys.Date() - dates < 8]
      if (length(newOrders) == 0 & verbose) {
        return(message("There are no ordered images."))
      }

      newOrders <- newOrders[!(newOrders %in% .self$order.list$order)]
      if (length(newOrders) > 0) {
        for (o in newOrders) {
          if (verbose) message(paste0("Order query: ",.self$espa_server, "/order/", o))
          r <- curl_fetch_memory(paste0(.self$espa_server, "/order/", o),
                                 espa_handle())
          json_data <- fromJSON(rawToChar(r$content))
          if (verbose) message(paste0("Order response:",json_data$status))
          if (json_data$note == .self$request &
              tolower(json_data$status) %in% c("complete",
                                               "processing",
                                               "oncache",
                                               "tasked",
                                               "ordered",
                                               "submitted")) {
            all.response <- unlist(json_data, recursive = TRUE)
            .self$order.list$order <- c(.self$order.list$order, o)
            .self$order.list$status <- c(.self$order.list$status,
                                         json_data$status)
            .self$order.list$id <- c(.self$order.list$id,
                                     all.response[grepl("inputs",
                                                        names(all.response))])
          }
        }
      }
    },
    espaUpdateOrderStatus = function() {
      norder <- length(.self$order.list$order)
      if (norder > 0) {
        for (o in 1:norder) {
          r <- curl_fetch_memory(paste0(.self$espa_server, "/order/",
                                        .self$order.list$order[o]),
                                 espa_handle())
          json_data <- fromJSON(rawToChar(r$content))
          if (!(length(json_data$note) == 0) &&
              json_data$note == .self$request) {
            all.response <- unlist(json_data, recursive = TRUE)
            .self$order.list$status[o] <- json_data$status
          }
        }
      }
    },
    espaDownloadsOrders = function(tile_name, out.file, verbose = FALSE) {
      espaGetOrders(verbose = verbose)
      c.handle <- .self$espa_handle()
      order_name <- .self$order.list$order[.self$order.list$id %in% tile_name]
      if (is.na(order_name[1])) {
        if (verbose)
          message(paste0(tile_name,
                         " image not ordered, cannot be downloaded."))
        return(TRUE)
      }
      r <- curl_fetch_memory(paste0(.self$espa_server, "/item-status/",
                                    order_name[1]),
                             c.handle)
      json_data <- unlist(fromJSON(rawToChar(r$content)),
                          recursive = TRUE)
      o.status <- json_data[grepl("status", names(json_data))]
      if (verbose) {
        message(paste0(tile_name, " order status: ", o.status))
      }

      if (tolower(o.status) == "complete") {
        message(paste0("Downloading ", tile_name, " image."))
        durl <- json_data[grepl("product_dload_url", names(json_data))]
        curl_download(url = durl, destfile = out.file, handle = c.handle)
        md5.url <- unlist(json_data, recursive = TRUE)
        md5.url <- md5.url[grepl("cksum_download_url", names(md5.url))]
        rmd5 <- curl_fetch_memory(
          md5.url,
          c.handle
        )
        md.file <- unlist(strsplit(rawToChar(rmd5$content), " "))
        if (genCheckMD5(out.file, toupper(md.file[1]))) {
          return(TRUE)
        } else {
          message(paste0("ERROR CHECKING MD5 OF ",
                         tile_name,
                         " IMAGE, TRYING THE DOWNLOAD PROCESS AGAIN."))
          file.remove(out.file)
          return(FALSE)
        }
      } else if (tolower(o.status) == "processing" |
                 tolower(o.status) == "oncache" |
                 tolower(o.status) == "tasked" |
                 tolower(o.status) == "submitted") {
        return(FALSE)
      } else if (tolower(o.status) == "unavailable") {
        message(paste0(tile_name, " image unavailable, try again later."))
        return(TRUE)
      } else {
        if (verbose) {
          message(paste0("Check order status: ", tolower(o.status)))
          message(paste0("Unknown download error with ",
                         tile_name,
                         " image, omitting this download."))
        }
        return(TRUE)
      }
    },
    order_usgs_records = function(espa_orders,db_path,verbose,...){
      message("Ordering on ESPA platform...")
      args <- list(...)
      orders <- new("records")
      if("product"%in%names(args)){
        product<-args$product
      }else{
        product<-"l1"
      }

      update <- TRUE
      while(length(espa_orders)>0){
        out.name <- file.path(db_path, get_file_path(espa_orders[1]))
        if (get_order(espa_orders[1]) & (!file.exists(out.name) ||
                                         file.size(out.name) == 0)){
          espaOrderImage(names(espa_orders[1]),
                         update.orders = update,
                         verbose = verbose,
                         product=product)
          update <- FALSE
          orders <- c(orders,espa_orders[1])
        }
        espa_orders <- espa_orders[-1]
      }
      return(orders)
    },
    download_espa_orders = function(espa.orders,db_path,verbose,...){
      message("Downloading on ESPA platform...")
      while (length(espa.orders) > 0) {
        for (i in rev(seq_len(length(espa.orders)))) {
          out.name <- file.path(db_path, get_file_path(espa.orders[i]))
          dir.create(dirname(out.name),
                     showWarnings = FALSE,
                     recursive = TRUE)
          if (!file.exists(out.name) || file.size(out.name) == 0) {
            if (espaDownloadsOrders(names(espa.orders[i]),
                                    out.name,
                                    verbose = verbose)) {
              espa.orders <- espa.orders[-i]
            }
          } else {
            message(paste0(names(espa.orders[i]), " already in your database."))
            espa.orders <- espa.orders[-i]
          }
        }
        if (length(espa.orders) > 0) {
          message("Waiting for ordered images.")
          Sys.sleep(10)
        }
      }
    },
    espa_order_and_download = function(usgs,espa.orders,db_path,verbose,...){
      espa.orders <- order_usgs_records(usgs,db_path,verbose,...)
      download_espa_orders(espa.orders,db_path,verbose,...)
    },
    getProducts = function(){
      return(list('Landsat-8'=c("landsat_ot_c2_l2",
                                "landsat_ot_c2_l1",
                                "lsr_landsat_8_c1",
                                "landsat_8_c1"),
                  'Landsat-7' = c("landsat_etm_c2_l2",
                                  "landsat_etm_c2_l1",
                                  "lsr_landsat_etm_c1",
                                  "landsat_etm_c1"),
                  'Landsat_1-5'=c("landsat_tm_c2_l2",
                                  "landsat_tm_c2_l1",
                                  "landsat_mss_c2_l1",
                                  "lsr_landsat_tm_c1",
                                  "landsat_tm_c1",
                                  "landsat_mss_c1")))
    }
  )
)


setMethod(
  "api_usgs",
  signature("character","missing"),
  function(api_name) {
    api <- new("api_usgs")
    api$api_name <- api_name
    api
  }
)
setMethod(
  "api_usgs",
  signature("character", "character"),
  function(api_name, request) {
    api <- new("api_usgs")
    api$api_name <- api_name
    api$request <- request
    api
  }
)

#' @rdname print
#' @aliases print,api
setMethod(
  "print",
  signature(x = "api_usgs"),
  function(x) {
    cat("Api Name: ", x$name, "\n", "Api Server: ", x$espa_server)
  }
)
