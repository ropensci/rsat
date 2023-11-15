#' @importFrom curl curl new_handle handle_setopt curl_download
#' @importFrom curl curl_fetch_memory handle_setform
#' @importFrom httr POST content_type content authenticate
#' @importFrom httr add_headers content_type
#' @importFrom xml2 read_html xml_attr
#' @importFrom rvest html_nodes
#' @importFrom methods new
#' @importFrom utils URLencode
setGeneric("api_scihub",
           function(api_name, request, ...) {
             standardGeneric("api_scihub")
           })
setRefClass(
  Class = "api_scihub",
  fields = list(
    api_name = "character",
    server = "character",
    api_server = "character",
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
      .self$server <- "https://apihub.copernicus.eu"
      .self$api_server <- "https://apihub.copernicus.eu/apihub"
      .self$credentials <- "earthdata"
      .self$status <- "Not checked"
      .self$order.list <- list("order" = c(), "status" = c(), "id" = c())
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
    download = function(url, f,verbose) {
      c.handle <- .self$api_handle()
      online <- gsub("/$value", "/Online/$value", url, fixed = TRUE)
      is.online <- curl_fetch_memory(online, handle = c.handle)
      if (rawToChar(is.online$content) == "false") {
        message("The image is archived, ordering...")
        order <- curl_fetch_memory(url, handle = c.handle)
        while (order$status_code != 202) {
          Sys.sleep(10)
          if (order$status_code == 503) {
            message(paste0("Service Unavailable. The retrieval of offline ",
                           "data is temporarily unavailable, please try ",
                           "again later"))
          } else if (order$status_code == 403) {
            message(paste0("Forbidden. User offline products retrieval ",
                           "quota exceeded"))
          } else if (order$status_code == 500) {
            message(paste0("Internal Server Error. Unexpected nav ",
                           "segment Navigation Property"))
          }
          order <- curl_fetch_memory(url, handle = c.handle)
        }
        message("Image ordered!")
        is.online <- curl_fetch_memory(online, handle = c.handle)
        while (rawToChar(is.online$content) == "false") {
          Sys.sleep(10)
          is.online <- curl_fetch_memory(online, handle = c.handle)
        }
        curl_download(url, destfile = f, handle = c.handle)
      } else {
        curl_download(url, destfile = f, handle = c.handle)
      }
    },
    check_LTA = function(records, verbose,...){
      for(r in seq(length(records))){
        html <- http_call(gsub("/$value", "/Online/$value",
                                    get_download(records[r]),
                                    fixed = TRUE),api_handle())
        is.online <- curl_fetch_memory(html, handle = c.handle)
        if (rawToChar(is.online$content) == "true"){ # is online?
          get_order(records[r])<-TRUE
        }else{
          get_order(records[r])<-FALSE
        }
      }
      return(records)
    },
    download_LTA = function(url,out.name, verbose){
      html <- http_call(gsub("/$value", "",
                                  url,
                                  fixed = TRUE),api_handle())
      html <- paste(html, collapse = "\n ")
      if (gsub(".*d:Online>", "", gsub("</d:Online.*", "", html)) ==
          "true"){ # is lta?
        message(paste0("Downloading ",
                       names(scihub.records[i]),
                       " image."))
        download(url, out.name)

      }
    },
    http_call = function(url, c.handle = new_handle()) {
      con <- curl(url, handle = c.handle)
      tryCatch({
        html <- suppressWarnings(readLines(con))
      },
      error = function(e) {
        close(con)
        if (grepl("HTTP error 503.", e$message)) {
          .self$status <- "Offline"
          stop("Service on maintenace. HTTP error 503.")
        } else if (grepl("HTTP error 401.", e$message)) {
          stop("Unauthorized error (HTTP error 401). Check your credentials.")
        } else if (grepl("HTTP error 502.", e$message)) {
          stop("Service on maintenace. HTTP error 502.")
        }
        stop(e)
      }
      )
      close(con)
      return(html)
    },
    sen_query = function(server, product, ...) {
      arg <- list(...)
      query <- file.path(server, "search?q=")
      if (!"verbose" %in% names(arg)) {
        arg$verbose <- FALSE
      }
      # add ingestion date to query
      if ("startDate" %in% names(arg)) {
        if (arg$verbose) {
          message("Adapting dates.")
        }
        startDate <- paste0(format(arg$startDate, "%Y-%m-%d"), "T00:00:00.000Z")
        if (is.null(arg$endDate)) {
          endDate <- "NOW"
        } else {
          endDate <- paste0(format(arg$endDate, "%Y-%m-%d"), "T23:59:59.999Z")
        }
        query <- paste0(query, "beginposition:[", startDate, " TO ", endDate, "]")
      }
      if ("platform" %in% names(arg)) {
        if (arg$verbose) {
          message("Adding platform name.")
        }
        query <- paste0(query, " AND platformname:", arg$platform)
      }
      if ("extent" %in% names(arg)) {
        stopifnot(inherits(extent(arg$extent),"Extent"))
        if (arg$verbose) {
          message("Adding query extent.")
        }
        ext <- extent(arg$extent)
        query <- paste0(
          query, " AND footprint:",
          '"',
          "intersects(POLYGON((", ext@xmin, " ", ext@ymin, ",",
          ext@xmin, " ", ext@ymax, ",",
          ext@xmax, " ", ext@ymax, ",",
          ext@xmax, " ", ext@ymin, ",",
          ext@xmin, " ", ext@ymin, ")))", '"'
        )
      }
      if ("lonlat" %in% names(arg)) {
        if (arg$verbose) {
          message(print("Adding query intersects"))
        }
        if (!length(arg$lonlat) == 2) {
          stop(paste0("The intersects argument is not a",
                      " longitude/latitude valid location."))
        }
        query <- paste0(query, " AND footprint:", '"', "intersects(",
                        arg$lonlat[1], ", ",
                        arg$lonlat[2], ")", '"')
      }
      if ("region" %in% names(arg)) {
        if (arg$verbose) {
          message(print("Adding query region"))
        }
        arg$region <- transform_multiple_proj(arg$region, proj4 = st_crs(4326))
        ext <- st_bbox(arg$region)
        query <- paste0(
          query, " AND footprint:", '"', "intersects(POLYGON((",
          ext$xmin, " ", ext$ymin, ",",
          ext$xmin, " ", ext$ymax, ",",
          ext$xmax, " ", ext$ymax, ",",
          ext$xmax, " ", ext$ymin, ",",
          ext$xmin, " ", ext$ymin, ")))", '"'
        )
      }
      if (!missing(product)) {
        if (arg$verbose) {
          message("Added product type.")
        }
        query <- paste0(query, " AND producttype:", product)
      } else {
        stop("product must to be defined for sentinel image searching.")
      }
      if ("relativeorbit" %in% names(arg)) {
        if (arg$verbose) {
          message("Added relative orbit number type.")
        }
        query <- paste0(query, " AND relativeorbitnumber:", arg$relativeorbit)
      }
      if ("timeliness" %in% names(arg)) {
        if (arg$verbose) {
          message("Added timeliness.")
        }
        url <- paste0(url, ' AND timeliness:"', arg$timeliness, '"')
      }
      if ("cloudCover" %in% names(arg)) {
        if (arg$verbose) {
          message("Added cloud cover percentage.")
        }
        query <- paste0(query, " AND cloudcoverpercentage:[",
                        min(arg$cloudCover), " TO ",
                        max(arg$cloudCover), "]")
      }
      if ("qformat" %in% names(arg)) {
        query <- paste0(query, "&format=", arg$qformat)
      } else {
        query <- paste0(query, "&format=json")
      }
      query <- paste0(query, "&rows=100")
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

      query <- sen_query(
        server = getServer(),
        product = product,
        startDate = startDate,
        endDate = endDate,
        region = region,
        verbose = verbose,
        ...
      )
      if(verbose) message(paste0("Sentinel_query: ",query))
      res.download <- fromJSON(http_call(query,api_handle()))
      ndownload <- as.numeric(res.download$feed$`opensearch:totalResults`)
      if (!is.na(ndownload) & ndownload > 0) {
        name <- c()
        download <- c()
        bounds <- c()
        dates <- c()
        path <- c()
        tileid <- c()
        sat <- c()
        for (img in res.download$feed$entry) {
          name <- c(name, img$title)
          download <- c(download, img$link[[1]]$href)
          dates <- c(dates, as.Date(img$date[[1]]$content))
          for (s in img$str) {
            if (s$name == "footprint") {
              coords <- as.numeric(
                unlist(strsplit(gsub("  ", " ",
                                     gsub(",", " ",
                                          gsub("\\)", "",
                                               gsub(".*\\(", "", s$content)
                                          )
                                     )
                ), " ")
                ))
              nc <- length(coords)
              x <- coords[seq(1, nc, 2)]
              y <- coords[seq(2, nc, 2)]
            }
            if (s$name == "tileid") {
              tileid <- c(tileid, s$content)
            }
            if (s$name == "platformname") {
              sat <- c(sat, s$content)
            }
          }

          pth <- unlist(img$int[[2]])
          if (length(pth) == 2) {
            path <- c(path, as.numeric(pth[2]))
          } else {
            path <- c(path, as.numeric(pth))
          }


          bounds <- rbind(bounds, c(xmin = min(x),
                                    ymin = min(y),
                                    xmax = max(x),
                                    ymax = max(y)))
        }
        nlen <- length(name)

        if (is.null(tileid)) {
          if (product == "S2MSI2A") {
            tileid <- getTileID_MSIL2A(name)
          } else {
            tileid <- rep("", nlen)
          }
        }

        # TODO define order by product
        # order = con$scihubIsLTA(download)
        order <- rep(FALSE, nlen)


        records <- new_record(
          sat = sat,
          name = name,
          date = as.Date(dates),
          product = rep(product, nlen),
          download = download,
          file_path = file.path(sat, product, paste0(name, ".zip")),
          path = path,
          row = rep(0, nlen),
          tileid = tileid,
          preview = gsub("$value", "Products('Quicklook')/$value",
                         download, fixed = TRUE),
          api_name = rep("scihub", nlen),
          order = order,
          extent_crs = new("extent_crs",
                           EPSG = rep(4326, nlen),
                           xmin = bounds[, "xmin"],
                           ymin = bounds[, "ymin"],
                           xmax = bounds[, "xmax"],
                           ymax = bounds[, "ymax"]
          )
        )

        if (ndownload > 100) {
          dt <- dates(records)
          mn.date <- min(dt)
          if (sum(dt == mn.date) == 100) {
            stop("Spatial regions composed by 100 or more tiles are not supported!
           Try the search using a smaller region.")
          }
          if (sum(dt == mn.date) > 50) {
            mn.date <- mn.date - 1
          }
          records <- c(records, sen_search(
            region = region,
            product = product,
            startDate = startDate,
            endDate = mn.date,
            verbose = verbose,
            ...
          ))
        }
        return(records)
      } else {
        message("There is no images for your region and periods.")
      }
    }
  )
)

setMethod(
  "api_scihub",
  signature("character","missing"),
  function(api_name) {
    api <- new("api_scihub")
    api$api_name <- api_name
    api
  }
)
setMethod(
  "api_scihub",
  signature("character", "character"),
  function(api_name, request) {
    api <- new("api_scihub")
    api$api_name <- api_name
    api$request <- request
    api
  }
)

#' @rdname print
#' @aliases print,api
setMethod(
  "print",
  signature(x = "api_scihub"),
  function(x) {
    cat("Api Name: ", x$api_name, "\n", "Api Server: ", x$api_server)
  }
)
