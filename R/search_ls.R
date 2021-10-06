#' @importFrom rjson fromJSON toJSON
#' @importFrom httr add_headers
#'
setGeneric("ls_query", function(server,
                                datasetName,
                                startDate,
                                endDate,
                                sf.obj,
                                ...) {
  standardGeneric("ls_query")
})
setMethod(
  f = "ls_query",
  signature = c("character", "character", "Date", "Date", "sf"),
  function(server, datasetName, startDate, endDate, sf.obj, apiKey, ...) {
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
      if (length(args$cloudCover) == 2 && class(args$cloudCover) == "numeric"){
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
  }
)


setGeneric("ls_search", function(region,
                                 product,
                                 ...) {
  standardGeneric("ls_search")
})
setMethod(
  f = "ls_search",
  signature = c("ANY", "character"),
  function(region,
           product,
           startDate,
           endDate,
           dates,
           logoout = TRUE,
           lvl = 2,
           verbose = FALSE,
           test.mode = FALSE,
           ...) {
    if (!missing(dates)) {
      startDate <- min(dates)
      endDate <- max(dates)
    }
    con <- connection$getApi("earthexplorer")
    if (con$api_key == ""&!test.mode) {
      con$loginEEApiKey(verbose = verbose)
    }
    attempts <- 5

    repeat{
      query <- ls_query(
        server = con$api_server,
        datasetName = product,
        startDate = startDate,
        endDate = endDate,
        sf.obj = st_transform(region, st_crs(4326)),
        ...
      )
      if(verbose) message(paste0("Landsat_query: ",query))

      jsonres <- con$postApiEE(query$url, query$json, con$api_key)
      if(test.mode){
        query<-paste0("https://unai-perez.github.io/rsat-test/",
                      "api-res-test/landsat-json-test.json")
        jsonres<-fromJSON(con$simpleCall(query))
        break
      }
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
      con$loginEEApiKey(verbose = verbose)
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
        api_name <- "earthexplorer"
        img.name <- unlist(res.df$entityId)
        nlen <- length(img.name)
        dataset.data <- con$getEEdatasetID(product, verbose = verbose)
        download_url <- paste0(con$server, "/download/",
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
        api_name <- "ESPA"
        img.name <- unlist(res.df$displayId)
        nlen <- length(img.name)
        download_url <- paste0(connection$getApi("ESPA")$api_server,
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
   if (tolower(product)%in%c("landsat_ot_c2_l2",
                             "landsat_ot_c2_l1",
                             "lsr_landsat_8_c1",
                             "landsat_8_c1"
                             )) {
      sat <- "Landsat-8"
    } else if (tolower(product)%in%c("landsat_etm_c2_l2",
                                     "landsat_etm_c2_l1",
                                     "lsr_landsat_etm_c1",
                                     "landsat_etm_c1",
    )) {
      sat <- "Landsat-7"
    } else if(tolower(product)%in%c("landsat_tm_c2_l2",
                                    "landsat_tm_c2_l1",
                                    "landsat_mss_c2_l1",
                                    "lsr_landsat_tm_c1",
                                    "landsat_tm_c1",
                                    "landsat_mss_c1")){
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
      api_name = rep(api_name, nlen),
      order = order,
      extent_crs = new("extent_crs",
        EPSG = rep(4326, nlen),
        xmin = bounds[, "LongitudeMin"],
        ymin = bounds[, "LatitudeMin"],
        xmax = bounds[, "LongitudeMax"],
        ymax = bounds[, "LatitudeMax"]
      )
    ))
  }
)
