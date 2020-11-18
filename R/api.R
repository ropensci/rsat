#' @import curl
#' @importFrom httr POST content_type content authenticate
#' @importFrom xml2 read_html xml_attr
#' @importFrom rvest html_nodes

setGeneric("api",function(api_name,server,api_server,credentials,request,...){standardGeneric("api")})
setRefClass(Class="api",
  fields = list(
    api_name = "character",
    server = "character",
    api_server = "character",
    username = "character",
    password = "character",
    request = "character",
    api_key = "character",
    credentials = "character",
    status = "character",
    order.list = "list"),
  methods = list(
    initialize=function(){
      .self$username=""
      .self$password=""
      .self$request="rsat request"
      .self$api_key=""
      .self$credentials=""
      .self$status="Not checked"
      .self$order.list=list("order"=c(),"status"=c(),"id"=c())
    },
    getCredentials =function(){
      return(c(username=.self$username,password=.self$password))
    },
    simpleCall = function(url){
      c.handle = new_handle()
      req <- curl(url, handle = c.handle)
      html<-suppressWarnings(readLines(req))
      html<-paste(html,collapse = "\n ")
      close(req)
      if(grepl("Internal Server Error", html)){
        .self$status="Offline"
        stop(paste("Error:",.self$api_server,"web out of service"))
      }
      return(html)
    },
    autoCall = function(url){
      c.handle = new_handle()
      handle_setopt(c.handle,
                    referer=.self$server,
                    useragent = connection$useragent,
                    followlocation = TRUE ,
                    autoreferer = TRUE)
      con<-curl(url,
                handle =c.handle)
      html<-suppressWarnings(readLines(con))
      close(con)
      return(html)
    },
    secureHandle=function(){
      c.handle = new_handle()
      if(.self$username==""|.self$password==""){stop("Check your credentials.")}
      handle_setopt(c.handle,
                    referer=.self$server,
                    useragent = connection$useragent,
                    followlocation = TRUE ,
                    autoreferer = TRUE,
                    username=.self$username,
                    password=.self$password)
      return(c.handle)
    },
    secureCall = function(url){
      c.handle<-.self$secureHandle()
      con=curl(url,handle =c.handle)
      tryCatch({
        html<-suppressWarnings(readLines(con))
      }, error = function(e) {
        close(con)
        if(grepl("HTTP error 503.",e$message)){
          .self$status="Service on maintenace."
          stop("Service on maintenace. HTTP error 503.")
        }else if(grepl("HTTP error 401.",e$message)){
          stop("Unauthorized error (HTTP error 401). Check your credentials.")
        }
        stop(e)
      })
      close(con)
      return(html)
    },
    scihubIsLTA = function(download.url){
      order<-c()
      for(d in download.url){
        html<-.self$secureCall(gsub("/$value","",d,fixed = TRUE))
        html<-paste(html,collapse = "\n ")
        if(gsub(".*d:Online>","",gsub("</d:Online.*","",html))=="false"){
          order=c(order,TRUE)
        }else{
          order=c(order,FALSE)
        }
      }
      order
    },
    secureDownload = function(url,f){
      if(.self$api_name=="earthexplorer"){
          c.handle<-.self$secureHandle()
          con=curl("https://ers.cr.usgs.gov/login/",handle =c.handle)
          html<-suppressWarnings(readLines(con))
          html<-paste(html,collapse = "\n ")
          html<-read_html(html)
          csrf<-html %>% html_nodes(xpath = '//*[@name="csrf_token"]') %>% xml_attr("value")
          if(grepl("ncforminfo",html)){
            nc<-html %>% html_nodes(xpath = '//*[@name="__ncforminfo"]') %>% xml_attr("value")
            handle_setform(c.handle,
                           'username' = username,
                           'password' = password,
                           "csrf_token"=csrf,
                           "__ncforminfo"=nc
            )
          }else{
            handle_setform(c.handle,
                           'username' = username,
                           'password' = password,
                           "csrf_token"=csrf)
          }
          req <- curl_fetch_memory("https://ers.cr.usgs.gov/login/", handle = c.handle)
          # if(verbose){
          #   message(paste(parse_headers(req$headers),collapse="\n"))
          # }
      }else{
        c.handle<-.self$secureHandle()
      }

      if(.self$api_name=="scihub"){
        online<-gsub("/$value","/Online/$value",url,fixed = T)
        is.online<-curl_fetch_memory(online,handle=c.handle)
        if(rawToChar(is.online$content)=="false"){
          message("The image is archived, ordering...")
          order<-curl_fetch_memory(url,handle=c.handle)
          while(order$status_code!=202){
            Sys.sleep(10)
            if(order$status_code==503){
              message("Service Unavailable. The retrieval of offline data is temporarily unavailable, please try again later")
            }else if(order$status_code==403){
              message("Forbidden. User offline products retrieval quota exceeded")
            }else if(order$status_code==500){
              message("Internal Server Error. Unexpected nav segment Navigation Property")
            }
            order<-curl_fetch_memory(url,handle=c.handle)
          }
          message("Image ordered!")
          is.online<-curl_fetch_memory(online,handle=c.handle)
          while(rawToChar(is.online$content)=="false"){
            Sys.sleep(10)
            is.online<-curl_fetch_memory(online,handle=c.handle)
          }
          curl_download(url, destfile=f,handle = c.handle)

        }else{
          curl_download(url, destfile=f,handle = c.handle)
        }
      }else{
          curl_download(url, destfile=f,handle = c.handle)
      }
    },
    pictureDownload=function(pic.url,destfile){
      c.handle<-.self$secureHandle()
      curl_download(pic.url, destfile=destfile,handle = c.handle)
    },
    ###############################################################
    # ESPA Connections
    ###############################################################
    espaOrderImage=function(img_name,product="sr",verbose=FALSE){#c("sr","source_metadata")
      if(length(img_name)>1)stop("Only one image is supported for each ESPA order.")
      .self$espaGetOrders(verbose)
      c.handle<-.self$secureHandle()
      if(!img_name%in%.self$order.list$id ){
        url.products = paste0(.self$api_server,'/available-products/', img_name)
        json_data <- rjson::fromJSON(paste(.self$secureCall(url.products), collapse=""))
        #if(verbose){message(paste0("ESPA response r obj: \n",json_data))}
        json_data2<-unlist(json_data,recursive=TRUE)
        products<-json_data2[grepl("products",names(json_data2))]
        if(length(products)==0){
          warning(paste0("Defined products are not available for image ",ids))
          warning(paste0("Products ",paste(json_data2,collapse = ", ")))
          next
        }
        if(any(!(product%in%products))){
          product<-product[product%in%products]
          if(length(product)==0){
            warning(paste0("Defined products are not available for image ",ids))
            warning(paste0("Products ",paste(json_data2,collapse = ", ")))
            next
          }
        }

        #create the query
        json_data[[1]]$products<-product#c("sr","source_metadata")#product
        json_post<-list(projection=list(lonlat=NA),
                        format="gtiff",
                        resampling_method="cc",
                        note=.self$request)
        json_post<-append(json_post,json_data)
        query<-toEspaJSON(json_post)

        #if(verbose){message(paste0("ESPA query: \n",query))}
        res = POST(paste0(.self$api_server,"/order"),
                   authenticate(.self$username, .self$password),
                   body = as.character(query))
        if(verbose){
          message("Order response:")
          print(res$status)
        }
        message(paste0(img_name," image ordered!"))
        #if(verbose){message(paste0("ESPA Order: \n",res))}
      }else{
        message(paste0("Alredy ordered image. Name: ",img_name))
      }
    },
    espaGetOrders=function(verbose=FALSE){
      .self$espaUpdateOrderStatus()
      c.handle<-.self$secureHandle()
      r <- curl_fetch_memory(paste0(.self$api_server,"/list-orders"), c.handle)
      newOrders<-fromJSON(rawToChar(r$content))
      dates<-as.Date(gsub(".*\\s*(\\d{8}).*","\\1",newOrders),"%m%d%Y")
      newOrders<-newOrders[Sys.Date()-dates<8]
      if(length(newOrders)==0)return(message("There are no ordered images."))

      newOrders<-newOrders[!(newOrders%in%.self$order.list$order)]
      if(length(newOrders)>0){
        for(o in newOrders){
          r <- curl_fetch_memory(paste0(.self$api_server,"/order/",o), c.handle)
          json_data<-fromJSON(rawToChar(r$content))
          if(verbose){
            print(json_data$status)
          }
          if(json_data$note==.self$request & tolower(json_data$status)%in%c("complete","processing","oncache","tasked","ordered","submitted")){
            all.response<-unlist(json_data,recursive=TRUE)
            .self$order.list$order<-c(.self$order.list$order,o)
            .self$order.list$status<-c(.self$order.list$status,json_data$status)
            .self$order.list$id<-c(.self$order.list$id,all.response[grepl("inputs",names(all.response))])
          }
        }
      }
    },
    espaUpdateOrderStatus=function(){
      norder<-length(.self$order.list$order)
      if(norder>0){
        c.handle<-.self$secureHandle()
        for(o in 1:norder){
          r <- curl_fetch_memory(paste0(.self$api_server,"/order/",.self$order.list$order[o]), c.handle)
          json_data<-fromJSON(rawToChar(r$content))
          if(!(length(json_data$note)==0)&&json_data$note==.self$request){
            all.response<-unlist(json_data,recursive=TRUE)
            .self$order.list$status[o]<-json_data$status
          }
        }
      }
    },
    espaDownloadsOrders=function(tile_name,out.file,verbose=FALSE){
      c.handle<-.self$secureHandle()
      order_name<-.self$order.list$order[.self$order.list$id%in%tile_name]
      if(is.na(order_name[1])){
        if(verbose) message(paste0(tile_name," image not ordered, cannot be downloaded."))
        return(TRUE)
      }
      r <- curl_fetch_memory(paste0(.self$api_server,"/item-status/",order_name[1]), c.handle)
      json_data<-unlist(fromJSON(rawToChar(r$content)),recursive=TRUE)
      o.status<-json_data[grepl("status",names(json_data))]
      if(verbose){
        message(paste0(tile_name," order status: ",o.status))
      }


      if(tolower(o.status)=="complete"){
        message(paste0("Downloading ",tile_name," image."))
        durl<-json_data[grepl("product_dload_url",names(json_data))]
        curl_download(url=durl,destfile=out.file,handle =c.handle)
        md5.url<-unlist(json_data,recursive=TRUE)
        md5.url<-md5.url[grepl("cksum_download_url",names(md5.url))]
        rmd5 <- curl_fetch_memory(md5.url,
                                  c.handle)
        md.file<-unlist(strsplit(rawToChar(rmd5$content)," "))
        if(genCheckMD5(out.file,toupper(md.file[1]))){
          return(TRUE)
        }else{
          message(paste0("ERROR CHECKING MD5 OF ",tile_name," IMAGE, TRYING THE DOWNLOAD PROCESS AGAIN."))
          file.remove(out.file)
          return(FALSE)
        }
      }else if(tolower(o.status)=="processing"|tolower(o.status)=="oncache"|tolower(o.status)=="tasked"|tolower(o.status)=="submitted"){
          return(FALSE)
      }else if(tolower(o.status)=="unavailable"){
          message(paste0(tile_name," image unavailable, try again later."))
          return(TRUE)
      }else{
        if(verbose){
          message(paste0("Check order status: ", tolower(o.status)))
          message(paste0("Unknown download error with ",tile_name," image, omitting this download."))
        }
        return(TRUE)
      }
    },
    ###############################################################
    # Login EarthExplorer API
    ###############################################################
    getApiKey = function(){
      if(.self$api_key==""){
        .self$loginEEApiKey()
      }
      .self$api_key
    },
    loginEEApiKey = function(){
      jsonquery<-list("username"=.self$username,
                      "password"=.self$password,
                      "authType"="EROS",
                      "catalogId"="EE")
      post.res <- POST(url = paste0(.self$api_server, "/login"),
                       body = paste0(toJSON(jsonquery)),
                       content_type("application/x-www-form-urlencoded; charset=UTF-8"))
      res <- content(post.res)
      if(!is.null(res$errorCode)){
        stop(res$errorMessage)
      }
      message('Logged into EE API.')
      .self$api_key=res$data
    },
    postApiEE = function(url,body,key){
      names(key)<-"X-Auth-Token"
      post.res <- POST(url = url,
                       body = body,
                       content_type("application/json"),
                       add_headers(key))
      if(post.res$status_code==200)return(content(post.res))
      return(list(errorCode=paste0("Error in Earth Explorer api connection. HTTP ",post.res$status_code,".")))
    },
    postdownload = function(){
      #TODO
      #https://dds.cr.usgs.gov/download/eyJpZCI6MTA5ODU3NywiY29udGFjdElkIjoxNTM4NTIxfQ==/
      url<-"https://m2m.cr.usgs.gov/api/api/json/stable/download-request"
      key<-"eyJjaWQiOjE1Mzg1MjEsInMiOiIxNTk2NjU2NTE0IiwiciI6NTYzLCJwIjpbXX0="
      names(key)<-"X-Auth-Token"
      body<-'{\"datasetName\":\"LANDSAT_8_C1\",\"sceneFilter\":{\"acquisitionFilter\":{\"start\":\"2018-07-01\",\"end\":\"2018-07-31\"},\"spatialFilter\":{\"filterType\":\"mbr\",\"lowerLeft\":{\"latitude\":41.9095732069108,\"longitude\":-2.49908963576402},\"upperRight\":{\"latitude\":43.3146327061922,\"longitude\":-0.726158447400539}}},\"maxResults\":\"50000\",\"startingNumber\":\"1\",\"sortOrder\":\"ASC\"}'
      #https://earthexplorer.usgs.gov/download/5e83d0b84df8d8c2/LC81990312020204LGN00/EE/
      #https://earthexplorer.usgs.gov/download/5e83d0b84df8d8c2/LC82000302019208LGN00/EE/
      post.res <- POST(url = url,
                       body = body,
                       content_type("application/json"),
                       add_headers(key),
                       authenticate(user="user",#change this
                                    password="pass",
                                    type = "basic"))

      content(post.res)
    },
    logoutEEAPI=function(){
      jsonquery<-list("apikey"=.self$api_key)
      if(!is.null(jsonquery$apikey)){
        post.res <- POST(url = paste0(.self$api_server, "/logout"),
                         body = URLencode(paste0('jsonRequest=',toJSON(jsonquery))),
                         content_type("application/x-www-form-urlencoded; charset=UTF-8"))
        res <- content(post.res)
        if(res$error!=""){
          message('Logged out from EE API.')
          .self$api_key=NULL
        }else{
          message('You are not logged in EE API.')
          stop(res$error)
        }
      }else{
        message('You are not logged in EE API.')
      }
    },
    ######################################################################
    # Generics
    #####################################################################
    getServer =function(){
      return(.self$api_server)
    },
    checkCredentials = function(){
      if(length(.self$username)==0){
        stop("Username must be defined.")
      }
      if(length(.self$username)==0){
        stop("Username must be defined.")
      }
    },
    request = function(request){
      .self$request=request
    }
  )
)


setMethod("api",
          signature("character","character","character","character","missing"),
          function(api_name,server,api_server,credentials) {
            api=new("api")
            api$api_name=api_name
            api$server=server
            api$api_server=api_server
            api$credentials=credentials
            api
          })
setMethod("api",
          signature("character","character","character","character","character"),
          function(api_name,server,api_server,credentials,request) {
            api=new("api")
            api$api_name=api_name
            api$server=server
            api$api_server=api_server
            api$credentials=credentials
            api$request=request
            api
          })

#' @rdname print-rtoi-method
#' @aliases print,api
setMethod("print",
          signature(x = "api"),
          function(x){
            cat("Api Name: ",x$api_Name,"\n","Api Server: ",x$api_server)
          })





