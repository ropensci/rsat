#' @import  leaflet leafem
createMap <- function(){
  leaflet()%>%
    addProviderTiles("OpenStreetMap", group="OpenStreetMap",options = providerTileOptions(zIndex = 0))%>%
    addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery",options = providerTileOptions(zIndex = 0))%>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron",options = providerTileOptions(zIndex = 0))%>%
    addProviderTiles("CartoDB.DarkMatter", group ="CartoDB.DarkMatter",options = providerTileOptions(zIndex = 0))%>%
    addLayersControl(baseGroups=c("OpenStreetMap",
                                  "Esri.WorldImagery",
                                  "CartoDB.Positron",
                                  "CartoDB.DarkMatter"),
                     position="topleft",
                     options = layersControlOptions(autoZIndex = FALSE))
}

updateLayersControl<-function(map,addBaseGroups=NULL,addOverlayGroups=NULL){
  for(i in 1:length(map$x$calls)){
    if(map$x$calls[[i]]$method=="addLayersControl"){
      map$x$calls[[i]][[2]][[1]]<-c(map$x$calls[[i]][[2]][[1]],addBaseGroups)
      map$x$calls[[i]][[2]][[2]]<-c(map$x$calls[[i]][[2]][[2]],addOverlayGroups)
    }
  }
  return(map)
}

addMapRasterRGB<-function(img,lname,lpos,project=project){
    nmap<-getPreviewMap()%>%
      #addRasterRGB(subset(img,lpos), group=lname) #%>%
      addRasterRGB(st_as_stars(subset(img,lpos)), group=lname,project=project)
      #addHomeButton(st_bbox(transform_multiple_proj(img, proj4=st_crs(4326))), group = lname, position = "bottomright", add = TRUE)
      #updateLayersControl(addOverlayGroups = lname)
    setPreviewMap(updateLayersControl(nmap,addOverlayGroups = lname))
}

addMapFeature<-function(sfobj,lname){
  nmap<-getPreviewMap()
    if(!any(unlist(lapply(nmap$x$calls,function(x){
      if(x$method=="addPolygons"){
        if(x$args[[3]]==lname){return(TRUE)}}
      return(FALSE)
    }),recursive=TRUE))){
      nmap<-nmap%>%
              addFeatures(sfobj, group=lname, label=lname,color = "#3f8dfc" , fillOpacity = 0.2)%>%
              addHomeButton(st_bbox(transform_multiple_proj(sfobj, proj4=st_crs(4326))), group = lname, position = "bottomright", add = TRUE)
      setPreviewMap(nmap)
    }
}

previewmap <- new.env()
assign("map", createMap(), env=previewmap)

getPreviewMap = function(){
  get("map", envir=previewmap)
}
setPreviewMap = function(map){
  assign("map", map, envir=previewmap)
}

#' Preview a \code{records} or an \code{rtoi} object
#'
#' @param x a \code{records} or an \code{rtoi} object.
#' @param lpos vector argument. Defines the position of the red-green-blue
#' layers to enable a false color visualization.
#' @param add.layer logical argument. If \code{TRUE}, the function plots the
#' image on an existing map.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @param tmp_dir character argument. The directory where preview images
#' are located.
#' @param get.map logical argument. If \code{TRUE}, the function return the leaflet map.
#' @param n the date expressed as the temporal index in the time series.
#' @param ... additional arguments
#'
#' @include rtoi.R records.R
#' @export
#' @import leafem
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
#'
#' sat_search(region=navarre,
#'           product="S2MSI1C",
#'           dates=as.Date("2018-01-01")+seq(1,30,1))
#'
#' preview(navarre)
#'
#'
#' sat_search(region=ex.navarre,
#'           product="S2MSI1C",
#'           dates=as.Date("2018-01-01")+seq(1,30,1))
#'
#' preview(navarre,n=1)
#' }
#' @export
setGeneric("preview", function(x,n,...) {standardGeneric("preview")})
#' @rdname preview
#' @aliases preview,rtoi,date
setMethod(f="preview",
          signature = c("rtoi","Date"),
          function(x,n,lpos=c(3,2,1),add.layer=FALSE,verbose = FALSE,...){
            #plot the rgb files
            preview(records(x),n,lpos,tmp_dir=get_database(x),add.layer,verbose,get.map=FALSE,...)
            #plot sf
            addMapFeature(region(x),names(x))
            return(getPreviewMap())
          }
)

#' @rdname preview
#' @aliases preview,rtoi,missing
setMethod(f="preview",
          signature = c("rtoi","missing"),
          function(x,n,lpos=c(3,2,1),add.layer=FALSE,verbose = FALSE,...){
            #plot the rgb files
            preview(records(x),dates(x)[1],lpos,tmp_dir=get_database(x),add.layer,verbose,get.map=FALSE,...)
            #plot sf
            addMapFeature(region(x),names(x))
            return(getPreviewMap())
          }
)
#' @rdname preview
#' @aliases preview,records,date
setMethod(f="preview",
          signature = c("records","Date"),
          function(x,n,lpos=c(3,2,1),tmp_dir=file.path(tempdir()),add.layer=FALSE,verbose = FALSE,get.map=TRUE,...){
              if(!add.layer)setPreviewMap(createMap())
              r<-x[dates(x)%in%n]
              for(n in 1:length(r)){
                preview(r,n,lpos,tmp_dir,add.Layer=TRUE,verbose,get.map=FALSE)
              }
              if(get.map)return(getPreviewMap())
          }
)
#' @rdname preview
#' @aliases preview,rtoi,numeric
setMethod(f="preview",
          signature = c("records","numeric"),
          function(x,n,lpos=c(3,2,1),tmp_dir=file.path(tempdir()),add.layer=FALSE,verbose = FALSE,get.map=TRUE,...){
            if(length(x)>=n&n>0){
              r<-x[n]
              dir.create(get_preview_path(r,tmp_dir),showWarnings = FALSE,recursive = TRUE)
              proj_file<-get_preview_proj(r,tmp_dir)
              if(!file.exists(proj_file)){
                download_preview(r,tmp_dir,verbose=verbose,...)
              }
              img<-raster::stack(proj_file)
              #lname<-paste0(sat_name(r),"_",dates(r))
              lname<-names(r)
              addMapRasterRGB(img,lname,lpos,project=FALSE)
              if(get.map)return(getPreviewMap())

            }else{
              if(verbose){
                if(length(x)==0){
                  message("No records for previewing.")
                }
                message("Cannot load new map.")
              }
              if(get.map)return(getPreviewMap())
            }
          }
)

get_preview_path<-function(r,tmp_dir){
  return(file.path(tmp_dir,get_dir(r),"rgt_preview"))
}
get_preview_file<-function(r,tmp_dir){
  return(file.path(get_preview_path(r,tmp_dir),names(r)))
}
get_preview_proj<-function(r,tmp_dir){
  return(paste0(get_preview_file(r,tmp_dir),"_proj"))
}

download_preview<-function(r,tmp_dir,verbose = FALSE,...){
    pre.file<-get_preview_file(r,tmp_dir)
    if(verbose)message(paste0("Preview file download file: ",pre.file))
    if(!file.exists(pre.file)){
      p.url<-get_preview(r)
      con<-connection$getApi(get_api_name(r))
      con$pictureDownload(p.url,pre.file)
    }
    img<-raster::stack(pre.file)
    # check if we have projection from search result
    if(any(is.na(as.vector(extent(r))))){
      #plotRGB(img)
      warning("Image without projection, cannot mosaick")
      return(NULL)
    }else{
      ext<-extent(r)
      extent(img)<-ext
      if(crs(r)==54008)
        projection(img)<-st_crs("ESRI:54008")$proj4string
      else
        projection(img)<-st_crs(crs(r))$proj4string
      tmp.img<-paste0(pre.file,"_tmp.tif")
      writeRaster(img,tmp.img,overwrite=TRUE)
      gdal_utils(util = "warp",
                 source = tmp.img,
                 destination = get_preview_proj(r,tmp_dir),
                 options=c("-t_srs",st_crs(4326)$proj4string)
      )
      rm(img)
      gc()
      file.remove(tmp.img)
    }
}
