#' Plots an rtoi object
#'
#' Plot (a map of) the values of an rtoi object.
#'
#' @param x an rtoi
#' @param y character argument. The product name to be plotted.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#' @param xsize the size of x dimension for subsetting.
#' @param ysize the size of y dimension for subsetting.
#' @param ... additional arguments.
#'
#' @return tmap plot.
#'
#' @import tmap
#' @importFrom sp proj4string proj4string<-
#' @include rtoi.R records.R
#' @export
setMethod(f="plot",
          signature = c("rtoi"),
          function(x, y, verbose = FALSE,xsize = 250,ysize = 250,...){
            # load the data
            dirs<-list.dirs(get_dir(x))
            mosaics.dir<-dirs[grepl("mosaic",dirs)]
            files<-list.files(mosaics.dir,full.names = TRUE,pattern=format(y,"%Y%j"))
            if(length(files)==0)stop("There is no image for provided date.")

            plot.list<-list()
            for(p in product(x)){
              debands<-deriveBandsData(p)
              if(!is.null(debands)){
                bands<-debands$bands
                files.p<-files[grepl(p,files)]
                if(length(files.p)>0){
                  files.p<-file.path("/vsizip",files.p,utils::unzip(files.p,list=TRUE)$Name)

                  rasterio<-list(nBufXSize = xsize, nBufYSize = ysize)

                  red<-read_stars(files.p[grepl(bands["red"],files.p,ignore.case = TRUE)][1],normalize_path = FALSE,RasterIO =rasterio)
                  green<-read_stars(files.p[grepl(bands["green"],files.p,ignore.case = TRUE)][1],normalize_path = FALSE,RasterIO =rasterio)
                  blue<-read_stars(files.p[grepl(bands["blue"],files.p,ignore.case = TRUE)][1],normalize_path = FALSE,RasterIO =rasterio)
                  img<-stack(stretch(as(red,"Raster"), minv = 0, maxv = 255),
                             stretch(as(green,"Raster"), minv = 0, maxv = 255),
                             stretch(as(blue,"Raster"), minv = 0, maxv = 255))
                  proj4string(img)<-proj4string(raster(files.p[grepl(bands["red"],files.p,ignore.case = TRUE)][1]))
                  plot.list<-c(plot.list,img)
                }
              }
            }

            # plot
            genPlotGIS(r=plot.list,region(x),...)
          }
)

#' @rdname plot-rtoi-ANY-method
#' @aliases plot,records
setMethod(f="plot",
          signature = c("records"),
          function(x, y, verbose = FALSE,...){
            # load the data
            if(length(x)>0){
              if(length(x)>5)message("Records plot takes a while downloading preview images.")
              img.list<-list()
              lname<-c()
              for(i in 1:length(x)){
                r<-x[i]
                p.url<-get_preview(r)
                na<-names(r)
                pre_dir<-file.path(tmpDir(),get_dir(r),"rgt_preview")
                dir.create(pre_dir,showWarnings = FALSE,recursive = TRUE)
                pre.file<-file.path(pre_dir,names(r))
                if(verbose)message(paste0("Preview file download file: ",pre.file))
                if(!file.exists(pre.file)){
                  con<-connection$getApi(get_api_name(r))
                  con$pictureDownload(p.url,pre.file)
                }
                img<-raster::stack(pre.file)
                extent(img)<-extent(r)
                projection(img)<-st_crs(crs(r))$proj4string
                img.list<-c(img.list,img)
                lname<-c(lname,paste0(sat_name(r),"_",dates(r)))
              }

              # plot
              genPlotGIS(r=img.list,...)
            }else{
              message("Empty records.")
            }
          }
)

#' @rdname plot-rtoi-ANY-method
#' @aliases plot,rtoi
setMethod(f="plot",
          signature = c("rtoi"),
          function(x, y, verbose = FALSE,xsize = 250,ysize = 250,...){
            # load the data
            dirs<-list.dirs(get_dir(x))
            mosaics.dir<-dirs[grepl("mosaic",dirs)]
            files<-list.files(mosaics.dir,full.names = TRUE,pattern=format(y,"%Y%j"))
            if(length(files)==0)stop("There is no image for provided date.")

            plot.list<-list()
            for(p in product(x)){
              debands<-deriveBandsData(p)
              if(!is.null(debands)){
                bands<-debands$bands
                files.p<-files[grepl(p,files)]
                if(length(files.p)>0){
                  files.p<-file.path("/vsizip",files.p,utils::unzip(files.p,list=TRUE)$Name)

                  rasterio<-list(nBufXSize = xsize, nBufYSize = ysize)

                  red<-read_stars(files.p[grepl(bands["red"],files.p,ignore.case = TRUE)][1],normalize_path = FALSE,RasterIO =rasterio)
                  green<-read_stars(files.p[grepl(bands["green"],files.p,ignore.case = TRUE)][1],normalize_path = FALSE,RasterIO =rasterio)
                  blue<-read_stars(files.p[grepl(bands["blue"],files.p,ignore.case = TRUE)][1],normalize_path = FALSE,RasterIO =rasterio)
                  img<-stack(stretch(as(red,"Raster"), minv = 0, maxv = 255),
                             stretch(as(green,"Raster"), minv = 0, maxv = 255),
                             stretch(as(blue,"Raster"), minv = 0, maxv = 255))
                  proj4string(img)<-proj4string(raster(files.p[grepl(bands["red"],files.p,ignore.case = TRUE)][1]))
                  plot.list<-c(plot.list,img)
                }
              }
            }

            # plot
            genPlotGIS(r=plot.list,region(x),...)
          }
)

#' @rdname plot-rtoi-ANY-method
#' @aliases plot,rtoi,missing
setMethod(f="plot",
          signature = c("rtoi","missing"),
          function(x, y, verbose = FALSE,...){
            # load the data
            y<-dates(x)[1]
            plot(x,y,verbose,...)

})

genPlotGIS<-function(r,region,breaks,labels,zlim,layout,proj,nbreaks=40,nlabels=10,as.grid=TRUE,compass.rm=FALSE,scale.bar.rm=FALSE,...){
  args<-list(...)

  # r and region projection management
  if(class(r)=="list"){
    if(class(r[[1]])=="RasterBrick"|class(r[[1]])=="RasterStack"){
      if(!missing(proj)){
        r = lapply(r, projectRaster,crs=proj)
        if(!missing(region)){region=transform_multiple_proj(region,proj4=projection(r[[1]]))}
      }
    }else{
      stop("genPlotGIS only supports RasterBrick or RasterStack, or a list composed by RasterBrick or RasterStack.")
    }
  }else{
    if(class(r)=="RasterBrick"|class(r)=="RasterStack"|class(r)=="RasterLayer"){
      if(!missing(proj)){
        r = projectRaster(r,crs=proj)
        if(!missing(region)){region=transform_multiple_proj(region,proj4=projection(r))}
      }
    }else{
      stop("genPlotGIS only supports RasterBrick or RasterStack, or a list composed by RasterBrick or RasterStack.")
    }
  }


  # layout preconfigured arguments
  tm_layout_args<-args[names(args)%in%names(formals(tm_layout))]
  if(!("legend.bg.color" %in% names(tm_layout_args))){
    tm_layout_args$legend.bg.color="white"
  }
  if(!("panel.show" %in% names(tm_layout_args))){
    tm_layout_args$panel.show=TRUE
  }
  if(!("panel.labels" %in% names(tm_layout_args))){
    tm_layout_args$panel.labels=names(r)
  }
  if(!("legend.outside" %in% names(tm_layout_args))){
    tm_layout_args$legend.outside=TRUE
  }
  if(!("legend.outside.size" %in% names(tm_layout_args))){
    tm_layout_args$legend.outside.size=0.08
  }
  if(!("legend.outside.position" %in% names(tm_layout_args))){
    tm_layout_args$legend.outside.position="right"
  }
  if(!("frame" %in% names(tm_layout_args))){
    tm_layout_args$frame=TRUE
  }

  if(!missing(layout)){
    lyt<-tm_facets(ncol=layout[2],nrow = layout[1])
  }else{
    lyt<-NULL
  }

  graticules_args<-c(names(formals(tm_graticules)),names(formals(tm_grid)))
  names(graticules_args)<-paste0("tm.graticules.",graticules_args)
  tm_graticules_args<-args[names(args)%in%names(graticules_args)]
  names(tm_graticules_args)<-graticules_args[names(tm_graticules_args)]
  if(!("lines" %in% names(tm_graticules_args))){
    tm_graticules_args$lines=FALSE
  }
  if(as.grid){
    tm_layout_args$between.margin=-.1
    if(!("labels.space.x" %in% names(tm_layout_args))){
      tm_graticules_args$labels.space.x=.10
    }
    if(!("labels.space.y" %in% names(tm_layout_args))){
      tm_graticules_args$labels.space.y=.10
    }
  }
  grid<-do.call(tm_graticules,tm_graticules_args)

  #compass arguments and preconfigured assignation
  if(!compass.rm){
    compass_args<-names(formals(tm_compass))
    names(compass_args)<-paste0("tm.compass.",compass_args)
    tm_compass_args<-args[names(args)%in%names(compass_args)]
    names(tm_compass_args)<-compass_args[names(tm_compass_args)]
    if(!("type" %in% names(tm_compass_args))){
      tm_compass_args$type="arrow"
    }
    if(!("position" %in% names(tm_compass_args))){
      tm_compass_args$position=c("right", "top")
    }
    if(!("size" %in% names(tm_compass_args))){
      tm_compass_args$size=2
    }
    if(!("show.labels" %in% names(tm_compass_args))){
      tm_compass_args$show.labels=0
    }
    compass<-do.call(tm_compass,tm_compass_args)
  }else{
    compass<-NULL
  }


  #scale bar arguments and preconfigured assignation
  if(!scale.bar.rm){
    scale_bar_args<-names(formals(tm_scale_bar))
    names(scale_bar_args)<-paste0("tm.scale.bar.",scale_bar_args)
    tm_scale_bar_args<-args[names(args)%in%names(scale_bar_args)]
    names(tm_scale_bar_args)<-scale_bar_args[names(tm_scale_bar_args)]
    if(!("position" %in% names(tm_scale_bar_args))){
      tm_scale_bar_args$position=c("left", "bottom")
    }
    if(!(any(c("text.size","size") %in% names(tm_scale_bar_args)))){
      tm_scale_bar_args$text.size=0.8
    }
    scale.bar<-do.call(tm_scale_bar,tm_scale_bar_args)
  }else{
    scale.bar<-NULL
  }

  if(!missing(region)){
    # region default arguments
    shape_region_args<-names(formals(tm_shape))
    shape_region_args<-shape_region_args[!(shape_region_args%in%"...")]
    names(shape_region_args)<-paste0("tm.shape.region.",shape_region_args)
    tm_shape_region_args<-args[names(args)%in%names(shape_region_args)]
    names(tm_shape_region_args)<-shape_region_args[names(tm_shape_region_args)]
    tm_shape_region_args$shp=region


    polygon_region_args<-c(names(formals(tm_polygons)),names(formals(tm_fill)),names(formals(tm_borders)))
    polygon_region_args<-unique(polygon_region_args[!(polygon_region_args%in%"...")])
    names(polygon_region_args)<-paste0("tm.polygon.region.",polygon_region_args)
    tm_polygon_region_args<-args[names(args)%in%names(polygon_region_args)]
    names(tm_polygon_region_args)<-polygon_region_args[names(tm_polygon_region_args)]
    if(!("alpha" %in% names(tm_polygon_region_args))){
      tm_polygon_region_args$alpha=0
    }
    if(!("lwd" %in% names(tm_polygon_region_args))){
      tm_polygon_region_args$lwd=1
    }

    reg<-do.call(tm_shape,tm_shape_region_args) + do.call(tm_polygons,tm_polygon_region_args)

  }else{
    reg<-NULL
  }

  if(class(r)=="list"){
    ####################################################
    # RGB plot
    ####################################################
    maplist<-lapply(r,function(shp,compass,scale.bar,grid,reg){return(tm_shape(shp=shp,frame=T)+tm_rgb()+compass+scale.bar+grid+reg)},compass,scale.bar,grid,reg)
    #tmap_arrange arguments
    tmap_arrange_args<-names(formals(tmap_arrange))
    tmap_arrange_args<-unique(tmap_arrange_args[!(tmap_arrange_args%in%"...")])
    names(tmap_arrange_args)<-paste0("tmap.arrange.",tmap_arrange_args)
    tm_tmap_arrange_args<-args[names(args)%in%names(tmap_arrange_args)]
    names(tm_tmap_arrange_args)<-tmap_arrange_args[names(tm_tmap_arrange_args)]

    if(!("asp" %in% tm_tmap_arrange_args)){
      tm_tmap_arrange_args$asp=NA
    }

    if(missing(layout)){
      if(length(r)>1){
        tm_tmap_arrange_args$ncol=ceiling(sqrt(length(r)))
      }else{
        return(tm_shape(shp=r[[1]],frame=T)+tm_rgb()+compass+scale.bar+grid+reg)
      }

    }else{
      tm_tmap_arrange_args$nrow=layout[1]
      tm_tmap_arrange_args$ncol=layout[2]
    }

    return(do.call(tmap_arrange,c(maplist,tm_tmap_arrange_args)))
  }

  ####################################################
  # Stack plot
  ####################################################
  # default label and breaks for the raster
  if(missing(zlim)){
    lower<-min(minValue(r))
    upper<-max(maxValue(r))
  }else{
    if((class(zlim)!="numeric")&(length(zlim)!=0))
      stop("zlim must be a vector of length 2 specifying the upper and lower boundaries of the legend.")
    lower<-min(zlim)
    upper<-max(zlim)
  }


  nbreaks=nbreaks-2
  if(missing(breaks))
    breaks<-c(-Inf,seq(from=lower,to=upper,by=((upper-lower)/nbreaks)),Inf)
  if(missing(labels)){
    labels<-c("",as.character(round(breaks[-c(1,length(breaks))],digits = 2)))
    if(length(labels)>nlabels){
      labels<-rep("",length(labels))
      labels[c(seq(1,length(labels),as.integer(length(labels)/nlabels)),length(labels))]<-as.character(round(seq(from=lower,to=upper,by=((upper-lower)/nlabels)),digits = 2))
    }
  }

  # raster default arguments
  shape_r_args<-names(formals(tm_shape))
  shape_r_args<-shape_r_args[!(shape_r_args%in%c("..."))]
  names(shape_r_args)<-paste0("tm.shape.r.",shape_r_args)
  tm_shape_r_args<-args[names(args)%in%names(shape_r_args)]
  names(tm_shape_r_args)<-shape_r_args[names(tm_shape_r_args)]
  tm_shape_r_args$shp=r

  raster_r_args<-names(formals(tm_raster))
  names(raster_r_args)<-paste0("tm.raster.r.",raster_r_args)
  tm_raster_r_args<-args[names(args)%in%names(raster_r_args)]
  names(tm_raster_r_args)<-raster_r_args[names(tm_raster_r_args)]
  if(!("col" %in% names(tm_raster_r_args))){
    tm_raster_r_args$col=names(r)
  }
  if(!("breaks" %in% names(tm_raster_r_args))){
    tm_raster_r_args$breaks=breaks
  }
  if(!("labels" %in% names(tm_raster_r_args))){
    tm_raster_r_args$labels=labels
  }
  if(!("legend.reverse" %in% names(tm_raster_r_args))){
    tm_raster_r_args$legend.reverse=TRUE
  }
  if(!("title" %in% names(tm_raster_r_args))){
    tm_raster_r_args$title=""
  }

  # Base tmap
  return(do.call(tm_shape,tm_shape_r_args) + do.call(tm_raster,tm_raster_r_args) +# tm_facets(nrow=3,ncol=2)+# raster conf
           do.call(tm_layout,tm_layout_args) +# layout
           compass + #the compass
           reg+ #region
           scale.bar+#scale
           grid+
           lyt)
}
