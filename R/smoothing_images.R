#' Fill data gaps and smooth outliers in a time series of satellite images
#'
#' \code{apply_ima} is the implementation of a spatio-temporal method
#' called Interpolation of Mean Anomalies(IMA) for gap filling and smoothing
#' satellite data \insertCite{militino2019interpolation}{rsat}.
#' \code{smoothing_images} is the implementation of a spatio temporal method
#' called image mean anomaly (IMA) for gap filling and smoothing satellite
#' data \insertCite{militino2019interpolation}{rsat}.
#'
#' This filling/smoothing method was developed by
#' \insertCite{militino2019interpolation;textual}{rsat}. IMA fills the gaps
#' borrowing information from an adaptable temporal neighborhood. Two
#' parameters determine the size of the neighborhood; the number of days
#'  before and after the target image (\code{nDays}) and the number of previous
#' and subsequent years (\code{nYears}). Both parameters should be adjusted
#' based on the temporal resolution of the of the time-series of images. We
#' recommend that the neighborhood extends over days rather than years, when
#' there is little resemblance between seasons. Also, cloudy series may require
#' larger neighborhoods.
#'
#' IMA gives the following steps; (1) creates a representative image from the
#' temporal neighborhood of the target image (image to be filled/smoothed) e.g.,
#' doing the mean, median, etc. for each pixel's time-series (\code{fun}), (2)
#' the target and representative images are subtracted giving an image of
#' anomalies, (3) the anomalies falling outside the quantile limits
#' (\code{aFilter}) are considered outliers and therefore removed, (4) it
#' aggregates the anomaly image into a coarser resolution (\code{fact}) to
#' reveal potential spatial dependencies, (5) the procedure fits a spatial
#' model (thin plate splines or TPS) to the anomalies which is then used to
#' interpolate the values at the original resolution, and (6) the output
#' is the sum of the interpolated anomalies and the average image.
#'
#' @references \insertRef{militino2019interpolation}{rsat}
#'
#' @param x \code{rtoi}, \code{RasterStack} or \code{RasterBrick} containing
#' a time series of satellite images.
#' @param method character argument. Defines the method used for processing the images, e.a. "IMA".
#' @param product character argument. The name of the product to
#' be processed. Check the name of the parameter with \code{\link{list_data}}
#' function. Check the name of the parameter with \code{\link{list_data}} function. By default, "ALL".
#' @param satellite character argument. The name of the satellite to
#' be processed. Check the name of the parameter with \code{\link{list_data}} function. By default, "ALL".
#' @param stage character argument. The name of the processed stage
#' of the data. Check the name of the parameter with \code{\link{list_data}} function. By default, "ALL".
#' @param variable character argument.The name of the variable to
#' be processed. Check the name of the parameter with \code{\link{list_data}} function. By default, "ALL".
#' @param ... arguments for nested functions:
#' \itemize{
#'   \item \code{Img2Fill}  a \code{vector} defining the images to be filled/smoothed.
#'   \item \code{r.dates} a \code{vector} of dates for the layers in \code{x}.
#' Mandatory when layer names of \code{x} do not contain their capturing dates
#' "\code{YYYYJJJ}" format.
#'   \item \code{nDays} a \code{numeric} argument with the number of previous and
#' subsequent days of the temporal neighborhood.
#'   \item \code{nYears} a \code{numeric} argument with the number of previous and
#' subsequent years of the temporal neighborhood.
#'   \item \code{aFilter} a \code{vector} of lower and upper quantiles defining
#' the outliers in the anomalies. Ex. c(0.05,0.95).
#'   \item \code{fact} a \code{numeric} argument specifying the aggregation factor of
#' the anomalies.
#'   \item \code{fun} a \code{function} used to aggregate the image of anomalies. Both
#' \code{mean} (default) or \code{median} are accepted.
#'   \item \code{snow.mode} logical argument. If \code{TRUE}, the process is parallelized
#' using the functionalities from the `\code{raster}' package.
#'   \item \code{predictSE} calculate the standard error instead the prediction.
#'   \item \code{factSE} the \code{fact} used in the standard error prediction.
#'   \item \code{out.name} the name of the folder containing the smoothed/filled images
#' when saved in the Hard Disk Device (HDD).
#'   \item \code{only.na} logical argument. If \code{TRUE} only fills the \code{NA} values.
#' \code{FALSE}  by default.
#' }
#'
#' @return a \code{RasterStack} with the filled/smoothed images.
#'
#' @export
#' @import fields
#' @examples
#' \dontrun{
#' # load an example of NDVI time series in Navarre
#' data(ex.ndvi.navarre)
#'
#' # the raster stack with the date in julian format as name
#' spplot(ex.ndvi.navarre)
#'
#' # smoothin and fill all the time series
#' tiles.mod.ndvi.filled  <- smoothing_images(ex.ndvi.navarre,
#'                                            method="IMA",
#'                                            only.na=TRUE)
#' # show the filled images
#' spplot(tiles.mod.ndvi.filled)
#' # plot comparison of the cloud and the filled images
#' tiles.mod.ndvi.comp <- stack(ex.ndvi.navarre[[1]], tiles.mod.ndvi.filled[[1]],
#'                              ex.ndvi.navarre[[2]], tiles.mod.ndvi.filled[[2]])
#' spplot(tiles.mod.ndvi.comp, layout=c(2, 2))
#' }
setGeneric("smoothing_images", function(x,
                                        method,
                                        ...) {
  standardGeneric("smoothing_images")
})
#' @rdname smoothing_images
#' @aliases smoothing_images,rtoi,character
setMethod("smoothing_images",
          signature = c("rtoi","character"),
          function(x,
                   method,
                   product="ALL",
                   satellite="ALL",
                   stage="ALL",
                   variable="ALL",
                   ...){
            var_to_process<-list_data(x)
            if(!product=="ALL"){
              var_to_process<-var_to_process[var_to_process$product%in%product,]
            }else if(!satellite=="ALL"){
              var_to_process<-var_to_process[var_to_process$satellite%in%satellite,]
            }else if(!stage=="ALL"){
              var_to_process<-var_to_process[var_to_process$stage%in%stage,]
            }else if(!variable=="ALL"){
              var_to_process<-var_to_process[var_to_process$variable%in%variable,]
            }

            # remove imasmoothing
            var_to_process<-var_to_process[!var_to_process$stage%in%"ima_smoothing",]

            apply(var_to_process,1,function(p,rtoi_dir,process_folder="ima_smoothing",...){
              process_list<-read_rtoi_dir(p,rtoi_dir)
              rStack<-stack(process_list)
              names(rStack)<-format(genGetDates(process_list),"%Y%j")
              genSmoothingIMA(rStack,
                              AppRoot=file.path(rtoi_dir,p[1],p[2],process_folder),
                              out.name=p[4],
                              ...)
            },rtoi_dir=get_dir(x),...)

})
#' @rdname smoothing_images
setMethod("smoothing_images",
          signature = c("RasterBrick","character"),
          function(x,
                   method,
                   ...){
            if(method=="IMA"){
              return(genSmoothingIMA(rStack=x,get.stack=TRUE,...))
            }else{
              stop("Method not supported.")
            }
          })
#' @rdname smoothing_images
setMethod("smoothing_images",
          signature = c("RasterStack","character"),
          function(x,
                   method,
                   ...){
            if(method=="IMA"){
              return(genSmoothingIMA(rStack=x,get.stack=TRUE,...))
            }else{
              stop("Method not supported.")
            }
          })

genSmoothingIMA<-function(rStack,
                          Img2Fill = NULL,
                          nDays = 3,
                          nYears = 1,
                          fact = 5,
                          fun=mean,
                          r.dates,
                          aFilter = c(.05,.95),
                          only.na = FALSE,
                          factSE=8,
                          predictSE=FALSE,
                          snow.mode=FALSE,
                          out.name="outname",
                          get.stack=FALSE,
                          ...){
  args<-list(...)
  stime<-Sys.time()
  if(snow.mode){
    beginCluster()
  }
  if("AppRoot"%in%names(args)){
    dir.create(args$AppRoot,showWarnings = TRUE,recursive = TRUE)
  }
  # select images to predict
  if(is.null(Img2Fill)){
    Img2Fill<-1:nlayers(rStack)
  }else{
    aux<-Img2Fill[Img2Fill%in%1:nlayers(rStack)]
    if(is.null(aux)){stop("Target images in Img2Fill do not exist.")}
    if(length(aux)!=length(Img2Fill)){warning("Some of target images in Img2Fill do not exist in imgTS.")}
    Img2Fill<-aux
  }
  if(!missing(r.dates)){
    if(length(r.dates)!=nlayers(rStack))stop("r.dates and rStack must have the same length.")
    alldates<-r.dates
  }else{
    alldates<-genGetDates(names(rStack))
  }
  if(get.stack){result<-raster::stack()}
  if(all(is.na(alldates))){stop("The name of the layers has to include the date and it must be in julian days (%Y%j) .")}
  for(i in Img2Fill){
    # get target date
    target.date<-alldates[i]
    message(paste0("Predicting period ",target.date))

    # define temporal neighbourhood
    neighbours<-dateNeighbours(ts.raster=rStack,
                               target.date=target.date,
                               r.dates=alldates,
                               nPeriods=nDays,
                               nYears=nYears)
    message(paste0("   - Size of the neighbourhood: ",nlayers(neighbours)))
    # calculate mean image
    meanImage<-raster::calc(neighbours,fun=fun,na.rm=TRUE)
    # get target image
    targetImage<-raster::subset(rStack,which(format(genGetDates(names(rStack)),"%Y%j")%in%format(target.date,"%Y%j")))
    # calculate anomaly
    anomaly<-targetImage-meanImage
    # remove extreme values
    qrm<-raster::quantile(anomaly,aFilter)
    anomaly[anomaly<qrm[1]|anomaly>qrm[2]]<-NA
    # reduce the resolution for tps
    aggAnomaly<-raster::aggregate(anomaly, fact=fact,fun=fun)

    # Tps model
    xy <- data.frame(xyFromCell(aggAnomaly, 1:ncell(aggAnomaly)))
    v <- getValues(aggAnomaly)
    tps <- suppressWarnings(Tps(xy, v))

    # smooth anomaly
    if(snow.mode){
      if(!predictSE){
        anomaly.prediction <- clusterR(anomaly, raster::interpolate, args=list(model=tps,fun=predict))
        # add mean image to predicted anomaly
        target.prediction<-anomaly.prediction+meanImage
      }else{
        se.size<-raster::aggregate(anomaly, fact=factSE,fun=fun)
        target.prediction <- clusterR(se.size, raster::interpolate, args=list(model=tps,fun=fields::predictSE))
      }

    }else{
      if(!predictSE){
        anomaly.prediction <- raster::interpolate(object=anomaly, model=tps,fun=predict)
        # add mean image to predicted anomaly
        target.prediction<-anomaly.prediction+meanImage
      }else{
        se.size<-aggregate(anomaly, fact=factSE,fun=fun)
        target.prediction <- raster::interpolate(object=se.size, model=tps,fun=fields::predictSE)
      }
    }

    if(only.na){
      targetImage[is.na(targetImage)]<-target.prediction[is.na(targetImage)]
      target.prediction<-targetImage
    }
    # write filled images
    if("AppRoot"%in%names(args)){
      outfile<-paste0(args$AppRoot,"/",format(target.date,"%Y%j"),".tif")
      out.zip<-file.path(args$AppRoot,paste0(out.name,".zip"))
      writeRaster(target.prediction,outfile)
      add2rtoi(outfile,out.zip)
    }
    if(get.stack){
      result<-addLayer(result,target.prediction)
    }
  }
  if(snow.mode){
    endCluster()
  }
  etime<-Sys.time()
  message(paste0(length(Img2Fill)," images processed in ",MinSeg(etime,stime)))
  if(get.stack){
    return(result)
  }
}

dateNeighbours<-function(ts.raster,
                         target.date,
                         r.dates,
                         nPeriods=1,
                         nYears=1){
  targetyear<-as.integer(format(target.date,"%Y"))
  tempolarPeriods<-format(as.Date((target.date-nPeriods):(target.date+nPeriods)),"%j")
  if("365"%in%tempolarPeriods&!"366"%in%tempolarPeriods){tempolarPeriods=c(tempolarPeriods,"366")}
  temporalYears<-(targetyear-nYears):(targetyear+nYears)
  temporalWindow<-paste0(rep(temporalYears,each=length(tempolarPeriods)),
                         rep(tempolarPeriods,length(temporalYears)))
  return(raster::subset(ts.raster,which(format(r.dates,"%Y%j")%in%temporalWindow)))
}

MinSeg=function(fim, ini){
  dif=as.numeric(difftime(fim, ini, units='mins'))
  return(paste0(sprintf('%02dm', as.integer(dif)), " ",
                sprintf('%02.0fs', (dif-as.integer(dif))*60),"."))
}
