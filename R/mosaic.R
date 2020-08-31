#' Mosaics the tiles covering the region of interest
#'
#' @param x an rtoi object with a records that cover the region of interest.
#' @param y Omitted
#' @param db_path path where the images will be stored when using an rtoi.
#' By default, the path is defined by the rtoi.
#' @param out.dir path where the images will be stored when using a records.
#' @param bfilter a vector with the bands to be extracted when \code{untar=TRUE}. If not supplied, all are extracted.
#' @param warp character argument. Defines how to warp the resulting tile.
#' @param region a \code{Spatial*}, projected \code{raster*}, o
#'  \code{sf} class object defining the region of interest.
#'
#' @param ... additional arguments
#'
#' @importFrom zip zipr
#' @import raster rgdal
#' @include rtoi.R records.R
#' @export
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
#' #Landsat-5
#' sat_search(region=navarre,
#'           product="LANDSAT_TM_C1",
#'           dates=as.Date("1988-08-01")+seq(1,35))
#' download(navarre)
#'
#' mosaic(navarre,overwrite=T)
#'
#' derive(navarre,"NDVI",product="LANDSAT_TM_C1")
#' }
setMethod(f="mosaic",
          signature = c("rtoi"),
          function(x, ...){
            rtoi_size_cal(x)
            #sat<-"sen2"
            args<-list(...)
            r<-records(x)
            if(length(r)==0){stop("There are no records in for mosaic.")}
            if(get_database(x)==""){stop("db_path in rtoi must be defined.")}
            for(p in unique(product(r))){
              mosaic(x=subset(r,p,"product"),
                     out_path=get_dir(x),
                     db_path=get_database(x),
                     region=region(x),
                     ...)
            }
            rtoi_size_cal(x)
          }
)
#subset(navarre$records,"mod","sat")

#' @rdname mosaic-rtoi-ANY-method
#' @aliases mosaic,records
#' @importFrom utils untar
#' @importFrom zip zip_list
#' @importFrom sp proj4string
setMethod(f="mosaic",
          signature = c("records"),
          function(x, out_path,db_path,bfilter,warp="extent",region,overwrite=FALSE,...){
            args<-list(...)
            days<-dates(x)
            if(length(unique(product(x)))>1){stop("All the records must be from the same satellite")}
            scratch.tmp<-file.path(tempdir(),"rgt_scratch")
            dir.create(scratch.tmp,recursive = TRUE,showWarnings = FALSE)
            for(d in unique(days)){
              d<-as.Date(d)
              #out.dir<-file.path(out_path,get_mosaic_dir(x[1]),paste0(format(d,"%Y%j")))
              #out.zip<-paste0(out.dir,".zip")
              out.dir<-file.path(tempdir(),get_mosaic_dir(x[1]),paste0(format(d,"%Y%j")))
              out.zip<-file.path(out_path,get_mosaic_dir(x[1]),paste0(format(d,"%Y%j"),".zip"))
              dir.create(dirname(out.zip),recursive = TRUE,showWarnings = FALSE)
              if(file.exists(out.zip)){
                if(overwrite){
                  file.remove(out.zip)
                  mosaiced.bands<-NULL
                }else{
                  mosaiced.bands<-zip_list(out.zip)$filename
                }
              }else{
                mosaiced.bands<-NULL
              }
                dir.create(out.dir,showWarnings = FALSE,recursive = TRUE)

                dr<-x[which(days%in%d)]
                mfiles<-file.path(db_path,get_file_path(dr))

                mfiles<-mfiles[file.exists(mfiles)]
                if(length(mfiles)<1) next

                if(grepl("^Landsat", sat_name(dr)[1])){
                  mosaicFunctions<-mosaic_fun_ls(mfiles)
                }else if("Sentinel-3"==sat_name(dr)[1]){
                  if(product(dr[1])=="SY_2_SYN___"){
                    mosaicFunctions<-mosaic_fun_SY_2_SYN(mfiles,scratch.tmp=scratch.tmp)
                  }else{
                    message(paste0("Product ",product(dr[1])," not supported"))
                  }
                }else if("Sentinel-2"==sat_name(dr)[1]){
                  mosaicFunctions<-mosaic_fun_sen2(mfiles)
                }else if("Modis"==sat_name(dr)[1]){
                  mosaicFunctions<-mosaic_fun_mod(mfiles)
                }

                # get product functions
                bands<-mosaicFunctions$bands
                bands.files<-mosaicFunctions$bands.files
                filterchunks<-mosaicFunctions$filterchunks
                readfromscratch<-mosaicFunctions$readfromscratch
                defineNodata<-mosaicFunctions$defineNodata

                allfiles<-c()
                for(m in mfiles){
                  allfiles<-c(allfiles,readfromscratch(m,bands.files,scratch.tmp=scratch.tmp))
                }
                ######################################
                # Bands
                ######################################
                message(paste0("Mosaicking bands for period ",d))
                for(bnds in bands){
                  chunks<-filterchunks(allfiles,bnds)
                  if(length(chunks)>0){
                    #bname<-paste0(format(d,"%Y%j_"),gsub(":","_",bnds))
                    bname<-gsub(":","_",bnds)
                    tmpfile<-file.path(tempdir(),paste0(bname,"1.vrt"))
                    cmpfile<-file.path(out.dir,paste0(bname,"_tmp.tif"))#gsub(".vrt","_tmp.tif",tmpfile)
                    out.file.name<-gsub("_tmp","",cmpfile)
                    out.file.name<-gsub("band","B",out.file.name)
                    if(basename(out.file.name)%in%mosaiced.bands){
                      next
                    }

                    genMosaicGdalUtils(typechunks=chunks,
                                       temp=tmpfile,
                                       nodata=defineNodata(chunks,bnds),
                                       out.name=cmpfile)


                    tryCatch({
                      switch(tolower(warp),
                             "extent"={
                               r.tmp <-raster(cmpfile)
                               region <- st_transform(region,proj4string(r.tmp)); rm(r.tmp);

                               #TODO get projection using gdal_crs, cannot close the connection and remove the file
                               #region <- st_transform(region,gdal_crs(cmpfile)$input)#proj4string(r.tmp)) ;gc();

                               ext<-extent(region)

                              gdal_utils(util = "warp",
                                         source = cmpfile,
                                         destination = out.file.name,
                                         options=c("-te",ext@xmin,ext@ymin,ext@xmax,ext@ymax,
                                                   "-te_srs",st_crs(region)$proj4string)
                                            )

                               gc()
                               file.remove(cmpfile)
                             },
                             {file.rename(cmpfile,
                                          out.file.name)}
                      )
                      },error = function(e){
                        warning(e)
                        file.rename(cmpfile,
                                    out.file.name)
                        warning(paste0("Error warping image in period ",d))
                      })

                  }
                  add2rtoi(out.file.name,out.zip)
                }
                #zipr(out.zip,files=list.files(out.dir,full.names = T))
                unlink(out.dir,recursive = TRUE)
                #TODO remove all not compressed files in database
              #}else{
              #  message(paste0("File for date ",d," exists, not mosaicking..."))
              #  message("If you want to override this data use 'overwrite=T' and 'dates' argument")
              #}
            }
          }
)

