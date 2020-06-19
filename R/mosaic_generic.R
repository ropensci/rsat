genMosaicGdalUtils<-function(typechunks,temp="temp.vrt",nodata,out.name){
  newchunks<-NULL
  tryCatch({
    if(is.null(nodata)){
        gdal_utils(util = "buildvrt",
                   source =typechunks,
                   destination = temp
        )
    }else{
      gdal_utils(util = "buildvrt",
                 source = typechunks,
                 destination = temp,
                 options=c("-srcnodata",nodata,"-vrtnodata",nodata)
      )
    }
  }, warning = function(warning_condition) {
    if(grepl("gdalbuildvrt does not support heterogeneous projection",warning_condition)){
      tryCatch({
        #reproject the images
        diffproj=TRUE
        suppressWarnings(file.remove(temp))
        proj<-paste0("EPSG:",gdal_crs(typechunks[1])$crs[[1]])
        newchunks<-c(typechunks[1])
        for(ni in 2:length(typechunks)){
          destemp<-file.path(tempdir(),basename(typechunks[ni]))
          gdal_utils(util = "warp",
                     source =typechunks[ni],
                     destination = destemp,
                     options=c("-t_srs",proj)
          )
          newchunks<-c(newchunks,destemp)
        }
        if(is.null(nodata)){
          gdal_utils(util = "buildvrt",
                     source =newchunks,
                     destination = temp
          )
        }else{
          gdal_utils(util = "buildvrt",
                     source =newchunks,
                     destination = temp,
                     options=c("-srcnodata",nodata,"-vrtnodata",nodata)
          )
        }
      },warning = function(war){
        if(grepl("Read error at scanline",warning_condition)){
          warning(paste0("Error reading an image, check the following input images:\n",paste(paste0(1:length(typechunks),". ",typechunks), collapse = '\n')))
          return(FALSE)
        }
      })
    }

  })
  gdal_utils(util = "translate",
             source =temp,
             destination = out.name,
             options=c("-of","GTiff")
  )
  if(file.exists(temp))file.remove(temp)
  if(!is.null(newchunks))file.remove(file.remove(newchunks[-1],showWarnings = FALSE))
  return(TRUE)
}

