mosaic_fun_SY_2_SYN<-function(mfiles,...){
  #TODO
  #"SY_2_SYN___"
  # if(product(dr[1])="OL_2_LFR___"){
  #   readfromscratch<-function(m){
  #     # extract
  #     # get new paths
  #     # filtra las bandas que queramos
  #     # return the paths
  #   }
  #   filterchunks<-function(allfiles,bnds){
  #     allfiles[grepl(paste0(bnds,".jp2"),allfiles, ignore.case=TRUE)]
  #   }
  #   bands<-c("ogvi.nc","tie_meteo.nc")#manual designation of bands
  #
  # }

  allfun<-NULL
  allfun$readfromscratch<-function(m,...){
    args<-list(...)
    if("bands.files"%in%names(args)){
      bands.files<-args$bands.files
    }else{
      stop("Error reading from scrach.")
    }
    ext.dir<-file.path(args$scratch.tmp,gsub(".zip","",basename(m)))
    unzip(m,exdir = ext.dir,junkpaths = TRUE)
    #newpaths<-list.files(ext.dir,full.names = TRUE)
    newpaths<-file.path(ext.dir,paste0(bands.files,".nc"))

    #extract coordinates
    coordinates.file<-file.path(ext.dir,"geolocation.nc")
    coor.info<-gdal_utils(util = "info",
                          source =coordinates.file,
                          quiet =TRUE)

    coor.info<-unlist(strsplit(coor.info,"\n"))
    coor.names<-coor.info[grepl(".*SUBDATASET_.*_NAME=", coor.info)]
    coor.names<-gsub(".*SUBDATASET_.*_NAME=","",coor.names)
    coor.names<-gsub("\"","",coor.names,fixed = TRUE)

    lat<-file.path(ext.dir,"lat.vrt")
    lon<-file.path(ext.dir,"lon.vrt")
    gdal_utils(util = "translate",
               source =coor.names[2],
               destination = lat,
               options = c("-of","VRT")
    )
    gdal_utils(util = "translate",
               source =coor.names[3],
               destination = lon,
               options = c("-of","VRT")
    )

    #TODO remove GCP from vrt
    #removeGCPfromVRT<-function(fpath){
    #  a<-read_xml(fpath)
    #}

    newbands<-c()
    for(np in newpaths){
      image.data<-gdal_utils(util = "info",
                             source = np,quiet = TRUE)
      image.data<-unlist(strsplit(image.data,"\n"))
      bands.names<-image.data[grepl(".*SUBDATASET_.*_NAME=", image.data)]
      bands.names<-gsub(".*SUBDATASET_.*_NAME=","",bands.names)
      bands.names<-gsub("\"","",bands.names,fixed = TRUE)
      img.name<-gsub("\\.nc","",basename(np))

      #add projection
      for(b in bands.names){
        b.tmp.name<-gsub(":","_",paste0(gsub("\\.","_",basename(b)),".vrt"))
        b.name<-gsub("\\.vrt",".tif",b.tmp.name)
        out.tmp.file.name<-file.path(ext.dir,b.tmp.name)
        print(out.tmp.file.name)
        out.file.name<-file.path(ext.dir,b.name)
        if(!file.exists(out.file.name)){
          gdal_utils(util = "translate",
                     source =b,
                     destination = out.tmp.file.name,
                     options = c("-of","VRT")
          )
          d<-readLines(out.tmp.file.name)
          lines<-c(as.character(d[1]),
                   '<metadata domain="GEOLOCATION">',
                   paste0('<mdi key="X_DATASET">',lon,'</mdi>'),
                   '<mdi key="X_BAND">1</mdi>',
                   paste0('<mdi key="Y_DATASET">',lat,'</mdi>'),
                   '<mdi key="Y_BAND">1</mdi>',
                   '<mdi key="PIXEL_OFFSET">0</mdi>',
                   '<mdi key="LINE_OFFSET">0</mdi>',
                   '<mdi key="PIXEL_STEP">1</mdi>',
                   '<mdi key="LINE_STEP">1</mdi>',
                   '</metadata>',
                   as.character(d[2:length(d)]))
          fileConn<-file(paste0(out.tmp.file.name))
          writeLines(lines, fileConn)
          close(fileConn)

          gdal_utils(util = "warp",
                     source =out.tmp.file.name,
                     destination = out.file.name,
                     options = c("-geoloc","-t_srs","EPSG:4326")
          )
        }
        newbands<-c(newbands,out.file.name)
      }
    }
    return(newbands)
  }
  allfun$filterchunks<-function(allfiles,bnds){
    allfiles[grepl(paste0(bnds,"$"),allfiles, ignore.case=TRUE)]
  }
  allfun$defineNodata<-function(chunks,bnds){
    0
  }
  allfun$bands.files<-c("Syn_Oa01_reflectance",
                        "Syn_Oa02_reflectance",
                        "Syn_Oa03_reflectance",
                        "Syn_Oa04_reflectance",
                        "Syn_Oa05_reflectance",
                        "Syn_Oa06_reflectance",
                        "Syn_Oa07_reflectance",
                        "Syn_Oa08_reflectance",
                        "Syn_Oa09_reflectance",
                        "Syn_Oa10_reflectance",
                        "Syn_Oa11_reflectance",
                        "Syn_Oa12_reflectance",
                        "Syn_Oa16_reflectance",
                        "Syn_Oa17_reflectance",
                        "Syn_Oa18_reflectance",
                        "Syn_Oa21_reflectance",
                        "Syn_S1N_reflectance",
                        "Syn_S1O_reflectance",
                        "Syn_S2N_reflectance",
                        "Syn_S2O_reflectance",
                        "Syn_S3N_reflectance",
                        "Syn_S3O_reflectance",
                        "Syn_S5N_reflectance",
                        "Syn_S5O_reflectance",
                        "Syn_S6N_reflectance",
                        "Syn_S6O_reflectance"#,
                        #"Syn_SDR_removed_pixel"
  )
  #TODO band filter
  npaths<-allfun$readfromscratch(mfiles[1],bands.files=allfun$bands.files,...)
  allfun$bands<-basename(npaths)
  # npaths<-npaths[basename(npaths)%in%paste0(bands.files,".vrt")]
  # npaths<-normalizePath(npaths)
  # bands<-c()
  # for(np in npaths){
  #   image.data<-gdal_utils(util = "info",
  #                          source = np,quiet = TRUE)
  #   image.data<-unlist(strsplit(image.data,"\n"))
  #   bands.names<-image.data[grepl(".*SUBDATASET_.*_NAME=", image.data)]
  #   bands.names<-gsub(".*SUBDATASET_.*_NAME=","",bands.names)
  #   bands.names<-gsub("\"","",bands.names,fixed = T)
  #   bands.names<-gsub("HDF5:","",bands.names)
  #   bands.names<-gsub(normalizePath(dirname(np)),"",bands.names,fixed=TRUE)
  #   bands.names<-gsub("\\","",bands.names,fixed=TRUE)
  #   bands<-c(bands,bands.names)
  # }
  #gdal_utils("info","C:\\Users\\Unai\\AppData\\Local\\Temp\\RtmpeW5J0W/rgt_scratch/S3A_SY_2_SYN____20190104T095416_20190104T095716_20190106T010554_0179_040_022_2160_LN2_O_NT_002/Syn_Oa01_reflectance.nc")
  allfun
}
