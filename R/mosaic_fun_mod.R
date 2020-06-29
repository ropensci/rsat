mosaic_fun_mod<-function(mfiles){
  allfun<-NULL
  allfun$readfromscrach<-function(m,...){
    image.data<-unlist(strsplit(gdal_utils(util = "info",
                                           source =m,quiet=TRUE),"\n"),
                       recursive =FALSE)
    bands.names<-image.data[grepl(".*SUBDATASET_.*_NAME=", image.data)]
    bands.names<-gsub(".*SUBDATASET_.*_NAME=","",bands.names)
    bands.names
  }
  allfun$filterchunks<-function(allfiles,bnds){
    allfiles[grepl(pattern=bnds,x=allfiles,perl = T)]
  }
  bands<-allfun$readfromscrach(mfiles[1])
  bands<-gsub(".*SUBDATASET_.*_NAME=","",bands)
  allfun$bands<-gsub('.*":','',bands)
  allfun$defineNodata<-function(chunks,bnds){
    NULL
  }

  allfun
}
