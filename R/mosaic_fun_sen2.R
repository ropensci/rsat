mosaic_fun_sen2<-function(mfiles){
  allfun<-NULL
  allfun$readfromscrach<-function(m,...){
    file.path("/vsizip",m,zip_list(m)$filename)
  }
  allfun$filterchunks<-function(allfiles,bnds){
    allfiles[grepl(paste0(bnds,".jp2"),allfiles, ignore.case=TRUE)]
  }
  bands<-allfun$readfromscrach(mfiles[1])
  bands<-bands[grepl(".jp2",bands,ignore.case = TRUE)]
  if(any(grepl("60m",bands))){
    bands<-unlist(lapply(strsplit(gsub(".*/","",gsub(".*\\\\","",bands)),"_"),function(x){do.call(paste,as.list(c(x[c(length(x)-1,length(x))],sep ="_")))}))
    bands<-gsub(".*\\d{8}T\\d{6}_","",bands)
  }else{
    bands<-gsub(".*_","",bands)
  }
  allfun$bands<-gsub(".jp2","",bands)

  allfun$defineNodata<-function(chunks,bnds){
    NULL
  }
  allfun
}
