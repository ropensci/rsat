#' @import Rdpack
as.Date<-function(x,...){
  if(is.numeric(x)){
    return(base::as.Date("1970-01-01")+x)
  }else{
    return(base::as.Date(x,...))
  }
}

transform_multiple_proj <- function(obj, proj4){
  # Object to be transformed
  if(is(obj, "sf")) {
    new_obj<-obj
  }else if(is(obj, "Spatial")){
    new_obj<-st_as_sf(obj)
  }else if(is(obj, "Raster")){
    new_obj <- extent(obj)
    new_obj<-st_as_sf(as(new_obj, 'SpatialPolygons'))
    st_crs(new_obj)<-projection(obj)
  }else{
    stop("Spatial object not supported!")
  }

  if(missing(proj4)){
    return(new_obj)
  }else{
    return(st_transform(new_obj, proj4))
  }
}

lsGetPathRow<-function(str){
  str<-basename(str)
  return(substr(str,4,9))
}

lsGetDates<-function(str,...){
  arg<-list(...)
  bname<-basename(str)
  str<-gsub("\\..*","",bname)
  sizes<-sapply(str,nchar)
  sday<-c()
  for(s in 1:length(sizes)){
    if(sizes[s]==21){#new name convention
      sday<-c(sday,as.Date(substr(basename(str[s]),10,16),"%Y%j"))
    }else{#old name convention
      sday<-c(sday,as.Date(substr(basename(str[s]),11,18),"%Y%m%d"))
    }
  }
  sday<-as.Date(sday)
  if("format"%in%names(arg)){
    return(format(sday,format=arg$format))
  }else{
    return(as.Date(sday,"%Y%j"))
  }
}

genGetDates<-function(str, ...){
  arg<-list(...)
  if("date.format"%in%names(arg)){
    return(format(as.Date(gsub(".*\\s*(\\d{7}).*", "\\1", str),"%Y%j"),format=arg$date.format))
  }else{
    return(as.Date(gsub(".*\\s*(\\d{7}).*", "\\1", str),"%Y%j"))
  }
}


toEspaJSON<-function(json_list,is.array=c("products","inputs")){
  nam<-names(json_list)
  resjson<-'{'
  for(n in 1:length(nam)){
    resjson<-paste0(resjson,'"',nam[n],'":')
    nlist<-json_list[[n]]
    if(class(nlist)=='list'){
      resjson<-paste0(resjson,toEspaJSON(nlist))
    }else if(length(nlist)>1|nam[n]%in%is.array){
      resjson<-paste0(resjson,'[')
      for(x in nlist){
        if(is.na(x)){
          resjson<-paste0(resjson,'null,')
        }else{
          resjson<-paste0(resjson,'"',x,'",')
        }
      }
      resjson<-paste0(resjson,']')
    }else if(length(nlist)==1){
      if(is.na(nlist)){
        resjson<-paste0(resjson,'null')
      }else{
        resjson<-paste0(resjson,'"',nlist,'"')
      }
    }
    resjson<-paste0(resjson,",")
  }
  resjson<-paste0(resjson,'}')
  resjson<-gsub(",]","]",resjson)
  resjson<-gsub(",}","}",resjson)
  return(resjson)
}

#' @importFrom tools md5sum
genCheckMD5<-function(path.file,oficial.md5,verbose=FALSE,...){
  file.md5<-md5sum(path.file)
  file.md5<-toupper(file.md5)
  if(file.md5==oficial.md5){
    if(verbose){
      message(paste0("File md5:",file.md5))
      message(paste0("Oficial md5:",oficial.md5))
      message("Return TRUE")
    }
    return(TRUE)
  }else{
    if(verbose){
      message(paste0("File md5:",file.md5))
      message(paste0("Oficial md5:",oficial.md5))
      message("Return FALSE")
    }
    return(FALSE)
  }
}


modGetDates<-function(str,...){
  arg<-list(...)
  dt<-as.Date(gsub(".*\\As*(\\d{7}).*", "\\1", str),"%Y%j")
  if(!any(is.na(dt))){
    if("format"%in%names(arg)){
      return(format(dt,format=arg$format))
    }else{
      return(dt)
    }
  }#else{
  # return(genGetDates(str,...))
  #}
}
modGetPathRow<-function(str){
  return(gsub(".*\\s*(h\\d{2}v\\d{2}).*", "\\1", str))
}
getTileID_MSIL2A<-function(str){
  return(substr(str,40,44))
}

SENPRODUCTS<-list("Sentinel-1"=c("SLC", "GRD", "OCN"),
                  "Sentinel-2"=c("S2MSI2A","S2MSI1C", "S2MS2Ap"),
                  "Sentinel-3"=c("SR_1_SRA___", "SR_1_SRA_A", "SR_1_SRA_BS", "SR_2_LAN___", "OL_1_EFR___", "OL_1_ERR___", "OL_2_LFR___", "OL_2_LRR___", "SL_1_RBT___", "SL_2_LST___", "SY_2_SYN___", "SY_2_V10___", "SY_2_VG1___", "SY_2_VGP___"),
                  "Sentinel-5"=c("L1B_IR_SIR", "L1B_IR_UVN", "L1B_RA_BD1", "L1B_RA_BD2", "L1B_RA_BD3", "L1B_RA_BD4", "L1B_RA_BD5", "L1B_RA_BD6", "L1B_RA_BD7", "L1B_RA_BD8", "L2__AER_AI", "L2__AER_LH", "L2__CH4", "L2__CLOUD_", "L2__CO____", "L2__HCHO__", "L2__NO2___", "L2__NP_BD3", "L2__NP_BD6", "L2__NP_BD7", "L2__O3_TCL", "L2__O3____", "L2__SO2___"))
