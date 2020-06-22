#' compute a remote sensing index from an rtoi
#'
#' @param x rtoi object from which the index is computed.
#' @param products the name of the dataset from which the index is computed.
#' @param dates a vector with the dates being considered (optional).
#' @param fun a \code{function} that computes the remote sensing index.
#' @param overwrite logical argument. If \code{TRUE}, overwrites the existing
#' images with the same name.
#' @param verbose logical argument. If \code{TRUE}, the function prints the
#' running steps and warnings.
#'
#' @param variable the name of the variable. Run \code{show_variables()}
#' to check the variables supported by the package.
#' @param ... additional argument for variable deriving
#'
#' @return nothing. The derived variables will be save in the hard drive.
#' Use get_stars to get the variables.
#'
#' @include rtoi.R
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
#'                   db.path)
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
setGeneric("derive", function(x,
                              variable,
                              ...) {
  standardGeneric("derive")
})

#' @rdname derive
#' @aliases derive,rtoi,character
#' @importFrom utils unzip
setMethod("derive",
          signature = c("rtoi","character"),
          function(x,
                   variable,
                   product,
                   dates,
                   fun,
                   overwrite=FALSE,
                   verbose=FALSE,
                   ...) {
            if(missing(product)){
              p<-unique(product(records(x)))
              if(length(p)!=1){
                stop("Your rtoi has more than one product, use 'product' argument to specify the product to derive variables.")
              }else{
                product<-p
              }
            }
            if(missing(fun)){
              fun<-get_var_fun(variable)
            }
            #product =
            rtoi_products<-basename(list.dirs(list.dirs(x$rtoi_path,recursive = FALSE),recursive = FALSE))
            if(!any(grepl(product,rtoi_products))){
              message(paste0("Product not mosaicked, mosaic the product from you will derive variables."))
              stop(paste0("\nAvailable products: ",paste(rtoi_products, collapse = ", "),"."))
            }

            bdata<-deriveBandsData(product)
            bands<-bdata$bands
            additional.sizes<-bdata$additional.sizes

            #############################################
            # Dir creation
            #############################################
            mdir<-get_mosaic_dir(x,product)
            out.dir<-file.path(get_var_dir(x,product),variable)
            dir.create(out.dir,recursive = TRUE,showWarnings = FALSE)

            #############################################
            # Image processing
            #############################################
            images<-list.files(mdir,full.names = TRUE)
            for(i in images){
              message(paste0("Processing image ",basename(i),"."))
              layers<-file.path("/vsizip",i,unzip(i,list=TRUE)$Name)
              for(size in additional.sizes){
                out.file<-file.path(out.dir,paste0(variable,"_",format(genGetDates(i),"%Y%j"),size,".tif"))
                layer.size<-layers[grepl(size, layers,fixed = TRUE)]

                if((!file.exists(out.file))||overwrite){
                  result<-deriveVariables(bands,layers=layer.size,fun,verbose=verbose,i=i,...)
                  if(!is.null(result)){
                    #writeRaster(result,out.file,overwrite=overwrite)
                    write_stars(result,out.file,update=overwrite)
                  }
                }else{
                  message(paste0("File already exists! file: ",out.file))
                }
              }
            }

})

deriveBandsData<-function(product){
  #############################################
  # Bands data
  #############################################
  #sentinel-2
  if(product%in%"S2MSI1C"){
    return(list(bands=variables$bands[["Sentinel-2"]],
                additional.sizes=""))

    #landsat 8 product download
  }else if(product%in%c("S2MSI2A","S2MS2Ap")){
    return(list(bands=variables$bands[["Sentinel-2"]],
                additional.sizes=c("_10m","_20m","_60m")))

    #landsat 8 product download
  }else if(grepl("LANDSAT_8_C1",product)){
    return(list(bands=variables$bands$ls8,
                additional.sizes=""))
    #mod09 product download
  }else if(grepl("LANDSAT_7_C1",product)){
    return(list(bands<-variables$bands$ls7,
                additional.sizes=""))
    #mod09 product download
  }else if(grepl("LANDSAT_TM_C1",product)){
    return(list(bands=variables$bands$ls5,
                additional.sizes=""))
    #mod09 product download
  }else if(substr(product,1,5)%in%c("mod09","myd09")){
    return(list(bands=variables$bands$mod09ga,
                additional.sizes=""))

  }else if(grepl("SY_2_SYN___",product)){
    return(list(bands=variables$bands$`SY_2_SYN___`,
                additional.sizes=""))
  }else{
    warning(paste0("Product '",product,"' not supported for mosaicking."))
    return(NULL)
  }
}


deriveVariables<-function(bands,layers,fun,verbose=FALSE,i=NULL,...){
  bjump<-FALSE
  result<-NULL
  funargs<-formalArgs(fun)
  funString<-"result<-fun("
  #band load and asignation
  funargs<-formalArgs(fun)
  for(arg in funargs){
    band<-bands[names(bands)%in%arg]
    if(length(band)!=0)
      band<-layers[grepl(band,layers,ignore.case = TRUE)]
    if(length(band)==0){
      if(verbose) warning(paste0("Error reading band ",arg))
      next
    }
    eval(parse( text=paste0(arg,"<-read_stars('",layers[grepl(band,layers)],"',normalize_path = FALSE)")))
    #eval(parse( text=paste0(arg,"<-raster('",band,"')") ))
    funString<-paste0(funString,arg,"=",arg,",")
  }
  # arguments asignation
  arguments<-as.list(match.call())
  arguments<-arguments[names(arguments)%in%funargs&
                         (!names(arguments)%in%names(layers))]
  for(arg in names(arguments)){
    funString<-paste0(funString,arg,"=function.arg$",arg,",")
  }
  # complete the function
  funString<-paste0(substr(funString,1,nchar(funString)-1),")")
  #if(verbose){message(paste0("Function for evaluation: \n",funString))}
  tryCatch({
    eval(parse(text=funString))
    return(result)
  },
  error=function(e) {
    if(verbose){
      message(e)
      message(paste0("Band not found for image ",i,". Check the mosaic of this image."))
    }
  })
  return(result)
}




