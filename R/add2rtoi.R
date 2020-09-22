#' @importFrom zip zipr zipr_append
setGeneric("add2rtoi", function(infile,
                                out.zip,
                                ...) {
  standardGeneric("add2rtoi")
})

setMethod(f="add2rtoi",
          signature = c("character","character"),
          function(infile, out.zip, ...){
            if(!file.exists(infile)){
              warning(paste0("File not created: ",infile))
              return(NULL)
            }
            if(!file.exists(out.zip)){
              zip::zipr(out.zip,files=infile)
            }else{
              zip::zipr_append(out.zip, infile, recurse = TRUE,
                               include_directories = TRUE)
            }
            file.remove(infile)
})

