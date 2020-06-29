#' @importFrom zip zipr_append
setGeneric("add2rtoi", function(infile,
                                out.zip,
                                ...) {
  standardGeneric("add2rtoi")
})

setMethod(f="add2rtoi",
          signature = c("character","character"),
          function(infile, out.zip, ...){
            if(!file.exists(out.zip)){
              zipr(out.zip,files=infile)
            }else{
              zipr_append(out.zip, infile, recurse = TRUE,
                          include_directories = TRUE)
            }
            file.remove(infile)
})
