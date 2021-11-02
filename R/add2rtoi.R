#' @importFrom zip zipr zipr_append
setGeneric("add2rtoi", function(infile,
                                outfile,
                                ...) {
  standardGeneric("add2rtoi")
})

setMethod(
  f = "add2rtoi",
  signature = c("character", "character"),
  function(infile, outfile, ...) {
    if (!file.exists(infile)) {
      warning(paste0("File not created: ", infile))
      return(NULL)
    }
    if (!file.exists(outfile)) {
      zip::zipr(outfile, files = infile)
    } else {
      zip::zipr_append(outfile, infile,
        recurse = TRUE,
        include_directories = TRUE
      )
    }
    file.remove(infile)
  }
)
