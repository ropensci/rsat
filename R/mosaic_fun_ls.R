mosaic_fun_ls <- function(mfiles) {
  allfun <- NULL
  allfun$readfromscratch <- function(m, ...) {
    file.path("/vsitar", m, untar(m, list = TRUE))
  }
  allfun$filterchunks <- function(allfiles, bnds) {
    allfiles[grepl(bnds, allfiles, ignore.case = TRUE)]
  }

  allfun$defineNodata <- function(chunks, bnds) {
    if (grepl("BQA", bnds, ignore.case = TRUE) |
        grepl("_qa", bnds, ignore.case = TRUE)) {
      nodata <- 1
    } else if (grepl("lvl2", chunks[1], ignore.case = TRUE) &
        !grepl("aerosol", bnds, ignore.case = TRUE)) {
      nodata <- -9999
    }else if(grepl("l2", chunks[1], ignore.case = TRUE)&
        !any(unlist(lapply(variables$bands$ls8,grepl,chunks,ignore.case = TRUE)))){
      nodata <- -9999
    } else {
      nodata <- 0
    }
    nodata
  }

  bands <- allfun$readfromscratch(mfiles[1])
  # lvl2 particular change
  qa <- grepl("_qa", bands)
  bands <- gsub(".*_", "", bands)

  if (any(qa)) {
    bands <- c(bands[!qa], "radsat_qa.tif", "pixel_qa.tif")
  }
  bands <- bands[grepl(".TIF", bands, ignore.case = TRUE)]
  allfun$bands <- gsub(".TIF", "", bands, ignore.case = TRUE)
  allfun
}
