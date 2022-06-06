#' @importFrom sf gdal_crs
genMosaicGdalUtils <- function(typechunks,
                               temp = "temp.vrt",
                               nodata, out.name,
                               verbose = FALSE) {
  newchunks <- NULL
  tryCatch(
    {
      if(verbose){
        print(paste0(Sys.time(), " - Working with files:"))
        writeLines(paste0("=>", typechunks))
      }
      if (is.null(nodata)) {
        gdal_utils(
          util = "buildvrt",
          source = typechunks,
          destination = temp,
          quiet = !verbose
        )
      } else {
        gdal_utils(
          util = "buildvrt",
          source = typechunks,
          destination = temp,
          options = c("-srcnodata", nodata, "-vrtnodata", nodata),
          quiet = !verbose
        )
      }
      if (verbose) message("genMosaicGdalUtils run correctly.")
    },
    warning = function(warning_condition) {
      if (paste0("GDAL Message 1: VSIFSeekL(xxx, SEEK_END) ",
                 "may be really slow on GZip streams.") %in%
          warning_condition$message) {
        gdal_utils(
          util = "buildvrt",
          source = typechunks,
          destination = temp,
          options = c("-srcnodata", nodata, "-vrtnodata", nodata),
          quiet = !verbose
        )
        # gdal_utils(util = "translate",
        #           source =temp,
        #           destination = out.name,
        #           options=c("-of","GTiff")
        # )
      } else {
        if (grepl("gdalbuildvrt does not support heterogeneous projection",
                  warning_condition)) {
          tryCatch(
            {
              # reproject the images
              if(verbose) print("Reprojecting images")
              diffproj <- TRUE
              suppressWarnings(file.remove(temp))
              proj <- gdal_crs(typechunks[1])$input
              if(verbose) print(paste0("Projection-> ", proj))
              newchunks <- c(typechunks[1])
              for (ni in 2:length(typechunks)) {
                destemp <- file.path(tempdir(), basename(typechunks[ni]))

                #gdal can have more than 1 driver for jp2 file format
                #gdal_utils throws a warning notifying that it chooses the default
                #the process would continue normally, but the warning forces the warp process to stop
                #if we force the default driver it works fine
                driverOptions <- c()
                if(endsWith(typechunks[ni], ".jp2")){
                  driverOptions <- c("-if", "JP2OpenJPEG",
                                     "-of", "JP2OpenJPEG")
                }


                gdal_utils(
                  util = "warp",
                  source = typechunks[ni],
                  destination = destemp,
                  quiet = !verbose,
                  options = c("-t_srs", proj,
                              "-overwrite",
                              driverOptions,
                              "-dstnodata", -9999)
                )
                newchunks <- c(newchunks, destemp)
              }
              if (is.null(nodata)) {
                gdal_utils(
                  util = "buildvrt",
                  source = newchunks,
                  destination = temp,
                  options = c("-srcnodata", -9999, "-vrtnodata", -9999),
                  quiet = !verbose
                )
              } else {
                gdal_utils(
                  util = "buildvrt",
                  source = newchunks,
                  destination = temp,
                  options = c("-srcnodata", paste0("\"", nodata, "-9999\""), "-vrtnodata", paste0("\"", nodata, "-9999\"")),
                  quiet = !verbose
                )
              }
            },
            warning = function(war) {
              if(verbose) print(paste0("Warning twrown while reprojecting: ", war))
              if (grepl("Read error at scanline", warning_condition)) {
                warning(paste0("Error reading an image, ",
                               "check the following input images:\n",
                               paste(paste0(seq_len(length(typechunks)),
                                            ". ", typechunks),
                                     collapse = "\n")))
                return(FALSE)
              }
            },
            error = function(e) {
              if(verbose) print(paste0("Error twrown while reprojecting: ", e))
              warning(e)
            }
          )
        } else {
          if(verbose) print(paste0("Warning twrown in mosaic_generic: ", warning_condition))
          message(warning_condition)
        }
      }
    },
    error = function(e) {
      if(verbose) print(paste0("Error twrown in mosaic_generic: ", e))
      warning(e)
    }
  )
  if(verbose){
    print("translating output to GTiff")
    print(paste0("source->", temp))
    print(paste0("destination->", out.name))
  }
  gdal_utils(
    util = "translate",
    source = temp,
    destination = out.name,
    options = c("-of", "GTiff"),
    quiet = !verbose
  )

  file.remove(temp)
  return(TRUE)
}
