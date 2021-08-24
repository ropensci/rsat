#' A class object for satellite image metadata
#'
#' This class object organizes the attributes of satellite images' metadata
#' from several missions/programs uniformly. Structuring the information
#' facilitates managing, previewing, and downloading data records.
#'
#' \code{records} works as vector. It accepts usual R methods such as
#' \code{c}, \code{[]}, \code{length()}, \code{subset()} or \code{unique()}.
#' Each record (vector element) contains several parameters or slots.
#'
#' The object can be coerced into a \code{data.frame} by
#' using the function \code{as.data.frame()}. The \code{data.frame} can
#' be transformed back into a \code{records} with the function
#' \code{as.records()}.
#' @slot sat the name of the satellite.
#' @slot name the name of the file.
#' @slot date capturing date of the image.
#' @slot product name of the data product.
#' @slot path the path of the tiling system.
#' @slot row the row of the tiling system.
#' @slot tileid the tile identification number.
#' @slot download the download url.
#' @slot file_path the saving directory for the satellite record.
#' @slot preview the preview url.
#' @slot api_name the name of the API.
#' @slot order boolean, whether the image needs to be ordered.
#' @slot extent_crs coordinate reference system of the preview.
#'
#' @include extent_crs.R
#' @examples
#' \dontrun{
#' data(ex.navarre)
#' # Create a records of Sentinel-2 Level 1C images
#' s2.lvl1.result <- sat_search(
#'   region = ex.navarre,
#'   product = "S2MSI1C",
#'   dates = as.Date("2018-01-01") + seq(1, 30, 1)
#' )
#'
#' # Create a records of Sentinel-2 Level 2A images
#' s2.lvl2.result <- sat_search(
#'   region = ex.navarre,
#'   product = "S2MSI2A",
#'   dates = as.Date("2019-01-01") + seq(1, 30, 1)
#' )
#' class(s2.lvl2.result)
#' dates(s2.lvl2.result)
#' all.records <- c(s2.lvl1.result, s2.lvl2.result)
#'
#' print(all.records)
#' }
setClass(
  "records",
  slots = c(
    sat = "character",
    name = "character",
    date = "Date",
    product = "character",
    path = "numeric",
    row = "numeric",
    tileid = "character",
    download = "character",
    file_path = "character",
    preview = "character",
    api_name = "character",
    order = "logical",
    extent_crs = "extent_crs"
  ),
  validity = function(object) {
    if (length(object@path) > 1) {
      if (!all(length(object@name) == c(
        length(object@date),
        length(object@sat),
        length(object@product),
        length(object@path),
        length(object@row),
        length(object@download),
        length(object@file_path),
        length(object@preview),
        length(object@extent_crs)
      ))
      ) {
        return("All slots must have the same length")
      }
    }
    return(TRUE)
  }
)


#' Create a new \code{records} object
#'
#' Create a new \code{records} object from scratch
#'
#' @param sat the name of the satellite to which the record belongs.
#' @param name the name of the record.
#' @param date the date of the record.
#' @param product the product.
#' @param path the path of the tiling system.
#' @param row the row of the tiling system.
#' @param tileid the tile id.
#' @param download the url to download the satellite record.
#' @param file_path the saving directory for the satellite record.
#' @param preview the url of the preview of the satellite record.
#' @param api_name the api name.
#' @param order boolean, defines if the image must be requested or not.
#' @param extent_crs extent (used to project the preview).
#'
#' @return records object
#'
#' @examples
#' \dontrun{
#' a <- new_record(
#'   sat = "modis",
#'   name = "mod09a",
#'   date = as.Date("2011087", "%Y%j"),
#'   product = "product",
#'   download = "url/aaa/download"
#' )
#' }
setGeneric("new_record", function(sat,
                                  name,
                                  date,
                                  product,
                                  download,
                                  file_path,
                                  path,
                                  row,
                                  tileid,
                                  preview,
                                  api_name,
                                  order,
                                  extent_crs) {
  standardGeneric("new_record")
})
#' @rdname new_record
#' @aliases new_record,
#' character,
#' character,
#' Date,
#' character,
#' character,
#' character,
#' numeric,
#' numeric,
#' character,
#' character,
#' character,
#' logical,
#' extent_crs
setMethod(
  "new_record",
  signature(
    sat = "character",
    name = "character",
    date = "Date",
    product = "character",
    download = "character",
    file_path = "character",
    path = "numeric",
    row = "numeric",
    tileid = "character",
    preview = "character",
    api_name = "character",
    order = "logical",
    extent_crs = "extent_crs"
  ),
  function(sat,
           name,
           date,
           product,
           download,
           file_path,
           path,
           row,
           tileid,
           preview,
           api_name,
           order,
           extent_crs) {
    new("records",
      sat = sat,
      name = name,
      date = as.Date(date),
      download = download,
      product = product,
      file_path = file_path,
      path = path,
      row = row,
      tileid = tileid,
      preview = preview,
      api_name = api_name,
      order = order,
      extent_crs = extent_crs
    )
  }
)

#' @rdname new_record
#' @aliases new_record,
#' character,
#' character,
#' Date,
#' character,
#' character,
#' character,
#' numeric,
#' numeric,
#' character,
#' character,
#' character,
#' logical,
#' missing
setMethod(
  "new_record",
  signature(
    sat = "character",
    name = "character",
    date = "Date",
    product = "character",
    download = "character",
    file_path = "character",
    path = "numeric",
    row = "numeric",
    tileid = "character",
    preview = "character",
    api_name = "character",
    order = "logical",
    extent_crs = "missing"
  ),
  function(sat,
           name,
           date,
           product,
           download,
           file_path,
           path,
           row,
           tileid,
           preview,
           api_name,
           order) {
    nlen <- length(sat)
    extent_crs <- new_extent_crs()
    if (nlen > 1) {
      for (x in 2:nlen) {
        extent_crs <- c(extent_crs, new_extent_crs())
      }
    }
    new("records",
      sat = sat,
      name = name,
      date = as.Date(date),
      download = download,
      product = product,
      file_path = file_path,
      path = path,
      row = row,
      preview = preview,
      order = order,
      extent_crs = extent_crs
    )
  }
)

#' @importFrom utils head
#' @rdname print-rtoi-method
#' @aliases print,records
#' @export
setMethod(
  "print",
  signature(x = "records"),
  function(x) {
    len <- length(x@path)
    if (len == 0) {
      return(cat("Empty records object"))
    } else if (len == 1) {
      str.print <- "Record: \n"
      slots <- names(getSlots("records"))
      slots <- slots[!slots %in% c("extent_crs")]
      for (s in slots) {
        str.print <- paste0(str.print, s, ": ",
                            paste0(slot(x, s), collapse = ","), "\n")
      }
      return(cat(str.print))
    } else {
      print(head(as.data.frame(x)))
    }
  }
)

#' Show an object
#'
#' Display the object, by printing, plotting or whatever suits its class.
#' This function has specialized methods. Use \code{showDefault()} to call
#' the default method.
#'
#' Formal methods for the function \code{show} will usually be invoked
#' automatically (see the details).
#'
#' @param object Any R Object
#'
#' @export
setMethod(
  f = "show",
  signature = "records",
  definition = function(object) {
    print(object)
  }
)

#' Coerce to a Data Frame
#'
#' Functions to check if an object is a data frame, or coerce it if possible.
#'
#' @param x Any R object.
#' @rdname as.data.frame
#' @returns returns a data frame, normally with all row names
#' @examples
#' # load example rtoi
#' navarre <- read_rtoi(system.file("ex/Navarre",package="rsat"))
#' # get the records
#' rcds <- records(navarre)
#' # coerce the records to rtoi
#' df <- as.data.frame(rcds)
#' # print the dataframe
#' print(df)
#' @export
setMethod(
  "as.data.frame",
  signature(x = "records"),
  function(x) {
    slots <- names(getSlots("records"))
    df <- data.frame(sat = slot(x, slots[1]))
    slots <- slots[!slots %in% c("extent_crs")]
    for (s in slots[-1]) {
      if (s == "date") {
        df[s] <- dates(x)
      } else {
        df[s] <- slot(x, s)
      }
    }
    return(cbind(df, as.data.frame(x@extent_crs)))
  }
)

#' Combine values into a vector or a list
#'
#' This is a generic function which combines its arguments.
#'
#' The default method combines its arguments to form a vector.
#' All arguments are coerced to a common type which is the type
#'  of the returned value. All attributes except names are removed.
#'
#' @param x a \code{records} object.
#' @param ... additional arguments.
#'
#' @export
setMethod(
  f = "c",
  signature("records"),
  definition = function(x, ...) {
    args <- list(...)

    for (z in args) {
      if (length(x) == 0) {
        x <- z
        next
      }
      if (length(z) == 0) {
        next
      }
      for (s in names(getSlots("records"))) {
        slot(x, s) <- c(slot(x, s), slot(z, s))
      }
    }
    return(x)
  }
)

#' Extract or replace parts of an object
#'
#' Operators acting on vectors, matrices, arrays and lists to
#' extract or replace parts.
#'
#' @param x object from which to extract element(s) or in which to
#' replace element(s).
#' @param i numeric argument. The the position of the element to
#' select/modify.
#' @param value a \code{records} argument. The slot of the records
#' to be changed.
#'
#' @export
setMethod(
  f = "[", signature = "records",
  definition = function(x, i) {
    for (s in names(getSlots("records"))) {
      slot(x, s) <- slot(x, s)[i]
    }
    return(x)
  }
)

#' Extract or replace parts of an object
#' @rdname sub-records-ANY-ANY-ANY-method
#' @aliases '[<-',records,records
setReplaceMethod(
  f = "[",
  signature = "records",
  definition = function(x, i, value) {
    for (s in names(getSlots("records"))) {
      slot(x, s)[i] <- slot(value, s)[i]
    }
    return(x)
  }
)

#' Length of an object
#'
#' Get or set the length of vectors (including lists) and factors,
#' and of any other R object for which a method has been defined.
#'
#' @param x a \code{records} object to compute its length.
#'
#' @export
setMethod(
  f = "length",
  signature = "records",
  definition = function(x) {
    return(length(x@path))
  }
)


#' Create records object from data frame
#'
#' @param x  a \code{data.frame} with columns representing the slots of
#' records.
#' @returns returns a records objects with the columns values in \code{x}
#' @export
#' @examples
#' # load example rtoi
#' navarre <- read_rtoi(system.file("ex/Navarre",package="rsat"))
#' # get the records
#' rcds <- records(navarre)
#' # coerce the records to dataframr
#' df <- as.data.frame(rcds)
#' # print the dataframe
#' print(df)
#'
#' # coerce the dataframe to records
#' rcds2 <- as.records(df)
#' # check the conversion
#' identical(rcds,rcds2)
setGeneric("as.records", function(x) {
  standardGeneric("as.records")
})

#' @importFrom methods as new slot<- show
#' @rdname as.records
#' @aliases as.records,data.frame
setMethod(
  f = "as.records",
  signature = "data.frame",
  definition = function(x) {
    type <- getSlots("records")
    na <- names(type)
    type <- type[which(!type %in% "extent_crs")]
    ecrs.index <- which(names(x) %in% c("EPSG",
                                        "xmin",
                                        "ymin",
                                        "xmax",
                                        "ymax"))
    if (length(ecrs.index) != 0) {
      ecrs.df <- x[, c("EPSG",
                       "xmin",
                       "ymin",
                       "xmax",
                       "ymax")]
      x <- x[, -ecrs.index]
    }

    if (all(names(x) %in% na)) {
      cols <- which(names(x) %in% na)
      x <- x[, cols]

      for (ty in seq_len(length(type))) {
        if (type[ty] == "Date") {
          x[, ty] <- as.Date(x[, ty],"%Y-%m-%d")
        } else {
          x[, ty] <- as(x[, ty], type[ty])
        }
      }

      if (length(ecrs.index) == 0) {
        return(do.call("c", apply(x, 1, FUN = function(i) {
          do.call(new_record, as.list(i))
        })))
      } else {
        extent_crs <- new("extent_crs",
          EPSG = as.numeric(ecrs.df[, "EPSG"]),
          xmin = as.numeric(ecrs.df[, "xmin"]),
          ymin = as.numeric(ecrs.df[, "ymin"]),
          xmax = as.numeric(ecrs.df[, "xmax"]),
          ymax = as.numeric(ecrs.df[, "ymax"])
        )
        r <- new("records")
        for (i in seq_len(nrow(x))) {
          r <- c(r, do.call(new_record, c(as.list(x[i, ]),
                                          list(extent_crs = extent_crs[i]))))
        }
      }

      return(r)
    } else {
      stop(paste0("To create a records object provide ",
                  "a data frame with the following names: ",
                  paste(na, collapse = ","), "."))
    }
  }
)

#' Get the name of the satellite(s) from a \code{records} or an \code{rtoi}
#'
#' @param x a \code{records} or an \code{rtoi} object.
#'
#' @export
#' @examples
#' # load example rtoi
#' navarre <- read_rtoi(system.file("ex/Navarre",package="rsat"))
#' # get the records
#' rcds <- records(navarre)
#' # coerce the records to dataframr
#' sat_name(rcds)
setGeneric("sat_name", function(x) standardGeneric("sat_name"))
#' @rdname sat_name
#' @aliases sat_name,records
setMethod(
  f = "sat_name",
  signature = c("records"),
  definition = function(x) {
    return(x@sat)
  }
)


#' Get the dates from a \code{records} or an \code{rtoi}
#'
#' @param x a \code{records} or an \code{rtoi} object.
#' @return returns a vector of \code{Date} class
#' @export
#' @examples
#' # load example rtoi
#' navarre <- read_rtoi(system.file("ex/Navarre",package="rsat"))
#'
#' # get a vector of dates includes in rtoi
#' dates(navarre)
#'
#' # get the records
#' rcds <- records(navarre)
#'
#' # coerce the records to dataframr
#' dates(rcds)
setGeneric("dates", function(x) standardGeneric("dates"))

#' @rdname dates
#' @export
setMethod(
  f = "dates",
  signature = "records",
  definition = function(x) {
    if (inherits(x@date, "Date")) {
      out <- x@date
    } else {
      out <- as.Date(integer(0))
    }
    return(out)
  }
)

#' @rdname dates
#' @export
setGeneric("dates<-", function(x, value) standardGeneric("dates<-"))

#' @rdname dates
setMethod(
  f = "dates<-",
  signature = "records",
  definition = function(x, value) {
    x@date <- value
    return(x)
  }
)


#' Get the name of the product from a \code{records} or an \code{rtoi}
#'
#' @param x a \code{records} or an \code{rtoi} object.
#'
#' @export
setGeneric("product", function(x) standardGeneric("product"))
#' @rdname product
#' @aliases product,records
setMethod(
  f = "product",
  signature = "records",
  definition = function(x) {
    return(x@product)
  }
)

#' Get the file path of a \code{records} or an \code{rtoi}
#'
#' @param x a \code{records} or an \code{rtoi} object.
#' @param value character argument. The new directory of \code{x}.
#'
#' @export
setGeneric("get_dir", function(x) standardGeneric("get_dir"))
#' @rdname get_dir
#' @aliases get_dir,records
setMethod(
  f = "get_dir",
  signature = "records",
  definition = function(x) {
    return(file.path(sat_name(x), product(x)))
  }
)


setGeneric("get_mosaic_dir", function(x, ...)
  standardGeneric("get_mosaic_dir"))
setMethod(
  f = "get_mosaic_dir",
  signature = c(x = "records"),
  definition = function(x) {
    return(file.path(get_dir(x), "mosaic"))
  }
)


setGeneric("get_file_path", function(x) standardGeneric("get_file_path"))
setMethod(
  f = "get_file_path",
  signature = "records",
  definition = function(x) {
    return(x@file_path)
  }
)

#' Get the slot called order from a \code{records} or an \code{rtoi}
#'
#' @param x a \code{records} or an \code{rtoi} object.
#' @param value logical argument. The new value for \code{x}.
#'
#' @export
setGeneric("get_order", function(x) {
  standardGeneric("get_order")
})
#' @rdname get_dir
#' @aliases get_dir,records
setMethod(
  f = "get_order",
  signature = "records",
  definition = function(x) {
    return(x@order)
  }
)

#' @rdname get_order
#' @aliases get_order<-
#' @export
setGeneric("get_order<-", function(x, value) {
  standardGeneric("get_order<-")
})

#' @rdname get_order
#' @aliases get_order<-,records
setMethod(
  f = "get_order<-",
  signature = "records",
  definition = function(x, value) {
    x@order <- value
    return(x)
  }
)



#' Filter the satellite records of a \code{records} or an \code{rtoi}
#'
#' @param x a \code{records} or an \code{rtoi} object.
#' @param subset an R object with the value for subsetting.
#' @param select character argument indicating the name of the slot.
#'
#' @export
setMethod(
  f = "subset",
  signature = "records",
  definition = function(x, subset, select) {
    if (inherits(subset, "numeric") & !select %in% c("path", "row")) {
      return(x[subset])
    }
    records.names <- names(getSlots("records"))
    if (!select %in% records.names) {
      stop("'select' must be a slot name from the class records.")
    }
    return(x[which(slot(x, select) %in% subset)])
  }
)

#' Extract unique elements
#'
#' It returns a \code{records} like \code{x} but with duplicate
#' elements/rows removed.
#'
#' @param x a \code{records} object.
#'
#' @export
setMethod(
  f = "unique",
  signature = "records",
  definition = function(x) {
    return(x[which(!duplicated(names(x)))])
  }
)

#' Extract the url of the preview
#'
#' It returns a character vector of urls to preview the data records.
#'
#' @param x a \code{records} object.
#'
#' @export
setGeneric("get_preview", function(x) {
  standardGeneric("get_preview")
})
#' @rdname get_preview
#' @aliases get_preview,records
setMethod(
  f = "get_preview",
  signature = "records",
  definition = function(x) {
    return(x@preview)
  }
)

#' Extract the url to download a data record
#'
#' It returns a character with the url to download the image.
#'
#' @param x a \code{records} object.
#'
#' @export
setGeneric("get_download", function(x) {
  standardGeneric("get_download")
})
#' @rdname get_preview
#' @aliases get_preview,records
setMethod(
  f = "get_download",
  signature = "records",
  definition = function(x) {
    return(x@download)
  }
)


#' Get the name of the object
#'
#' A function to get or set the names of an object.
#'
#' @param x a \code{records} or an \code{rtoi} object.
#' @param value character argument. The new value for \code{x}.
#'
#' @return a character vector containing the name of
#' all the names in \code{x}.
#' @export
#'
#' @examples
#' \dontrun{
#' data(ex.navarre)
#' # path where the data will be
#' rtoi.path <- tempdir()
#' # path where downloads are stored
#' db.path <- file.path(tempdir(), "DATABASE")
#' navarre <- new_rtoi(
#'   "Navarre",
#'   ex.navarre,
#'   rtoi.path,
#'   db.path
#' )
#' names(navarre)
#'
#' # Create a set of records using sat_search function
#' s2.lvl2.result <- sat_search(
#'   region = ex.navarre,
#'   product = "S2MSI2A",
#'   dates = as.Date("2019-01-01") + seq(1, 30, 1)
#' )
#' names(s2.lvl2.result)
#'
#' names(s2.lvl2.result) <- "New name"
#' names(s2.lvl2.result)
#' }
setMethod(
  f = "names",
  signature = "records",
  definition = function(x) {
    return(x@name)
  }
)

setMethod(
  f = "extent",
  signature = "records",
  definition = function(x) {
    return(extent(x@extent_crs))
  }
)

#' @importFrom terra ext
setMethod(
  f = "ext",
  signature = "records",
  definition = function(x) {
    return(ext(x@extent_crs))
  }
)

#' @importFrom terra crs
setMethod(
  f = "crs",
  signature = "records",
  definition = function(x) {
    return(crs(x@extent_crs))
  }
)

#' Get the API name of a \code{records}
#'
#' A function to get or set the api names of an object.
#'
#' @param x a \code{records} object.
#'
#' @return a character vector containing the API names of the
#' elements in \code{x}.
#' @export
#' @examples
#' # load example rtoi
#' navarre <- read_rtoi(system.file("ex/Navarre",package="rsat"))
#'
#' # get the records
#' rcds <- records(navarre)
#'
#' # coerce the records to dataframr
#' get_api_name(rcds)
setGeneric("get_api_name", function(x) {
  standardGeneric("get_api_name")
})
#' @rdname get_api_name
#' @aliases get_api_name,records
setMethod(
  f = "get_api_name",
  signature = "records",
  definition = function(x) {
    return(x@api_name)
  }
)
