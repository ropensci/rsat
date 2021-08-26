setClass(
  "extent_crs",
  slots = c(
    EPSG = "numeric",
    xmin = "numeric",
    ymin = "numeric",
    xmax = "numeric",
    ymax = "numeric"
  ),
  validity = function(object) {
    if (length(object) > 1) {
      if (!all(length(object@EPSG) == c(
        length(object@xmin),
        length(object@ymin),
        length(object@xmax),
        length(object@ymax)
      ))
      ) {
        return("All slots must have the same length")
      }
    }
    return(TRUE)
  }
)


setGeneric("new_extent_crs", function(EPSG, xmin, ymin, xmax, ymax) {
  standardGeneric("new_extent_crs")
})

setMethod(
  "new_extent_crs",
  signature(
    EPSG = "numeric",
    xmin = "numeric",
    ymin = "numeric",
    xmax = "numeric",
    ymax = "numeric"
  ),
  function(EPSG, xmin, ymin, xmax, ymax) {
    if (length(EPSG) == 1) {
      if (EPSG == 0) {
        return(new("extent_crs",
          EPSG = 0,
          xmin = 0,
          ymin = 0,
          xmax = 0,
          ymax = 0
        ))
      }
    }
    new("extent_crs",
      EPSG = EPSG,
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax
    )
  }
)
setMethod(
  "new_extent_crs",
  signature(
    EPSG = "missing",
    xmin = "missing",
    ymin = "missing",
    xmax = "missing",
    ymax = "missing"
  ),
  function() {
    return(new("extent_crs",
      EPSG = 0,
      xmin = 0,
      ymin = 0,
      xmax = 0,
      ymax = 0
    ))
  }
)

#' @rdname print
#' @aliases print,extent_crs
setMethod(
  "print",
  signature(x = "extent_crs"),
  function(x) {
    len <- length(x@EPSG)
    if (len == 0) {
      return(cat("Empty extent_crs object"))
    } else if (len == 1) {
      if (x@EPSG == 0) {
        return(cat(
          "Extent: \n",
          "EPSG: NA\n",
          "xmin: NA\n",
          "ymin: NA\n",
          "xmax: NA\n",
          "ymax: NA\n"
        ))
      }
      return(cat(
        "Extent: \n",
        "EPSG: ", st_crs(x@EPSG)$proj4string, "\n",
        "xmin: ", x@xmin, "\n",
        "ymin: ", x@ymin, "\n",
        "xmax: ", x@xmax, "\n",
        "ymax: ", x@ymax, "\n"
      ))
    }
  }
)

#' @rdname show
#' @aliases show
setMethod(
  f = "show",
  signature = "extent_crs",
  definition = function(object) {
    print(object)
  }
)

#' @rdname as.data.frame
#' @aliases as.data.frame,extent_crs
setMethod(
  "as.data.frame",
  signature(x = "extent_crs"),
  function(x) {
    return(data.frame(
      EPSG = x@EPSG,
      xmin = x@xmin,
      ymin = x@ymin,
      xmax = x@xmax,
      ymax = x@ymax
    ))
  }
)

#' @rdname c
#' @aliases c
#' @importFrom methods slot getSlots
setMethod(
  f = "c",
  signature("extent_crs"),
  definition = function(x, ...) {
    args <- list(...)

    for (z in args) {
      if (length(x) == 0) {
        x <- z
        next
      }
      for (s in names(getSlots("extent_crs"))) {
        slot(x, s) <- c(slot(x, s), slot(z, s))
      }
    }
    return(x)
  }
)

#' @aliases sub,extent_crs
#' @rdname sub
setMethod(
  f = "[", signature = "extent_crs",
  definition = function(x, i) {
    for (s in names(getSlots("extent_crs"))) {
      slot(x, s) <- slot(x, s)[i]
    }
    return(x)
  }
)

#' @aliases sub,extent_crs,extent_crs
#' @rdname sub
setReplaceMethod(
  f = "[",
  signature = "extent_crs",
  definition = function(x, i, value) {
    for (s in names(getSlots("extent_crs"))) {
      slot(x, s)[i] <- slot(value, s)[i]
    }
    return(x)
  }
)

#' @rdname length
setMethod(
  f = "length",
  signature = "extent_crs",
  definition = function(x) {
    return(length(x@EPSG))
  }
)

#' @aliases extent,extent_crs
#' @importFrom raster extent
setMethod(
  f = "extent",
  signature = "extent_crs",
  definition = function(x) {
    return(extent(x@xmin, x@xmax, x@ymin, x@ymax))
  }
)


#' @importFrom terra ext
setMethod(
  f = "ext",
  signature = "extent_crs",
  definition = function(x) {
    return(ext(c(x@xmin, x@xmax, x@ymin, x@ymax)))
  }
)

setMethod(
  f = "crs",
  signature = "extent_crs",
  definition = function(x) {
    if(x@EPSG==54008){
      return("ESRI:54008")
    }
    return(x@EPSG)
  }
)
