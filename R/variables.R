#' @importFrom terra quantile
variables <- setRefClass("variables",
  # Define the slots
  fields = list(
    bands = "list"
  ),
  methods = list(
    EVI = function(blue, red, nir, scfun = function(r) {
                     r
                   }) {
      blue <- scfun(blue)
      red <- scfun(red)
      nir <- scfun(nir)
      evi <- 2.5 * ((nir - red) / (nir + 6 * red - 7.5 * blue + 1))
      return(evi)
    },
    MSAVI2 = function(red, nir) {
      msavi <- (2 * nir + 1 - sqrt((2 * nir + 1)^2 - 8 * (nir - red))) / 2
      return(msavi)
    },
    NBR = function(nir, swir2) {
      nbr <- (nir - swir2) / (nir + swir2)
      return(nbr)
    },
    NBR2 = function(swir1, swir2) {
      nbr2 <- (swir1 - swir2) / (swir1 + swir2)
      return(nbr2)
    },
    NDMI = function(nir, swir1) {
      ndmi <- (nir - swir1) / (nir + swir1)
      return(ndmi)
    },
    NDVI = function(red, nir) {
      ndvi <- (nir - red) / (nir + red)
      return(ndvi)
    },
    NDWI = function(green, nir) {
      ndwi <- (green - nir) / (green + nir)
      return(ndwi)
    },
    RGB = function(red, green, blue, q.range = c()) {
      rgb <- list(red, green, blue)
      names(rgb) <- c("red", "green", "blue")

      if (!is.null(q.range)) {
        rgb <- lapply(rgb, FUN = function(r, q.range) {
          q <- raster::quantile(r, q.range, na.rm = TRUE)
          r <- clamp(r, lower = q[1], upper = q[2])
          return(r)
        }, q.range)
        names(rgb) <- c("red", "green", "blue")
      }
      rgb <- lapply(rgb, raster::stretch, minv = 0, maxv = 255)
      image <- raster::stack(rgb)
      return(image)
    },
    SAVI = function(red, nir, L = 0.5, scfun = function(r) {
                      r
                    }) {
      red <- scfun(red)
      nir <- scfun(nir)
      savi <- ((nir - red) / (nir + red + L)) * (1 + L)
      return(savi)
    },
    fields = function(x, ...) {
      return(names(.refClassDef@fieldClasses))
    }
  )
)$new(bands = list(
  ls1 = c(green = "B4",
          red = "B5",
          nir = "B6",
          nir2 = "B7",
          tirs1 = "B8",
          quality = "BQA",
          cloud = "CLD"),
  ls2 = c(green = "B4",
          red = "B5",
          nir = "B6",
          nir2 = "B7",
          tirs1 = "B8",
          quality = "BQA",
          cloud = "CLD"),
  ls3 = c(green = "B4",
          red = "B5",
          nir = "B6",
          nir2 = "B7",
          tirs1 = "B8",
          quality = "BQA",
          cloud = "CLD"),
  ls4 = c(blue = "B1",
          green = "B2",
          red = "B3",
          nir = "B4",
          swir1 = "B5",
          tirs1 = "B6",
          swir2 = "B7",
          quality = "BQA",
          cloud = "CLD"),
  ls5 = c(blue = "B1",
          green = "B2",
          red = "B3",
          nir = "B4",
          swir1 = "B5",
          tirs1 = "B6",
          swir2 = "B7",
          quality = "BQA",
          cloud = "CLD"),
  ls7 = c(blue = "B1",
          green = "B2",
          red = "B3",
          nir = "B4",
          swir1 = "B5",
          tirs1 = "B6_VCID_1",
          tirs2 = "B6_VCID_2",
          swir2 = "B7",
          panchromatic = "B8",
          quality = "BQA",
          cloud = "CLD"),
  ls8 = c(bluecoastal = "B1",
          blue = "B2",
          green = "B3",
          red = "B4",
          nir = "B5",
          swir1 = "B6",
          swir2 = "B7",
          panchromatic = "B8",
          cirrus = "B9",
          tirs1 = "B10",
          tirs2 = "B11",
          quality = "BQA",
          cloud = "CLD"),
  mod09ga = c(red = "B01_1",
              nir = "B02_1",
              blue = "B03_1",
              green = "B04_1",
              tirs1 = "B05_1",
              swir1 = "B06_1",
              swir2 = "B07_1",
              quality = "_state_1km_1",
              cloud = "CLD"),
  myd09ga = c(red = "B01_1",
              nir = "B02_1",
              blue = "B03_1",
              green = "B04_1",
              tirs1 = "B05_1",
              swir1 = "B06_1",
              swir2 = "B07_1",
              quality = "_state_1km_1",
              cloud = "CLD"),
  mcd43a4 = c(red = "Nadir_Reflectance_Band1",
              nir = "Nadir_Reflectance_Band2",
              blue = "Nadir_Reflectance_Band3",
              green = "Nadir_Reflectance_Band4",
              swir1 = "Nadir_Reflectance_Band5",
              swir2 = "Nadir_Reflectance_Band6",
              mwir = "Nadir_Reflectance_Band7"),
  "Sentinel-1" = c(""),
  "Sentinel-2" = c(bluecoastal = "B01",
                   blue = "B02",
                   green = "B03",
                   red = "B04",
                   vegrededge = "B05",
                   vegrededge1 = "B06",
                   vegrededge2 = "B07",
                   nir = "B08",
                   narrownir = "B8A",
                   watervapour = "B09",
                   cirrus = "B10",
                   swir1 = "B11",
                   swir2 = "B12",
                   cloud = "CLD",
                   snow = "SNW",
                   rgb = "TCI",
                   preview = "PVI",
                   watervap = "WVP"),
  "Sentinel-3" = c(""),
  "SY_2_SYN___" = c(blue = "Oa04_.tif",
                    green = "Oa06_.tif",
                    red = "Oa08_.tif",
                    nir = "Oa12_.tif")
))

setGeneric("get_var_fun", function(var) {
  standardGeneric("get_var_fun")
})
setMethod(
  "get_var_fun",
  "character",
  function(var) {
    eval(parse(text = paste0("variables$", var)))
  }
)

#' @rdname print-rtoi-method
#' @aliases print,records
setMethod(
  "print",
  "variables",
  function(x, ...) {
    fields <- names(variables$getRefClass()$fields())
    cat("Data and variable methods provided by rsat\n")
    cat("Satellite products: ", paste(names(variables$bands),
                                      collapse = ", "),
        ".\n", sep = "")
    methods <- variables$getRefClass()$methods()
    methods <- methods[!methods %in% c("initFields",
                                       "methods",
                                       "copy",
                                       "callSuper",
                                       ".objectPackage",
                                       "export",
                                       "untrace",
                                       "getClass",
                                       "show",
                                       "usingMethods",
                                       ".objectParent",
                                       "field",
                                       "fields",
                                       "trace",
                                       "getRefClass",
                                       "import")]
    cat("Variable Methods: ", paste(methods, collapse = ", "), ".", sep = "")
  }
)

#' @rdname show
#' @aliases show,records
setMethod(
  "show",
  "variables",
  function(object) {
    print(object)
  }
)

#' List the variables and satellites supported by \code{rsat}
#'
#' Displays the satellites and variable method
#' @param ... arguments for nestering functions
#'
#' @return prints supported satellites and derived variables information.
#' @export
#' @examples
#' show_variables()
setGeneric("show_variables", function(...) {
  standardGeneric("show_variables")
})

#' @rdname show_variables
#' @aliases show_variables-method
setMethod(
  "show_variables",
  c(),
  function() {
    print(variables)
  }
)
