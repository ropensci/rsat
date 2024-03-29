% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtoi.R
\name{rsat_get_raster}
\alias{rsat_get_raster}
\alias{rsat_get_raster,rtoi-method}
\alias{rsat_get_raster,rtoi}
\alias{rsat_get_SpatRaster}
\alias{rsat_get_SpatRaster,rtoi-method}
\alias{rsat_get_SpatRaster,rtoi}
\alias{rsat_get_stars}
\alias{rsat_get_stars,rtoi-method}
\alias{rsat_get_stars,rtoi}
\title{Loads into R a time series of images regarding an rtoi, satellite product,
and remote sensing index.}
\usage{
rsat_get_raster(x, p, v, s, ...)

\S4method{rsat_get_raster}{rtoi}(x, p, v, s, ...)

rsat_get_SpatRaster(x, p, v, s, ...)

\S4method{rsat_get_SpatRaster}{rtoi}(x, p, v, s, ...)

rsat_get_stars(x, p, v, s, ...)

\S4method{rsat_get_stars}{rtoi}(x, p, v, s, ...)
}
\arguments{
\item{x}{an rtoi.}

\item{p}{a character with the name of the satellite data product.}

\item{v}{a character with the name of the index.}

\item{s}{a character with the name of the stage wanted.}

\item{...}{additional arguments.}
}
\value{
a raster stack.
}
\description{
Loads into R a time series of images regarding an rtoi, satellite product,
and remote sensing index.
}
\examples{
\dontrun{
library(rsat)
# load example rtoi
file.copy(from=system.file("ex/PamplonaDerived",package="rsat"),
         to=tempdir(),
         recursive = TRUE)

# load example rtoi
pamplona.derived <- read_rtoi(file.path(tempdir(),"PamplonaDerived"))

# print available variables
rsat_list_data(pamplona.derived)

# get RasterStack from raster package
suppressWarnings(mod.ndvi.raster <-
           rsat_get_raster(pamplona.derived, "mod09ga", "NDVI"))
plot(mod.ndvi.raster)

# get spatraster from terra package
mod.ndvi.rast <- rsat_get_SpatRaster(pamplona.derived, "mod09ga", "NDVI")
plot(mod.ndvi.rast)

# get stars from stars package
suppressWarnings(mod.ndvi.stars <-
rsat_get_stars(pamplona.derived, "mod09ga", "NDVI"))
plot(mod.ndvi.stars)


## get any band in rtoi
# list available data
rsat_list_data(pamplona.derived)
# select band 1: MODIS_Grid_500m_2D_sur_refl_b01_1
mod.ndvi.rast <- rsat_get_SpatRaster(pamplona.derived,
                                     "mod09ga",
                                     "MODIS_Grid_500m_2D_sur_refl_b01_1")
plot(mod.ndvi.rast)
}
}
