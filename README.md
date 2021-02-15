
# rsat

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/rsat)](https://cran.r-project.org/web/packages/rsat/)
[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

The goal of `rsat` is to help you handling time-series of satellite
images from multiple platforms in a local, efficient, and standardized
way. The package provides tools to;

1.  Search
2.  Download
3.  Customize, and
4.  Process

satellite images from Landsat, MODIS, and Sentinel for a region and a
time of interest.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("unai-perez/stars")
devtools::install_github("spatialstatisticsupna/rsat")
```

### Linux

In Linux, you need to install additional libraries before starting with
`rsat`. Use the following commands for:

-   **Debian/Ubuntu**

<!-- -->

        sudo apt update sudo apt install r-cran-rcpp gdal-bin libgdal-dev libproj-dev libssl libssl-dev xml2 libxml2-dev libmagick++-dev

-   **RedHat/Fedora**

<!-- -->

    sudo dnf install gdal gdal_devel proj_devel xml2 libxml2_devel libcurl_devel openssl_devel ImageMagick-c++\_devel

## Log-in profiles

The registration in the following online portals is required to get a
full access to satellite images;

-   [EarthData](https://ers.cr.usgs.gov/register/): A repository of
    NASA’s earth observation data-sets. More information about EarthData
    can be found
    [here](https://earthdata.nasa.gov/earth-observation-data).
-   [SciHub](https://scihub.copernicus.eu/dhus/#/self-registration), a
    web service giving access to Copernicus’ scientific data hub. Please
    go [here](https://scihub.copernicus.eu/) to find more details about
    the data hub.

## Example

This is a basic example which shows you how to compute the Normalized
Difference Vegetation Index from a MODIS captured on January 11th, 2020
in northern Spain (Navarre):

``` r
library(rsat)

set_credentials("username", "password")

roi <- ex.navarre
toi <- as.Date("2020-01-11")
rtp <- tempdir()
dbp <- file.path(tempdir(), "DATABASE")
navarre <- new_rtoi("Navarre", roi, rtp, dbp)

sat_search(region = navarre, product = "mod09ga", dates = toi)
download(navarre)
mosaic(navarre, overwrite = TRUE)
derive(navarre, "NDVI", product = "mod09ga")
```

## Citation

``` r
citation(rsat)[1]
```

To cite the package:

U Pérez-Goya, M Montesino-SanMartin, A F Militino, M D Ugarte (2020).
rsat: Handling Multiplatform Satellite Images. R package version 0.1.3.
<https://CRAN.R-project.org/package=rsat>.

A BibTeX entry for LaTeX users is

@Manual{, title = {rsat: Handling Multiplatform Satellite Images},
author = {U P{'e}rez-Goya and M Montesino-SanMartin and A F Militino and
M D Ugarte}, year = {2020}, note = {R package version 0.1.3}, url =
{<https://CRAN.R-project.org/package=rsat>}, }
