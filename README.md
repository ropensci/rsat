# rsat 
Handling multiplatform satellite images.

[![CRAN version](https://www.r-pkg.org/badges/version/rsat)](https://cran.r-project.org/web/packages/rsat/)
[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
## Table of contents

- [The package](#the-package)
- [Installation](#installation)
- [Credentials for downloading satellite images](#credentials-for-downloading-satellite-images)
- [Copyright and license](#copyright-and-license)


# The package
This package enables you downloading, customizing, and processing time series of
satellite images from Landsat, MODIS and Sentinel in a standardized way. Some
functions download and convert automatically the platform-specific file formats
into GTiff, so they can be loaded in R. The customization functions support tile
mosaicking, cropping, cloud masking and deriving new variables of interest,
such as the NDVI, EVI, etc. Tile mosaicking is required when the region of
interest extends over several tiles, so they can be combined into a single
image. Cropping involves removing the pixels outside the region of interest,
making any analysis more computationally and memory efficient. Cloud masking
eliminates cloud reflectance that would otherwise be erroneously attributed
to land surface features. Cloud removal and (measurement or processing) errors
trigger data gaps and outliers, decreasing the quality and quantity of 
measurements. Hence, the package includes a set of function for filling and
smoothing the satellite imagery. The combination of functions in rsat
results in a stack of satellite images ready-to-use. 


# Installation
## Install from CRAN
Not in CRAN

## Install from GitHub
```
# Install devtools package from cran repository
install.packages("devtools")

# load devtools library
library(devtools)

# Install rsat from GitHub repositoy
install_github("spatialstatisticsupna/rsat")
```
## Dependencies for linux
The package depends on some R packages that in Linux requires the installation of some libraries before the installation in R. Here you have the command to install all the applications from repository for Debian/Ubuntu and RedHat/Fedora.
### Debian/Ubuntu
```
sudo apt update
sudo apt install r-cran-rcpp gdal-bin libgdal-dev libproj-dev libssl libssl-dev xml2 libxml2-dev libmagick++-dev
```
### RedHat/Fedora
```
sudo dnf install gdal gdal_devel proj_devel xml2 libxml2_devel libcurl_devel openssl_devel ImageMagick-c++_devel
```

# Credentials for downloading satellite images
### Modis
Credentials [EarthData](https://ers.cr.usgs.gov/register/) 

### Landsat
Credentials [EarthData](https://ers.cr.usgs.gov/register/) 

### Sentinel
Credentials [SciHub](https://scihub.copernicus.eu/dhus/#/self-registration) 

## Copyright and license
Licensed under the GPL-3 License. [Full license here](/LICENSE.md).
