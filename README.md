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
results in a stack of satellite images ready-to-use. Due to the wide variety
of procedures and sources of information being handled in rsat, the
functions are divided into 7 categories, which are identified by the first 3
characters of the function names; 

1. ```mod``` identifies Modis Terra and Aqua satellite functions.
2. ```sen``` identifies Sentinel functions.
3. ```ls7``` identifies Landsat 7 functions.
4. ```ls8``` identifies Landsat 8 functions.
5. ```ls``` identifies both Landsat 7 and 8 functions.
6. ```gen``` identifies function for being used in any of the three platforms.
7. ```var``` identifies function for deriving variables in any of the three platforms.

Below, there is a list of the most important functions grouped by platform,
and listed in operational order. These functions include searching, previewing,
downloading, mosaicking, deriving new variables, compositing, cloud masking
and filling/smoothing satellite imagery.

## I. Landsat functions
The Landsat program is currently releasing imagery captured by two satellites;
the Landsat-7 and Lansat-8. Both satellites are treated separately in coding
terms due to discrepancies in their spectral coverages and data formats. To
download Landsat imagery with the following functions, a USGS's EarthExplorer
account is required. Please, register [here](https://ers.cr.usgs.gov/register/).

### Landsat-7

* ```ls7LoadMetadata``` Loads the Landsat-7 metadata file.
* ```ls7Search``` Seeks a time series of Landsat-7 images.
* ```lsPreview``` Previews Landsat satellite images.
* ```lsDownSearch``` Downloads a time series of Landsat images.
* ```lsMosaic``` Mosaics Landsat images.
* ```ls7FolderToVar``` Computes new variables from Landsat-7 multispectral images.
* ```lsCloudMask``` Creates cloud masks for Landsat images.
* ```genSaveTSRData``` Saves a time series of images.

### Landsat-8
* ```ls8LoadMetadata``` Loads the Landsat-7 metadata file.
* ```ls8Search``` Seeks a time series of Landsat-7 images.
* ```lsPreview``` Previews Landsat satellite images.
* ```lsDownSearch``` Downloads a time series of Landsat images.
* ```lsMosaic``` Mosaics Landsat images.
* ```ls8FolderToVar``` Computes new variables from Landsat-7 multispectral images.
* ```lsCloudMask``` Creates cloud masks for Landsat images.
* ```genSaveTSRData``` Saves a time series of images.

## II. MODIS functions
Functions in rsat download all land products from Terra and Aqua 
satellites, but the processing focuses on the multispectral images. Be aware
that an EarthData account is required to use NASA's web service so, please,
register [here](https://urs.earthdata.nasa.gov/users/new).

* ```modSearch``` Seeks a time series of MODIS images.
* ```modPreview``` Previews MODIS satellite images.
* ```modDownSearch``` Downloads a time series of MODIS images.
* ```modMosaic``` Mosaics MODIS images.
* ```modFolderToVar``` Computes new variables from MODIS multispectral images.
* ```modCloudMask``` Creates cloud masks for MODIS images.
* ```genSaveTSRData``` Saves a time series of images.

## III. Sentinel functions
Sentinel archives provide a wide variety of products based on a 5-satellite
constellation. The functions to download Sentinel images can cope with any
product available in ESA's SciHub web service. However, image processing is
focused on Sentinel-2 multispectal images. SciHub credentials are required to
download Sentinel imagery and can be obtained 
[here](https://scihub.copernicus.eu/dhus/#/self-registration).

* ```senSearch``` Seeks a time series of Sentinel images.
* ```senPreview``` Previews Sentinel images.
* ```senDownSearch``` Downloads a time series of Sentinel images.
* ```senMosaic```  Mosaics Sentinel images.
* ```senCloudMask```  Creates cloud masks for Sentinel images.
* ```senFolderToVar``` Computes new variables from Sentinel-2 multispectral images.
* ```genSaveTSRData``` Saves a time series of images.

## IV. Important general functions
In addition to functions above, the package provides some general functions
for a better data handling:

* ```genCompositions``` Creates image compositions from a time series of satellite images.
* ```genSmoothingIMA``` Fills the gaps and smooths outliers in a time series of satellite images.
* ```genSmoothingCovIMA``` Fills the gaps and smooths outliers in a time series of satellite images using covariates.
* ```genPlotGIS```  Plots satellite images with a proper GIS format.
* ```genGetDates``` Gets the capturing date of an image from the name of a raster layer.


## V. Remote sensing variables 
New variables can be derived from multispectral images. The most common
variables in the scientific literature are pre-programmed in rsat. They
can be identified by the prefix "var".

* ```varEVI``` Calculates the enhanced vegetation index (EVI).
* ```varMSAVI2``` Calculates the modified soil-adjusted vegetation index (MSAVI2).
* ```varNBR``` Calculates the normalized burn ratio (NBR).
* ```varNBR2``` Calculates the normalized burn ratio 2 (NBR2).
* ```varNDMI``` Calculates the normalized difference moisture index (NDMI).
* ```varNDVI``` Calculates the normalized difference vegetation index (NDVI).
* ```varNDWI```  Calculates the normalized difference water index (NDWI).
* ```varRGB```  Calculates an RGB image from 3 spectral bands.
* ```varSAVI```  Calculates the soil-adjusted vegetation index (SAVI).


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
