rsat 0.1.15 (2021-08-18)
=========================
### NEW FEATURES
  
  * Added database path to global environment with the function 
  `set_database("character")` and `get_database()`
  * Added `get_SpatRaster` function to get `SpatRaster` 
  class from `terra` package 

### MINOR IMPROVEMENTS

  * Improved the examples in the documentation
  * Added vignettes enumeration
  * Improved the performance of search function
  * Fixed the warnings produced by mosaic and derive functions
  * Updated `sf` objects to the last version
  * Use `terra` package instead of `raster` internally

### BUG FIXES

  * Fixed error searching modis images
  * Fixed `unique` function in records
  
### DEPRECATED AND DEFUNCT

  * Function name change `sat_search()` -> `rsat_search()`
  * Function name change `download()` -> `rsat_download()`
  * Function name change `mosaic()` -> `rsat_mosaic()` 
  * Function name change `derive()` -> `rsat_derive()`
  * Function name change `cloud_mask()` -> `rsat_cloudMask()`
  * Function name change `smoothing_images()` -> `rsat_smoothing_images()`
  * Function name change `list_data()` -> `rsat_list_data()`
  * Function name change `get_raster()` -> `rsat_get_raster()`
  * Function name change `get_stars()` -> `rsat_get_stars()`

### DOCUMENTATION

  * Added rsat package section


rsat 0.1.14 (2021-01-01)
=========================

### NEW FEATURES

  * ROpenSci review version
