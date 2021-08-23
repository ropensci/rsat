rsat 0.1.15 (2021-08-18)
=========================
### NEW FEATURES
  
  * Added database path to global environment with the function `set_database("character")` and `get_database()`
  * Added `get_SpatRaster` function to get `SpatRaster` object from `terra` package 

### MINOR IMPROVEMENTS

  * Improved the examples in documentation
  * Vignettes enumeration
  * Improved the performance of search function
  * Fixed unique in records
  * Fixed the warnings produced by mosaic and derive functions
  * Updated `sf` objects
  * Use `terra` package instead of `raster` 

### BUG FIXES

  * Fixed error searching modis images

### DEPRECATED AND DEFUNCT

  * Function name changes `sat_search()` -> `rsat_search()`, `download()` -> `rsat_download()`,
  `mosaic()` -> `rsat_mosaic()`, `derive()` -> `rsat_derive()`

### DOCUMENTATION

  * Added rsat package section


rsat 0.1.14 (2021-01-01)
=========================

### NEW FEATURES

  * ROpenSci review version
