rsat 0.1.17 (2021-12-13)
=========================
### NEW FEATURES
  * Fixed minor bugs in the code
  * Corrected some examples from the manual
  
rsat 0.1.16 (2021-10-06)
=========================
### NEW FEATURES
  * Added support to new landsat products: landsat_ot_c2_l2, 
  landsat_ot_c2_l1, lsr_landsat_8_c1, landsat_8_c1,landsat_etm_c2_l2, 
  landsat_etm_c2_l1, lsr_landsat_etm_c1, landsat_etm_c1, 
  landsat_tm_c2_l2, landsat_tm_c2_l1, landsat_mss_c2_l1, 
  lsr_landsat_tm_tm_c1, landsat_tm_c1, landsat_mss_c1
  
  
rsat 0.1.15 (2021-09-09)
=========================
### NEW FEATURES
  
  * Added database path to global environment with the function 
  `set_database("character")` and `get_database()`
  * Added `get_SpatRaster` function to get `SpatRaster` 
  class from `terra` package 

### MINOR IMPROVEMENTS

  * Improved the examples in the documentation and added new datasets
  * Added vignettes enumeration
  * Improved the performance of search function
  * Fixed the warnings produced by mosaic and derive functions
  * Updated `sf` objects to the last version
  * Use `terra` package instead of `raster` internally
  * Improved stored `rtoi` files
  * Added `rsat` section in the manual

### BUG FIXES

  * Fixed error searching modis images
  * Fixed `unique` function in records
  * Fixed `rsat_list_data`
  * Fixed multiple errors around the code
  
### DEPRECATED AND DEFUNCT

  * Function name change `sat_search()` -> `rsat_search()`
  * Function name change `download()` -> `rsat_download()`
  * Function name change `mosaic()` -> `rsat_mosaic()` 
  * Function name change `derive()` -> `rsat_derive()`
  * Function name change `preview()` -> `rsat_preview()`
  * Function name change `cloud_mask()` -> `rsat_cloudMask()`
  * Function name change `smoothing_images()` -> `rsat_smoothing_images()`
  * Function name change `list_data()` -> `rsat_list_data()`
  * Function name change `get_raster()` -> `rsat_get_raster()`
  * Function name change `get_stars()` -> `rsat_get_stars()`

### DOCUMENTATION

  * Added rsat package section
  * Added examples in several functions with new datasets
  * Improved documentation in general
  * Grouped some functions in one manual entry


rsat 0.1.14 (2021-01-01)
=========================

### NEW FEATURES

  * ROpenSci review version
