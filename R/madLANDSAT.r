#' @name madLANDSAT
#'
#' @title Rasters of surface reflectance for an eastern portion of Madagascar
#'
#' @description Raster layers of surface reflectance from LANDSAT 9 for an eastern portion of Madagascar taken May 21, 2023. Four bands are represented:
#' * `band2': Blue (450-510 nm)
#' * `band3': Green (530-590 nm)
#' * `band4': Red (640-670 nm)
#' * `band5': Near-infrared (850-880 nm)
#' The rasters have been resampled to 90-m resolution to reduce their size, then rescaled to integers in the range 0 to 255.
#'
#' @docType data
#'
#' @format An object of class `SpatRaster` in Universal Trans-Mercator (UTM), Zone 39 North with a WGS84 coordinate system, at 90 m resolution.
#'
#' @keywords land Madagascar
#'
#' @details *IMPORTANT*: To import this raster into **GRASS** using [fast()], you typically must set the `checkCRS` argument to `FALSE`. If you do this, be sure, though that the [location][tutorial_sessions] has the same coordinate reference system as this raster (WGS84 / UTM 39N).
#'
#' @source United States Geological Survey's [EarthExplorer](https://earthexplorer.usgs.gov). Also see [band definitions](https://www.usgs.gov/faqs/what-are-band-designations-landsat-satellites).
#'
#' @example man/examples/ex_fastData.r
#'
NULL
