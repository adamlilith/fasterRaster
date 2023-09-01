#' @name tutorial_raster_data_types
#'
#' @title Raster data types
#'
#' @description **fasterRaster** `GRaster`s can represent three numeric data types and one categorical type.
#'
#' ## `CELL`/Integer values
#' Rasters that represent integers are of type `CELL` in **GRASS** and **fasterRaster**. You can force a raster to be an integer using `as.cell()`. Some of the functions in [app()] function will also return integer-type rasters. Integer rasters typically take the least memory.
#'
#' ## `FCELL`/Floating-point values
#' Floating-point values are accurate to about the 7th decimal place. These are called `FCELL` rasters in **GRASS** and **fasterRaster**. By default, operations conducted using raster math (e.g., `raster1 * raster2`) return floating-point valued rasters, but this can be changed using [setFastOptions()] (e.g., `setFastOptions(rasterDataType = "DCELL")`). Floating-point rasters typically take more memory than integer rasters, but less than double-floating point rasters.
#'
#' ## `DCELL`/Double-floating point values
#' Double-floating point values are accurate to about the 15th decimal place, and are called `DCELL` rasters in **GRASS** and **fasterRaster**. These rasters typically take the most memory.
#' 
#' ## Categorical rasters
#' Categorical rasters are actually `CELL` (integer) rasters, but have an associated attribute table that maps each integer value to a category label, such as "wetland" or "forest".
#'
#' ## Functions relevant to raster data types
#' * [datatype()] returns `CELL`, `DCELL`, or `FCELL`.
#' * [is.cell()], [is.fcell()], and [is.dcell()] indicate if the raster is of the given type.
#' * [as.cell()], [as.fcell()], and [as.dcell()] coerces a raster to the given type.
#' * [is.factor()] indicates if the raster is a categorical raster.
#' * [levels()] returns the attribute table of a categorical raster (or a blank list if the raster is not categorical).
#' * [nlevels()] returns the number of categorical values represented by a categorical raster.
#'
#'
#' ## Saving rasters to disk
#' You can save substantial space on disk if you set the `datatype` argument in [writeRaster()] when saving a raster. This argument allows for finer "divisions" than just integer/float/double-float, so depending on the range of values in your raster, you can optimize file size by selecting the one that best matches the values in the raster. See the documentation for [writeRaster()] for more information.
#' 
#' @keywords tutorial
#' @example man/examples/ex_GRaster_GVector.r
NULL
