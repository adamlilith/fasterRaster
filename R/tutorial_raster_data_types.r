#' @name tutorial_raster_data_types
#'
#' @title Raster data types
#'
#' @description **fasterRaster** `GRaster`s can represent three numeric data types and one categorical type.
#'
#' ## Integers
#' Rasters that represent integers are called "integer" rasters in **fasterRaster** and `CELL` rasters in **GRASS**. You can force a raster to be an integer using [as.int()]. Some of the functions in [app()] function will also return integer-type rasters. Integer rasters typically take the least memory.
#'
#' ## Floating-point values
#' Floating-point values are accurate to about the 6th to 9th decimal place. These are called "float" rasters in **fasterRaster** and `FCELL` rasters in **GRASS**. By default, operations conducted using raster math (e.g., `raster1 * raster2`) return floating-point valued rasters, but this can be changed to double-floating point precision using [setFastOptions()] (e.g., `setFastOptions(rasterDataType = "double")`). Floating-point rasters typically take more memory than integer rasters, but less than double-floating point rasters.
#'
#' ## Double-floating point values
#' Double-floating point values are accurate to about the 15th to 17th decimal place. These are called "double" rasters in **fasterRaster** and `DCELL` rasters in **GRASS**. These rasters typically take the most memory. All "`numeric`" values in **RR** are double-floating point values. **R** does not use single floating-point values.
#' 
#' ## Categorical rasters
#' Categorical rasters are actually integer rasters, but have an associated attribute table that maps each integer value to a category label, such as "wetland" or "forest".
#'
#' ## Functions relevant to raster data types
#' * [datatype()] returns the data type of a `GRaster`.
#' * [as.int()], [as.float()], and [as.doub()] coerces a raster to an integer, float, or double.
#' * [is.int()], [is.float()], and [is.doub()] indicate if the raster is an integer, float, or double raster.
#' * [is.factor()] indicates if the raster is a categorical raster.
#' * [levels()] returns the attribute table of a categorical raster (or a blank list if the raster is not categorical).
#' * [nlevels()] returns the number of categorical values represented by a categorical raster.
#' * [levels()<-] and [categories()] can be used to coerce an integer raster to a categorical raster and assign categories.
#' * [activeCat()] and [activeCat()<-] can be used to see or assign which column in a "levels" table associated with a categorical raster is used as category labels.
#' * [droplevels()] can be used to remove "unused" levels in a levels table.
#' * [as.int()], [as.float()], and [as.doub()] can be used to convert a raster to an integer, float, or double-floating precision raster (and remove levels in the process).
#'
#' ## Saving rasters to disk
#' You can save substantial space on disk if you set the `datatype` argument in [writeRaster()] when saving a raster. This argument allows for finer "divisions" than just integer/float/double-float, so depending on the range of values in your raster, you can optimize file size by selecting the one that best matches the values in the raster. See the documentation for [writeRaster()] for more information.
#'
#'    | **`fasterRaster`** | **`terra`** | **`GRASS`** | **`GDAL`** | **Values** |
#'    | ------------------ | ----------- | ----------- | ---------- | ------ |
#'    | `integer`          | `INT1U`     | `CELL`      | `Byte`     | Integer values from 0 to 255 |
#'    | `integer`          | `INT2U`     | `CELL`      | `UInt16`   | Integer values from 0 to 65,534 |
#'    | `integer`          | `INT2S`     | `CELL`     | `Int16`    | Integer values from -32,767 to -32,767 |
#'    | `integer`          | `INT4S`     | `CELL`     | `Int32`    | Integer values from -2,147,483,647 to 2,147,483,647 |
#'    | `float`            | `FLT4S`     | `FCELL`   | `Float32`    | Values from -3.4e+38 to 3.4e+38, including decimal values |
#'    | `double`           | `FLT8S`     | `DCELL`   | `Float64`    | Values from -1.7e+308 to 1.7e+308, including decimal values |
#'    | `factor`           | `INT`*      | `CELL`    | *            | Integer values corresponding to categories
#'
#' `*` Depends on the integers (signed/unsigned, range of values)
#' 
#' @keywords tutorial
#' @example man/examples/ex_GRaster_GVector.r
NULL
