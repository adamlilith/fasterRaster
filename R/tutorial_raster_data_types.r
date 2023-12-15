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
#' Floating-point values are accurate to about the 6th to 9th decimal place. These are called "float" rasters in **fasterRaster** and `FCELL` rasters in **GRASS**. By default, operations conducted using raster math (e.g., `raster1 * raster2`) return double floating-point valued rasters, but this can be changed to floating point precision using [faster()] (e.g., `faster(rasterPrecision = "float")`). Floating-point rasters typically take more memory than integer rasters, but less than double-floating point rasters.
#'
#' ## Double-floating point values
#' Double-floating point values are accurate to about the 15th to 17th decimal place. These are called "double" rasters in **fasterRaster** and `DCELL` rasters in **GRASS**. These rasters typically take the most memory. All "`numeric`" values in **R** are double-floating point values.
#' 
#' ## Categorical rasters
#' Categorical rasters (also called "factor" rasters) are actually integer rasters, but have an associated attribute table that maps each integer value to a category label, such as "wetland" or "forest". The table has at least two columns. The first is integer values, and (by default) the second is category names. This second column is the "active" category column, but can be changed using \code{\link[fasterRaster]{activeCat<-}}.
#'
#' ## Functions relevant to raster data types
#' * [activeCat()] and \code{\link[fasterRaster]{activeCat<-}} can be used to see or assign which column in a "levels" table associated with a categorical raster is used as category labels.
#' * [addCats()] adds information to the "levels" table using [data.table::merge()] (same as [merge()]).
#' * \code{\link[fasterRaster]{addCats<-}} add new levels to a "levels" table.
#' * [as.int()], [as.float()], and [as.doub()] coerce a raster to an integer, float, or double.
#' * [catNames()] reports the column names of the "levels" table of each layer of a raster.
#' * [cats()] returns the entire "levels" table of a categorical raster.
#' * [combineCats()] combines levels of two or more categorical or integer rasters.
#' * [complete.cases()] finds rows in the levels table that have no `NA`s.
#' * [datatype()] returns the data type of a `GRaster`.
#' * [droplevels()] removes "unused" levels in a "levels" table.
#' * [freq()]: Frequency of each category across cells of a raster\cr
#' * [is.factor()] indicates if the raster is a categorical raster.
#' * [is.int()], [is.float()], and [is.doub()] indicate if values in a a raster are integers, floating-point, or double-floating point precision.
#' * [levels()] returns the "levels" table of a categorical raster (just the value column and the active column).
#' * \code{\link[fasterRaster]{levels<-}} and [categories()] can be used to assign categories to an integer raster and make it categorical (i.e., a "factor" raster).
#' * [match()], \code{\link[fasterRaster]{%in%}}, and \code{\link[fasterRaster]{%notin%}}: Find which cells of a `GRaster` match or do not match certain category labels\cr
#' * [missing.cases()] finds rows in the levels table that have at least one `NA`.
#' * [missingCats()] finds values in categorical rasters that do not have a category assigned to them.
#' * [nlevels()] returns the number of levels represented by a categorical raster.
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
