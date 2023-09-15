#' @name tutorial_undocumented_functions
#'
#' @title Undocumented functions
#'
#' @description **fasterRaster** has a number of undocumented functions. These are used internally, but may be of utility to power-users, so are documented here.
#' 
#' ## `.copyGSpatial()`
#' Creates in **GRASS** a copy of a `GSpatial` object (typically, a `GRaster` or a `GVector`).
#'
#* `x`: `GRaster`, `GVector`, or character: The object or the `sources` of the object to be copied. Can take multi-layered objects or multiple `sources`.
#' 
#' ## `dbCat()`
#' Get a `data.table` with a single column named `cat`, which corresponds to the **GRASS** attribute table's `cat` column.
#'
#' `x`: A `GVector`.
#
#' ## `dbToDataTable()`
#' Convert the attribute table linked to a vector in **GRASS** to a `data.table`. This table is distinct from the attribute table attached to a `GVector`.
#'
#' `x`: A `GVector`.
#'
#' ## `dbRemove`
#' Delete an attribute table linked to a vector in **GRASS**. This table is distinct from the attribute table attached to a `GVector`. Note that even the `cat` column, which is necessary for identifying individual features, is dropped.
#'
#' `x`: A `GVector`.
#'
#' ## `.layerIndex()`
#' Gets the index of raster layers from a vector.
#'
#' `layer`: Integer, numeric, logical, or character: Refers to one or more layers.
#'
#' `x`: A `GRaster`.
#' `recycle` Logical: If `TRUE` (default), and `layer` is logical and smaller in number than the number of layers, then recycle the vector of `layer`.
#' 
#' ## `.ls()`
#' Lists the `sources` of all objects in the current **GRASS** session.
#'
#' `type`: One or more of `"rasters"`, `"vectors"`, `"rasters3d"`, `"groups"`, or missing: Type of object(s) to display. If missing, all objects are displayed.
#'
#' ## `.makeSourceNames()`
#' Makes one or more statistically unique strings that can be used as file names to represent rasters or vectors in **GRASS**.
#'
#' `x`: Missing, character, `SpatRaster`, `SpatVector`, `sf`, `GRaster`, or `GVector`. Object from which to make the `source` (if possible).
#'
#' `rastOrVect`: Character: `"raster"` or `"vector"` (partial matching is used). Type of `source` to make.
#'
#' `n`: Numeric integer: Number of `sources` to make.
#' 
#' ## `.makeGRaster()` and `.makeGVector()`
#' Make `GRaster`s or `GVector`s from a vector of `sources`, which is a pointer to files in **GRASS**.
#'
#' `src`: Character vector of `sources`.
#'
#' `name`: Character (rasters only): Name(s) of the `GRaster`s.
#' 
#' ## `.minVal()` and `.maxVal()`
#' Get the value of the `@minVal` and `@maxVal` slot in a `GRaster`.
#'
#' `x`: A `GRaster`.
#' 
#' ## `.nlevels()`
#' Count number of levels in a `SpatVector`, `data.frame`, `data.table`, empty string, or a list of `data.frame`s, `data.table`s, and/or empty strings.
#'
#' `x`: A `GRaster`, `data.frame`, `data.table`, empty string, or a list thereof.
#' 
#' ## `.projection()`
#' Get the value of the `@projection` slot in a `GRaster` or `GVector`.
#'
#' A `GRaster` or `GVector`.
#' 
#' ## `.refresh()`
#' Refresh metadata held within a `GRaster` or `GVector`` by querying the object in **GRASS**.
#'
#' `x`: A `GRaster` or `GVector`.
#' 
#' ## `.rename()`
#' Rename a **GRASS** raster or vector.
#'
#' `from` and `to`: Character: The `sources` of the object to be renamed.
#'
#' `rastOrVect`: Character: Either `"raster"` or `"vector"` or `NULL`. Type of object to rename. If `NULL`, function will try to determine if `from` is a raster or vector (slower).
#' 
#' ## `.rm()`
#' Delete all **GRASS** files representing rasters or vectors.
#'
#' `x`: A `GRaster`, `GVector`, character, or missing (default): The `source`(s) of the object(s) to be deleted, or objects from which `source`(s) can be obtained. If missing, all objects will be deleted (but their type can specified using `type`).
#'
#' `type`: Character: `"rasters"`, `"vectors"`, `"rasters3d"`, `"groups"`, or `NULL` (default). Type(s) of objects to delete. If `NULL`, and `x` is missing, all objects will be deleted.
#'
#' `warn`: Logical: If `TRUE` (default), ask user before deleting everything (`x` is missing).
#' 
#' ## `.rastInfo()` and `.vectInfo()`
#' Metadata for a **GRASS** raster or vector.
#'
#' `x`: A `GRaster`, `GVector`, or character vector representing `sources`.
#'
#' @keywords tutorial
NULL
