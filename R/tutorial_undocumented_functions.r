#' @name tutorial_undocumented_functions
#'
#' @title Undocumented functions
#'
#' @description **fasterRaster** has a number of undocumented functions. These are used internally, but may be of utility to power-users, so are documented here.
#' 
#' ## `.copyGSpatial()`
#' Creates in **GRASS** a copy of a `GSpatial` object (typically, a `GRaster` or a `GVector`).
#' 
#' ## `.dbName()` and `.dbLayer()`
#' **GRASS** names of the data table and data layer associated with a vector.
#'
#' ## `.gnames()`
#' Gets the name(s) of the file(s) in **GRASS** that represent(s) the object(s).
#'
#' ## `.ls()`
#' Lists the `gnames` of all objects in the current **GRASS** session.
#'
#' ## `.makeGNames()`
#' Makes one or more statistically unique strings that can be used as file names to represent rasters or vectors in **GRASS**.
#' 
#' ## `.makeGRaster()` and `makeGVector()`
#' Make `GRaster`s or `GVector`s from a vector of `gname`(s), which point to files in **GRASS**.
#' 
#' ## `.projection()`
#' Get the value of the `@projection` slot in a `GRaster` or `GVector`.
#' 
#' ## `.refresh()`
#' Refresh metadata held within a `GRaster` or `GVector`` by querying the object in **GRASS**.
#' 
#' ## `.rename()`
#' Rename a **GRASS** raster or vector.
#' 
#' ## `.rm()`
#' Delete all **GRASS** files representing rasters or vectors.
#' 
#' ## `.rastInfo()` and `.vectInfo()`
#' Metadata for a **GRASS** raster or vector.
#'
#' @keywords tutorial
NULL
