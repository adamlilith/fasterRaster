#' @name tutorial_undocumented_functions
#'
#' @title Undocumented functions
#'
#' @description **fasterRaster** interfaces with **GRASS GIS** to process rasters and spatial vector data. It is intended as an add-on to the **terra** and **sf** packages, and relies heavily on them. For most rasters and vectors that are small or medium-sized in memory/disk, those packages will almost always be faster. They may also be faster for very large objects.  But when they aren't, **fasterRaster** can step in.
#'
#' ## Installing **fasterRaster** has a number of undocumented functions. These are used internally, but may be of utility to power-users, so are documented here.
#' 
#' ### `.copyGSpatial()`
#' Creates in **GRASS** a copy of a `GSpatial` object (a `GRaster` or a `GVector`).
#' 
#' ### `dbName()` and `dbLayer()`
#' **GRASS** names of the data table and data layer associated with a vector.
#'
#' ### `.gnames()`
#' Gets the name(s) of the file(s) in **GRASS** that represent(s) the object(s).
#'
#' ### `.ls()`
#' Lists the `gnames` of all objects in the current **GRASS** session.
#'
#' ### `.makeGNames()`
#' Makes one or more statistically unique strings that can be used as file names to represent rasters or vectors in **GRASS**.
#' 
#' ### `.refresh()`
#' Refresh metadata held within a `GRaster` or `GVector`` by querying the object in **GRASS**.
#' 
#' ### `.rename()`
#' Rename a **GRASS** raster or vector.
#' 
#' ### `.rm()`
#' Delete all **GRASS** files representing rasters or vectors.
#' 
#' ### `.rastInfo()`
#' Metadata for a **GRASS** raster.
#'
#' ### `.vectInfo()`
#' Metadata for a **GRASS** vector.
#'
#' @keywords tutorial
NULL
