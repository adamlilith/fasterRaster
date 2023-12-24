#' @name tutorial_regions
#'
#' @title An explanation of GRASS "regions"
#'
#' @description A **GRASS** *region* is a data structure like a raster in that it is composed of "cells", but different in that these cells do not contain values. Rather, their resolution and the extent of the region influence how rasters are imported, created, processed, and exported. In most cases, whenever a raster undergoes one of these processes using a **GRASS** module, the raster will be resampled and/or crop/extend it so that matches the region"s extent and resolution. If ignored, this can cause unintended side effects if the region's geometry doesn't match the raster being processed.
#'
#' Generally, most users of **fasterRaster** will not need to know how regions work because their management is handled automatically.  This help page is provided to assist power users who may wish to use regions explicitly or develop their own applications based on **fasterRaster**.
#'
#' Each **GRASS** ["location"][tutorial_locations_mapsets] typically has one active region. The extent and resolution of this region is initially set by the first raster that is imported into it.
#'
#' ### Functions for managing regions in **fasterRaster**
#'
#' **fasterRaster** provides functions for managing regions:
#'
#' * `.region(): Extent *and* dimensions/resolution of a region. This function is used by many other **fasterRaster** functions before raster processing to ensure the raster is not changed (by the region).
#' * `.regionDim()`: Dimensions (number of rows and columns)
#' * `.regionExt()`: Extent
#' * `.regionRes()`: Resolution
#'
#' Most of these functions can be used in three different ways:
#' * No arguments: The function reports the respective value(s) (e.g., `regionExt()` reports the extent of the region).
#' * With a `GRaster`, `GVector`, `SpatRaster`, `SpatVector`, or `sf` object: Resizes and/or resamples the region so it has the same extent and/or resolution as the spatial object. Note that vector objects do not have a resolution, so cannot be used in `.region*()` functions that resample the region's resolution.
#' * With numeric values (i.e., representing extent, resolution, or dimensions): Resize or resample the region so it has these dimensions. When a region's extent is resized or resolution is changed, it will be extended to have the next-larger number of rows and/or columns so it can accommodate cells of the desired size.
#'
#' Regions are inherently 3-dimensional, but often have a single depth (i.e., one "layer" of cells).
#'
#' @keywords tutorial
#' @example man/examples/ex_regions.r
NULL
