#' Name of a raster or vector in a GRASS session
#'
#' @description `sources()` retrieves the name of a raster or vector in **GRASS**. `GRaster`s and `GVector`s are actually pointers to objects stored in a **GRASS** database. When using **fasterRaster** functions on rasters and vectors, the commands are translated into **GRASS** commands and executed on the objects named in the pointers. These objects use a "source" (which is really a filename) to refer to the **GRASS** objects. This function is mostly of use to developers.
#'
#' @param x Either a `GSpatial` object or one that inherits from it (i.e., a `GRaster` or `GVector`), *or* a character. If a character, then the character itself is returned.
#'
#' @returns Character.
#'
#' @example man/examples/ex_GRaster.r
#' 
#' @aliases sources
#' @rdname sources
#' @exportMethod sources
methods::setMethod(
	f = "sources",
	signature = "GRaster",
	definition = function(x) x@sources
)

#' @aliases sources
#' @rdname sources
#' @exportMethod sources
methods::setMethod(
	f = "sources",
	signature = "GVector",
	definition = function(x) x@sources
)

#' @aliases sources
#' @rdname sources
#' @exportMethod sources
methods::setMethod(
	f = "sources",
	signature = "character",
	definition = function(x) x
)
