#' @title Number of rows, columns, depths, cells, and layers
#'
#' @description
#' Number of rows, columns, depths, and cells of a `GRegion`:
#' * `dim()`: Rows, columns, depths, and layers of a
#' * `nrow()`: Rows
#' * `ncol()`: Columns
#' * `ndepth()`: Depths (for 3-dimensional rasters only)
#' * `ncell()`: Number of cells (2 dimensions)
#' * `ncell3d()`: Number of cells (3 dimensions)
#'
#' For `GRaster`s: As above, plus number of cells and layers:
#' * `nlyr()`: Layers (number of "stacked" rasters--different from depths of a raster).
#'
#' For `GVector`s: Number of geometries and fields (columns):
#' * `dim()`: Number of geometries and fields
#' * `nrow()`: Number of geometries
#' * `ncol()`: Number of fields (columns)
#'
#' @param x A `Gregion`, `GRaster`, or `GVector`.
#'
#' @return A numeric value or vector.
#' 
#' @seealso [terra::dim()]
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases dim
#' @rdname dim
#' @exportMethod dim
methods::setMethod(
	f = 'dim',
	signature = 'GRegion',
	definition = function(x) c(rows = x@dimensions[1L], cols = x@dimensions[2L], depths = x@dimensions[3L])
)

#' @aliases dim
#' @rdname dim
#' @exportMethod dim
methods::setMethod(
	f = 'dim',
	signature = 'GRaster',
	definition = function(x) c(rows = x@dimensions[1L], cols = x@dimensions[2L], depths = x@dimensions[3L], nlyr = x@nLayers)
)

#' @rdname dim
#' @aliases nrow
#' @exportMethod nrow
setMethod(
	f = 'nrow',
	signature = 'GRegion',
	definition = function(x) unname(x@dimensions[1L])
)

#' @rdname dim
#' @aliases ncol
#' @exportMethod ncol
setMethod(
	f = 'ncol',
	signature = 'GRegion',
	definition = function(x) unname(x@dimensions[2L])
)

#' @rdname dim
#' @aliases ndepth
#' @exportMethod ndepth
setMethod(
	f = 'ndepth',
	signature = 'GRegion',
	definition = function(x) unname(x@dimensions[3L])
)

#' @rdname dim
#' @aliases ncell
#' @exportMethod ncell
setMethod(
	f = 'ncell',
	signature = 'GRegion',
	definition = function(x) prod(x@dimensions[1L:2L])
)

#' @rdname dim
#' @aliases ncell3d
#' @exportMethod ncell3d
setMethod(
	f = 'ncell3d',
	signature = 'GRegion',
	definition = function(x) prod(x@dimensions[1L:3L])
)

#' @aliases dim
#' @rdname dim
#' @exportMethod dim
methods::setMethod(
	f = 'dim',
	signature = 'GVector',
	definition = function(x) c(geometries = x@nGeometries, fields = x@nFields)
)

#' @rdname dim
#' @aliases nrow
#' @exportMethod nrow
setMethod(
	f = 'nrow',
	signature = 'GVector',
	definition = function(x) x@nGeometries
)

#' @rdname dim
#' @aliases ncol
#' @exportMethod ncol
setMethod(
	f = 'ncol',
	signature = 'GVector',
	definition = function(x) x@nFields
)

#' @rdname dim
#' @aliases nlyr
#' @exportMethod nlyr
setMethod(
	f = 'nlyr',
	signature = 'GRaster',
	definition = function(x) x@nLayers
)
