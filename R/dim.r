#' @title Number of rows, columns, depths, cells, and layers of a GRaster
#'
#' @description
#' For `GRaster`s: Number of rows, columns, depths, cells, and layers:
#' * `dim()`: Rows, columns, depths, and layers of a
#' * `nrow()`: Rows
#' * `ncol()`: Columns
#' * `ndepth()`: Depths (for 3-dimensional rasters only)
#' * `nlyr()`: Layers (number of rasters or vectors). Note that for vectors, this does not count "geometries" (features), but rather independent vectors which are composed of one or more geometries (i.e., points, lines, polygons).
#' * `ncell()`: Number of cells (2 dimensions)
#' * `ncell3d()`: Number of cells (3 dimensions)
#' For `GVector`s: Number of geometries and fields (columns):
#' * `dim()`: Number of geometries and fields
#' * `nrow()`: Number of geometries
#' * `ncol()`: Number of fields
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @return A numeric value or vector.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases dim
#' @rdname dim
#' @export
#' @exportMethod dim
methods::setMethod(
	f = 'dim',
	signature = 'GRaster',
	definition = function(x) x@dimensions
)

#' @rdname dim
#' @aliases nrow
#' @exportMethod nrow
setMethod(
	f = 'nrow',
	signature = 'GRaster',
	definition = function(x) x@dimensions[1L]
)

#' @rdname dim
#' @aliases ncol
#' @exportMethod ncol
setMethod(
	f = 'ncol',
	signature = 'GRaster',
	definition = function(x) x@dimensions[2L]
)

#' @rdname dim
#' @aliases ndepth
#' @exportMethod ndepth
setMethod(
	f = 'ndepth',
	signature = 'GRaster',
	definition = function(x) x@dimensions[3L]
)

#' @rdname dim
#' @aliases ncell
#' @exportMethod ncell
setMethod(
	f = 'ncell',
	signature = 'GRaster',
	definition = function(x) prod(x@dimensions[1L:2L])
)

#' @rdname dim
#' @aliases ncell3d
#' @exportMethod ncell3d
setMethod(
	f = 'ncell3d',
	signature = 'GRaster',
	definition = function(x) prod(x@dimensions[1L:3L])
)

#' @aliases dim
#' @rdname dim
#' @export
#' @exportMethod dim
methods::setMethod(
	f = 'dim',
	signature = 'GVector',
	definition = function(x) c(x@numGeometries, x@numFields)
)

#' @rdname dim
#' @aliases nrow
#' @exportMethod nrow
setMethod(
	f = 'nrow',
	signature = 'GVector',
	definition = function(x) x@numGeometries
)

#' @rdname dim
#' @aliases ncol
#' @exportMethod ncol
setMethod(
	f = 'ncol',
	signature = 'GVector',
	definition = function(x) x@numFields
)

#' @rdname dim
#' @aliases nlyr
#' @exportMethod nlyr
setMethod(
	f = 'nlyr',
	signature = 'GSpatial',
	definition = function(x) x@nLayers
)
