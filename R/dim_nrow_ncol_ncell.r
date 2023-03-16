#' Number of rows, columns, and depths of a GRaster
#'
#' Number of rows, columns, and (for 3D rasters) depths of a `GRaster` raster object. Functions report:
#' `dim()`: Number of rows and columns (x- and y-dimensions)\cr
#' `dim3d()`: Number of rows, columns, and depths in x-, y-, and z-dimensions (3D rasters only)\cr
#' `ncol()`: Number of columns\cr
#' `nrow()`: Number of rows\cr
#' `ndepth()`: Number of depths (3D rasters only)\cr
#' `ncell()`: Number of cells in x- and y-dimensions\cr
#' `ncell3d()`: Number of cells in x-, y-, and z-dimensions (3D rasters only)\cr
#'
#' @param x A `GRaster`.
#'
#' @return A numeric value or vector.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export
# if (!isGeneric('dim')) setGeneric(name='dim', def=function(x) standardGeneric('dim'))
setMethod(
	f = 'dim',
	signature = 'GRaster',
	definition = function(x) x@dimensions[1L:2L]
)

#' @name dim3d
#' @title Number of rows in a 3D `GRaster`
#' @rdname dim3d
#' @export
# if (!isGeneric('dim3d')) setGeneric(name='dim3d', def=function(x) standardGeneric('dim3d'))
setMethod(
	f = 'dim3d',
	signature = 'GRaster',
	definition = function(x) x@dimensions[1L:3L]
)

#' @name nrow
#' @title Number of rows in a `GRaster`
#' @rdname dim
#' @export
# if (!isGeneric('nrow')) setGeneric(name='nrow', def=function(x) standardGeneric('nrow'))
setMethod(
	f = 'nrow',
	signature = 'GRaster',
	definition = function(x) x@dimensions[1L]
)

#' @name ncol
#' @title Number of columns in a `GRaster`
#' @rdname dim
#' @export
# if (!isGeneric('ncol')) setGeneric(name='ncol', def=function(x) standardGeneric('ncol'))
setMethod(
	f = 'ncol',
	signature = 'GRaster',
	definition = function(x) x@dimensions[2L]
)

#' @name ndepth
#' @title Number of depths in a 3D `GRaster`
#' @rdname dim
#' @export
# if (!isGeneric('ndepth')) setGeneric(name='ndepth', def=function(x) standardGeneric('ndepth'))
setMethod(
	f = 'ndepth',
	signature = 'GRaster',
	definition = function(x) x@dimensions[3L]
)

#' @name ncell
#' @title Number of cells in a `GRaster`
#' @rdname dim
#' @export
# if (!isGeneric('ncell')) setGeneric(name='ncell', def=function(x) standardGeneric('ncell'))
setMethod(
	f = 'ncell',
	signature = 'GRaster',
	definition = function(x) prod(x@dimensions[1L:2L])
)

#' @name ncell3d
#' @title Number of cells in a 3D `GRaster`
#' @rdname dim
#' @export
# if (!isGeneric('ncell3d')) setGeneric(name='ncell3d', def=function(x) standardGeneric('ncell3d'))
setMethod(
	f = 'ncell3d',
	signature = 'GRaster',
	definition = function(x) prod(x@dimensions[1L:3L])
)
