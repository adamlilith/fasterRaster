#' Number of rows and columns of a `GRaster` raster
#'
#' Number of rows and columns of a `GRaster` raster object.
#'
#' @param x A `GRaster`.
#'
#' @return A numeric vector.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export
if (!isGeneric('dim')) dim.GRaster <- setGeneric(name='dim', def=function(x) { standardGeneric('dim') })
setMethod(
	f = 'dim',
	signature = 'GRaster',
	definition = function(x) x@dimensions
)

#' @name nrow
#' @title Number of rows in a 'GRaster'
#' @rdname dim
#' @export
if (!isGeneric('nrow')) nrow.GRaster <- setGeneric(name='nrow', def=function(x) { standardGeneric('nrow') })
setMethod(
	f = 'nrow',
	signature = 'GRaster',
	definition = function(x) x@dimensions[1L]
)

#' @name ncell
#' @title Number of columns in a 'GRaster'
#' @rdname dim
#' @export
if (!isGeneric('ncol')) ncol.GRaster <- setGeneric(name='ncol', def=function(x) { standardGeneric('ncol') })
setMethod(
	f = 'ncol',
	signature = 'GRaster',
	definition = function(x) x@dimensions[2L]
)

#' @name ncell
#' @title Number of cells in a 'GRaster'
#' @rdname dim
#' @export
if (!isGeneric('ncell')) ncell.GRaster <- setGeneric(name='ncell', def=function(x) { standardGeneric('ncell') })
setMethod(
	f = 'ncell',
	signature = 'GRaster',
	definition = function(x) prod(x@dimensions[1L:2L])
)
