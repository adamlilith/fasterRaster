#' Convert a 'GRaster' to a 'SpatRaster' or 'stars' raster
#'
#' Convert a `GRaster` to a `SpatRaster` or `stars` raster. The format of the output can be decided by the `format` argument, or set for all uses of this function with [setFastOptions()].
#'
#' @param x A `GRaster`.
#' @param format Either `'SpatRaster'` (default) or `'stars'`.
#'
#' @return A `SpatRaster` or a `stars` raster.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export

# if (!isGeneric('rast')) rast.GRaster <- setGeneric(name='rast', def=function(x) { standardGeneric('rast') })

# setMethod(f = 'rast',
	# signature='GRaster',
	# definition = function(x) {
	
	# ABS: WRITE RASTER
	
	# } # EOF
# )

