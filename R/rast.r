#' Convert a 'GRaster' to a 'SpatRaster' raster
#'
#' Convert a `GRaster` to a `SpatRaster`. Note that if you intend to save the raster to disk (and do no other processing on it), it is almost always faster just to use [writeRaster()].
#'
#' @param x A `GRaster`.
#'
#' @return A `SpatRaster`.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export
# if (!isGeneric('rast')) setGeneric(name='rast', def=function(x) standardGeneric('rast'))

setMethod(f = 'rast',
	signature='GRaster',
	definition = function(x) {
	
	file <- paste0(tempfile(), '.tif')
	file <- forwardSlash(file)
	out <- writeRaster(x, filename = file, format = 'GeoTIFF', overwrite = TRUE)
	out
	
	} # EOF
)

