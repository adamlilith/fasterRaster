#' Get the datatype of a 'GRaster'
#'
#' Returns the data type of a `GRaster`. Note that this may not necessarily be the optimal data type for ensuring the smallest file size given the values to be saved. Precedence is given to retaining data (vs. truncating values) over size on disk.
#'
#' @param x A `GRaster`.
#' @param format Either `'GRASS'` (default), `'terra'` (**terra** package data types; see [terra::writeRaster()]), or `'GDAL'` (see [https://gdal.org/user/raster_data_model.html#raster-band](GDAL: Raster Band)).
#'
#' @return Character.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export

if (!isGeneric('datatype')) datatype.GRaster <- setGeneric(name='datatype', def=function(x, type) { standardGeneric('datatype') })

setMethod(f = 'datatype',
	signature = 'GRaster',
	definition = function(x, type = 'GRASS') {
	
	dt <- x@datatypeGRASS
	
	type <- .pmatch(type, c('GRASS', 'terra', 'GDAL'))
	
	if (type == 'terra') {
		dt[dt == 'CELL'] <- 'INT4S'
		dt[dt == 'FCELL'] <- 'FLT4S'
		dt[dt == 'DCELL'] <- 'FLT8S'
	} else if (type == 'GDAL') {
		dt[dt == 'CELL'] <- 'Int32'
		dt[dt == 'FCELL'] <- 'Float32'
		dt[dt == 'DCELL'] <- 'Float64'
	}
	
	dt
	
	} # EOF
)

