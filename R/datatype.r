#' Get the datatype of a GRaster or of GVector columns
#'
#' Returns the data type of a `GRaster` or of each column of a `GVector`.
#'
#' @param x A `GRaster` or `GVector`.
#' @param type Character: What type of datatype to report (`GRaster` only): `'GRASS'` (default), `'terra'` (**terra** package data types; see [terra::writeRaster()]), or `'GDAL'` (see [GDAL: Raster Band](https://gdal.org/user/raster_data_model.html#raster-band)).
#'
#' @return `datatype()` for a `GRaster` returns a character. `datatype()` for a `GVector` returns a data frame, with one row per field.
#'
#' @example man/examples/ex_GRaster.r
#'
#' @aliases datatype
#' @rdname datatype
#' @export datatype
#' @exportMethod datatype
setMethod(f = 'datatype',
	signature = c(x = 'GRaster'),
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

#' @aliases datatype
#' @rdname datatype
#' @export datatype
#' @exportMethod datatype
setMethod(f = 'datatype',
	signature = c(x = 'GVector'),
	definition = function(x) {

		info <- rgrass::execGRASS('v.info', flags='c', map=gnames(x), intern=TRUE)
		info <- info[!grepl(info, pattern='Displaying column types/names for database connection of layer')]
		
		info <- strsplit(info, split='\\|')
		
		dt <- fields <- rep(NA_character_, length(info))
		for (i in seq_along(info)) {
			fields[i] <- info[[i]][2L]
			dt[i] <- info[[i]][1L]
		}
		
		dt <- tolower(dt)
		data.frame(field = fields, datatype = dt)
	
	} # EOF
)
