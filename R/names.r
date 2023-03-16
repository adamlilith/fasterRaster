#' Name(s) of a `GRaster`
#'
#' Names(s) of a `GRaster`.
#'
#' @param x A `GRaster`.
#'
#' @details `GRaster`s have two types of names, a "gname", which is used internally to point to the **GRASS** representation of the raster (this is not of use to most users), and an "rname", which is taken from the "**R**" name of the raster or the filename. This function returns the "rname".
#'
#' @return Character vector.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export

# if (!isGeneric('names')) setGeneric('names', function(x) standardGeneric('names'))
setMethod(
	f = 'names',
	signature = 'GRaster',
	definition = function(x) x@names
)

if (!isGeneric('names<-')) setGeneric(
	'names<-',
	function(x, value) standardGeneric('names<-')
)
setMethod(
	f = 'names<-',
	signature = 'GRaster',
	definition = function(x, value) {
		x@names <- value
		validObject(x)
		x
	}
)
