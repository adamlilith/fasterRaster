#' @title Classes for 'fasterRaster' locations, rasters, and vectors
#'
#' @describeIn GSession
#'
#' @importFrom methods new
#' @importFrom methods show
#' @exportClass GRegion
GRegion <- setClass(
	Class = 'GRegion',
	contains = 'GSpatial',
	slots = list(
		dimensions = 'integer',         # 3 integers
		resolution = 'numeric'          # 3 numerics
	),
	prototype = prototype(
		dimensions = c(NA_integer_, NA_integer_, NA_integer_),
		resolution = c(NA_real_, NA_real_, NA_real_)
	)
)

setValidity('GRegion',
	function(object) {
		if (any(object@dimensions[1L:2L] <= 0L)) {
			'First two values in @dimensions must be positive integers.'
		} else if (!is.na(object@dimensions[3L]) && object@dimensions[3L] <= 0L) {
			'Third value in @dimensions must be NA or a positive integer.'
		} else if (any(object@resolution[1L:2L] <= 0)) {
			'First two values in @resolution must be positive real values.'
		} else if (!is.na(object@resolution[3L]) && object@resolution[3L] <= 0) {
			'Third value in @resolution must be NA or a positive real value.'
		} else {
			TRUE
		}

	} # EOF
)

# show
methods::setMethod(
	f = 'show',
	signature = 'GRegion',
	definition = function(object) {

		details <- getFastOptions('details')

		digs <- min(5, getOption('digits'))
		resol <- round(object@resolution, digs)

		extent <- round(object@extent, max(round(digs / 2), 2))
		zextent <- round(object@zextent, max(round(digs / 2), 2))
		
		crs <- object@crs
		crs <- sf::st_crs(crs)
		crs <- crs$input

		cat('class       :', paste(class(object), collapse=', '), '\n')
		if (getFastOptions('details')) {
			# cat('gnames(s)   :', object@gnames, '\n')
			cat('location    :', object@location, '\n')
			cat('mapset      :', object@mapset, '\n')
		}
		cat('topology    :', object@topology, '\n')
		cat('coord ref.  :', crs, '\n')
		cat('dimensions  :', paste(object@dimensions, collapse=', '), '(nrow, ncol, ndepth)\n')
		cat('resolution  :', paste(resol, collapse=', '), '(x, y, z)\n')
		cat('extent      :', paste(extent, collapse=', '), '(xmin, xmax, ymin, ymax)\n')
		cat('z extent    :', paste(object@zextent, collapse=', '), ' (bottom, top)\n')
	}
)

# print
methods::setMethod(
	f = 'print',
	signature = 'GRegion',
	definition = function(x) show(x)
)
