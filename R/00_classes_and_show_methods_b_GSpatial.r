#' @title Classes for 'fasterRaster' locations, rasters, and vectors
#'
#' @describeIn GSession
#'
#' @importFrom methods new
#' @importFrom methods show
#' @exportClass GSpatial
GSpatial <- setClass(
	Class = 'GSpatial',
	contains = 'GSession',
	slots = list(
		topology = 'character',			# 2D or 3D
		extent = 'numeric',				# horizontal extent (4 numerics)
		nLayers = 'integer',			# number of layers
		gnames = 'character'			# name in GRASS
	),
	prototype = prototype(
		topology = NA_character_,
		extent = c(NA_real_, NA_real_, NA_real_, NA_real_),
		nLayers = NA_integer_,
		gnames = NA_character_
	)
)

setValidity('GSpatial',
	function(object) {
		if (length(object@location) != 1L) {
			'@location can only be a single character string.'
		} else if (length(object@mapset) != 1L) {
			'@mapset can only be a single character string.'
		} else if (length(object@crs) != 1L) {
			'@crs can only be a single character string.'
		} else if (!all(object@topology %in% c(NA_character_, '2D', '3D'))) {
			paste0('@topology can only be a NA, ', sQuote('2D'), ', or ', sQuite('3D'), '.')
		} else if (object@nLayers < 1) {
			'@nLayers must be a positive integer.'
		} else if (object@nLayers != length(object@gnames)) {
			'@nLayers is different from the number of @gnames.'
		} else {
			TRUE
		}
	} # EOF
)

# GSpatial <- function(
	# location = NA_character_,
	# mapset = NA_character_,
	# crs = NA_character_,
	# gnames = NA_character_,
	# topology = NA_character_,
	# extent = c(NA_real_, NA_real_, NA_real_, NA_real_),
	# ztop = NA_real_,
	# zbottom = NA_real_
# ) {
	# new(
		# 'GSpatial',
		# location = location,
		# mapset = mapset,
		# crs = crs,
		# gnames = gnames,
		# topology = topology,
		# extent = extent,
		# ztop = ztop,
		# zbottom = zbottom
	# )
# }

# show
methods::setMethod(
	f = 'show',
	signature = 'GSpatial',
	definition = function(object) {

		digs <- min(3, getOption('digits'))
		extent <- round(object@extent, digs)
		
		crs <- object@crs
		crs <- sf::st_crs(crs)
		crs <- crs$input

		cat('class       :', paste(class(object), collapse=', '), '\n')
		cat('topology    :', object@topology, '\n')
		if (getFastOptions('details')) {
			cat('gnames(s)   :', object@gnames, '\n')
			cat('location    :', object@location, '\n')
			cat('mapset      :', object@mapset, '\n')
		}
		cat('extent      :', paste(extent, collapse=', '), '(xmin, xmax, ymin, ymax)\n')
		cat('coord ref.  :', crs, '\n')
	}
)

# print
methods::setMethod(
	f = 'print',
	signature = 'GSpatial',
	definition = function(x) show(x)
)
