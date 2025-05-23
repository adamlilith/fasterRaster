#' @title Classes for "fasterRaster" locations, rasters, and vectors
#'
#' @aliases GSpatial
#' @rdname GLocation
#' @exportClass GSpatial
GSpatial <- methods::setClass(
	Class = "GSpatial",
	contains = "GLocation",
	slots = list(
		topology = "character",			# 2D or 3D
		extent = "numeric",				# horizontal extent (4 numerics)
		zextent = "numeric",			# vertical extent (2 numerics)
		sources = "character"			# name in GRASS
	),
	prototype = prototype(
		topology = NA_character_,
		extent = c(NA_real_, NA_real_, NA_real_, NA_real_),
		zextent = c(NA_real_, NA_real_),
		sources = NA_character_
	)
)

methods::setValidity("GSpatial",
	function(object) {
		if (length(object@location) != 1L) {
			"@location can only be a single character string."
		} else if (length(object@mapset) != 1L) {
			"@mapset can only be a single character string."
		} else if (length(object@crs) != 1L) {
			"@crs can only be a single character string."
		} else if (!all(object@topology %in% c(NA_character_, "2D", "3D"))) {
			paste0("@topology can only be a NA, `2D` or `3D`.")
		} else if (object@topology == "3D" && any(is.na(object@zextent))) {
			paste0("@topology is `3D` but @zextent has at least one NA value.")
		} else if (!anyNA(object@zextent[1L]) & is.na(object@zextent[2L]) | (is.na(object@zextent[1L]) & !is.na(object@zextent[2L]))) {
			"Both values of @zextent must be NA or must be numeric values."
		} else if (!anyNA(object@zextent) && object@zextent[2L] < object@zextent[1L]) {
			"Bottom value of @zextent is greater than the top value of @zextent."
		} else {
			TRUE
		}
	} # EOF
)

# GSpatial <- function(
	# location = NA_character_,
	# mapset = NA_character_,
	# crs = NA_character_,
	# sources = NA_character_,
	# topology = NA_character_,
	# extent = c(NA_real_, NA_real_, NA_real_, NA_real_),
	# ztop = NA_real_,
	# zbottom = NA_real_
# ) {
	# methods::new(
		# "GSpatial",
		# location = location,
		# mapset = mapset,
		# crs = crs,
		# sources = sources,
		# topology = topology,
		# extent = extent,
		# ztop = ztop,
		# zbottom = zbottom
	# )
# }

