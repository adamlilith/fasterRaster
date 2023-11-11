#' Area of polygons or length of lines
#'
#' @description This function calculates the area of each polygon in a "polygons" `GVector` or the length of lines in a "lines" `GVector`.
#'
#' @param x A "polygons" or "lines" `GVector`.
#'
#' @param unit Character: Units in which to report values. Can be any of:
#' * `"meters"` (default)
#' * `"km"` or `"kilometers"`
#' * `"ha"` or `"hectares"`
#' * `"ft"` or `"feet"`
#' * `"percent"`
#'
#' Partial matching is used and case is ignored.
#'
#' @returns Numeric values, one per geometry in `x`.
#'
#' @example man/examples/ex_GVector.r
#'
#' @aliases expanse
#' @rdname expanse
#' @exportMethod expanse
methods::setMethod(
	f = "expanse",
	signature = c(x = "GVector"),
	function(x, unit = "m") {
	
	gtype <- geomtype(x, grass = TRUE)
	if (!(gtype %in% c("area", "line"))) stop("GVector must represent lines or polygons.")

	.restore(x)
	
	if (unit == "m") unit <- "meters"
	units <- c("meters", "km", "kilometers", "ha", "hectares", "ft", "feet", "miles", "percent")
	
	unit <- pmatchSafe(unit, units, nmax = 1L)

	if (unit == "km") {
		unit <- "kilometers"
	} else if (unit == "ha") {
		unit <- "hectares"
	} else if (unit == "ft") {
		unit <- "feet"
	}

	option <- if (gtype == "area") {
		"area"
	} else if (gtype == "line") {
		"length"
	}

	info <- rgrass::execGRASS(
		cmd = "v.report",
		map = sources(x),
		option = option,
		units = unit,
		flags = c("quiet", "d"),
		intern = TRUE
	)

	info <- strsplit(info, split = "\\|")
	header <- info[[1L]]
	
	if (gtype == "line") {
		column <- which(header == "length")
	} else {
		column <- which(header == "area")
	}

	out <- rep(NA_character_, ngeom(x))
	for (i in seq_along(out)) {
	
		val <- info[[i + 1L]][column]
		out[i] <- val
	
	}
	out <- as.numeric(out)
	out

	} # EOF
)