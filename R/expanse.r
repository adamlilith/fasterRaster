#' Area of polygons or length of lines
#'
#' @description This function calculates the area of each polygon in a "polygons" `GVector` or the length of lines in a "lines" `GVector`.
#'
#' @param x A "polygons" or "lines" `GVector`.
#'
#' @param unit Character: Units in which to report values. Areal units are squared, linear are not. Can be any of:
#' * `"meters"`(default), `"metres"`, or `"m"`
#' * `"km"` or `"kilometers"`
#' * `"ha"` or `"hectares"`
#' * `"ft"` or `"feet"`
#' * `"ac"` or `"acres"`
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

	.locationRestore(x)
	
	units <- c("m", "m2", "meters2", "metres2", "km2", "kilometers2", "acres", "ha", "hectares", "ft2", "feet2", "mi2", "miles2", "%", "percent")
	unit <- omnibus::pmatchSafe(unit, units, useFirst = TRUE, nmax = 1L)
	if (unit == "m2") unit <- "meters"
	if (unit == "meters2") unit <- "meters"
	if (unit == "metres2") unit <- "meters"
	if (unit == "km2") unit <- "kilometers"
	if (unit == "kilometers2") unit <- "kilometers"
	if (unit == "mi2") unit <- "miles"
	if (unit == "miles2") unit <- "miles"
	if (unit == "ft2") unit <- "feet"
	if (unit == "feet2") unit <- "feet"
	if (unit == "ha") unit <- "hectares"
	if (unit == "%") unit <- "percent"
	unit <- omnibus::expandUnits(unit)

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
		flags = c(.quiet(), "d"),
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
