#' Geographic distance
#'
#' @description
#' This function produces a raster or a matrix of geographic distances, depending on the input:
#'
#' **Case 1: Argument `x` is a `GRaster` and `y` is missing: (`distance()`)** By default, this function replaces values in all `NA` cells with the distance between them and their closest non-`NA` cell. Alternatively, all non-`NA` cells can have their values replaced by the distance to `NA` cells. You can also specify which cells (by value) have their values replaced by distance to other cells.
#'
#' **Case 2: Argument `x` is a `GRaster` and `y` is a `GVector` (`distance()`):** All cells in the raster have their value replaced by the distance to the nearest features in the `GVector`. Alternatively, calculate the distance from any cell covered by a vector object and the nearest cell *not* covered by a vector object. Note that the vector is rasterized first.
#'
#' **Case 3: Argument `x` is a `GVector` and `y` is a `GVector` (`distance()` and `st_distance()`):** A matrix of pairwise distances between all features in one versus the other `GVector` is returned.
#' 
#' @param x A `GRaster` or `GVector`.
#'
#' @param y Either missing, or a `GVector`.
#'
#' @param target Numeric: Only applicable for case 1, when `x` is a `GRaster` and `y` is missing.  If this is `NA` (default), then cells with `NA`s have their values replaced with the distance to the nearest non-`NA` cell. If this is another value, then cells with these values have their values replaced with the distance to any other cell (meaning, both `NA` and non-`NA`, except for cells with this value).
#'
#' @param fillNA Logical: Determines which raster cells to fill with distances.
#' * Case 1, when `x` is a `GRaster` and `y` is missing: If `TRUE` (default), fill values of `NA` cells with distances to non-`NA` cells. If `FALSE`, fill non-`NA` cells width distance to `NA` cells.
#' * Case 2, when `x` is a `GRaster` and `y` is a `GVector`: If `TRUE` (default), then the returned raster will contain the distance from the cell to the closest feature in the vector. If `FALSE`, then cells covered by the vector will have their values replaced with the distance to the nearest cell not covered, and cells that are not covered by the vector will have values of 0.
#' * Case 3, when `x` is a `GVector` and `y` is a `GVector`: This argument is not used in this case.
#'
#' @param unit Character: Units of the output. Any of:
#' * `"meters"`, `"metres"`, or `"m"`
#' * `"kilometers"` or `"km"`
#' * `"miles"` or `"mi"`
#' * `"nautical miles"` or `"nmi"`
#' * `"yards"` or `"yd"`
#' * `"feet"` or `"ft"` -- international, 1 foot exactly equal to 0.3048 meters
#'
#' Partial matching is used and case is ignored.
#'
#' @param thick Logical: Only applicable for case 2, when `x` is a `GRaster` and `y` is a `GVector`. If `TRUE` (default), then the vector will be represented by "thickened" lines (i.e., any cell that the line/boundary touches, not just the ones on the rendering path).
#'
#' @param method Character: The type of distance to calculate. Partial matching is used and capitalization is ignored. Possible values include:
#' * `Euclidean` (default for projected rasters): Euclidean distance.
#' * `geodesic` (default for unprojected rasters): Geographic distance. If `x` is unprojected (e.g., WGS84 or NAD83), then the `method` must be `"geodesic"`.
#' * `squared`: Squared Euclidean distance (faster than just Euclidean distance but same rank--good for cases where only order matters).
#' * `maximum`: Maximum Euclidean distance.
#' * `Manhattan`: Manhattan distance (i.e., "taxicab" distance, distance along cells going only north-south and east-west and never along a diagonal).
#'
#' @param minDist,maxDist Either `NULL` (default) or numeric values: Ignore distances less than or greater than these distances.
#'
#' @returns If `x` is a `GRaster`, then the output is a `GRaster`. If `x` is a `GVector`, then the output is a numeric vector.
#'
#' @seealso [terra::distance()]; [sf::st_distance()], **GRASS** modules `r.grow.distance` and `v.distance`
#'
#' @example man/examples/ex_distance.r
#'
#' @aliases distance
#' @rdname distance
#' @exportMethod distance
methods::setMethod(
	"distance",
	signature(x = "GRaster", y = "missing"),
	function(
		x,
		y,
		target = NA,
		fillNA = TRUE,
		unit = "meters",
		method = ifelse(is.lonlat(x), "geodesic", "Euclidean"),
		minDist = NULL,
		maxDist = NULL
	) {
	
	if (!is.null(minDist) && minDist < 0) stop("Argument ", sQuote("minDist"), " must be positive or NULL.")
	if (!is.null(maxDist) && maxDist < 0) stop("Argument ", sQuote("maxDist"), " must be positive or NULL.")
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop("Argument ", sQuote("minDist"), " is greater than ", sQuote("maxDist"), ".")

	method <- tolower(method)
	methods <- c("euclidean", "squared", "maximum", "manhattan", "geodesic")
	method <- omnibus::pmatchSafe(method, methods)
	if (is.lonlat(x) & method != "geodesic") warning("Argument ", sQuote("method"), " should be ", sQuote("geodesic"), " for rasters with longitude/latitude coordinate reference systems.")

	.restore(x)
	region(x)
	
	src <- sources(x)
	
	# create mask
	if (!is.na(target)) {
		
		src <- .makeSourceName("r_mapcalc", "raster") # note: redefining "src"
		ex <- paste0(src, " = if(", sources(x), " == ", target, ", 1, null())")
  		rgrass::execGRASS(
			"r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)
		
		fillNA <- !fillNA
		
	}
	
	srcOut <- .makeSourceName("r_grow_distance", "raster")
	args <- list(
		cmd = "r.grow.distance",
		input = src,
		distance = srcOut,
		metric = method,
		flags = c(.quiet(), "overwrite", "m"),
		intern = TRUE
	)
	if (!fillNA) args$flags <- c(args$flags, "n")
	if (!is.null(minDist)) args <- c(args, minimum_distance = minDist)
	if (!is.null(maxDist)) args <- c(args, maximum_distance = maxDist)
	
	do.call(rgrass::execGRASS, args)
	
	# convert units
	src <- .convertRastFromMeters(srcOut, unit)
	.makeGRaster(src, "distance")
	
	} # EOF
)

#' @aliases distance
#' @rdname distance
#' @exportMethod distance
methods::setMethod(
	"distance",
	signature(x = "GRaster", y = "GVector"),
	function(
		x,
		y,
		fillNA = TRUE,
		thick = TRUE,
		unit = "meters",
		method = ifelse(is.lonlat(x), "geodesic", "Euclidean"),
		minDist = NULL,
		maxDist = NULL
	) {
	
	if (!is.null(minDist) && minDist < 0) stop("Argument ", sQuote("minDist"), " must be positive or NULL.")
	if (!is.null(maxDist) && maxDist < 0) stop("Argument ", sQuote("maxDist"), " must be positive or NULL.")
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop("Argument ", sQuote("minDist"), " is greater than ", sQuote("maxDist"), ".")

	methods <- c("euclidean", "squared", "maximum", "manhattan", "geodesic")
	method <- omnibus::pmatchSafe(method, methods)
	if (is.lonlat(x) & method != "geodesic") warning("Argument ", sQuote("method"), " should be ", sQuote("geodesic"), " for rasters with longitude/latitude coordinate reference systems.")

	compareGeom(x, y)
	.restore(x)
	
	# rasterize
 	gtype <- geomtype(y, grass = TRUE)

	srcRasterized <- .makeSourceName("v_to_rast", "raster")
	args <- list(
		cmd = "v.to.rast",
		input = sources(y),
		output = srcRasterized,
		use = "val",
		value = 1,
		type = gtype,
		flags = c(.quiet(), "overwrite")
	)
	if (thick & gtype == "line") args$flags <- c(args$flags, "d")
	do.call(rgrass::execGRASS, args = args)
	
	# distance
	src <- .makeSourceName("r_grow_distance", "raster")
	args <- list(
		cmd = "r.grow.distance",
		input = srcRasterized,
		distance = src,
		metric = method,
		flags = c("m", .quiet(), "overwrite")
	)
	if (!fillNA) args$flags <- c(args$flags, "n")
	do.call(rgrass::execGRASS, args = args)
	
	# convert units
	src <- .convertRastFromMeters(src = src, unit = unit)
	.makeGRaster(src, "distance")
	
	} # EOF
)

#' @aliases distance
#' @rdname distance
#' @exportMethod distance
methods::setMethod(
	"distance",
	signature(x = "GVector", y = "GVector"),
	function(
		x,
		y,
		unit = "meters",
		minDist = NULL,
		maxDist = NULL
	) {
	
	if (!is.null(minDist) && minDist < 0) stop("Argument ", sQuote("minDist"), " must be positive or NULL.")
	if (!is.null(maxDist) && maxDist < 0) stop("Argument ", sQuote("maxDist"), " must be positive or NULL.")
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop("Argument ", sQuote("minDist"), " is greater than ", sQuote("maxDist"), ".")

	compareGeom(x, y)
	.restore(x)

	if (is.null(minDist)) minDist <- -1
	if (is.null(maxDist)) maxDist <- -1

	out <- rgrass::execGRASS(
		cmd = "v.distance",
		from = sources(x),
		to = sources(y),
		upload = "dist",
		dmin = minDist,
		dmax = maxDist,
		flags = c(.quiet(), "overwrite", "p"),
		intern = TRUE
	)
	
	out <- out[-1L]
	out <- strsplit(out, split="\\|")
	out <- lapply(out, as.numeric)
	out <- do.call(rbind, out)
	out <- out[ , 2L, drop = TRUE]
	
	# convert units
	units <- c("m", "meters", "metres", "kilometers", "km", "miles", "nautical miles", "nmi", "yards", "yd", "feet", "ft")
    unit <- omnibus::pmatchSafe(unit, units, useFirst = TRUE)
	unit <- omnibus::expandUnits(unit)
	if (unit == "metres") unit <- "meters"
	omnibus::convertUnits(from = "meters", to = unit, x = out)
	
	} # EOF
)

#' @aliases st_distance
#' @rdname distance
#' @exportMethod st_distance
methods::setMethod(
	"st_distance",
	signature(x = "GVector", y = "GVector"),
	function(x, y, unit = "meters", minDist = NULL, maxDist = NULL) {
  		distance(x = x, y = y, unit = unit, minDist = minDist, maxDist = maxDist)
	}
)

st_distance <- function(x) UseMethod("st_distance", x)

.convertRastFromMeters <- function(src, unit) {
    
	if (inherits(src, "GRaster")) src <- sources(src)

    # convert raster units
	units <- c("m", "meters", "metres", "km", "kilometers", "mi", "miles", "nmi", "nautical miles", "yd", "yards", "ft", "feet")
	
    unit <- omnibus::pmatchSafe(unit, units, useFirst = TRUE)
    unit <- omnibus::expandUnits(unit)
	if (unit == "metres") units <- "meters"

	if (unit != "meters") {
	
		convFact <- omnibus::convertUnits(from = "meters", to = unit)

        srcIn <- src
        srcOut <- .makeSourceName("convertRastFromMeters", "raster")

		ex <- paste0(srcOut, " = ", srcIn, " * ", convFact)
	
        rgrass::execGRASS(
            "r.mapcalc",
            expression = ex,
            flags = c(.quiet(), "overwrite")
        )
        out <- srcOut
	
	} else {
		out <- src
	}
	out
	
}
