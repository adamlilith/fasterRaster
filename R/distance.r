#' Geographic distance
#'
#' @description
#' This function produces a raster or a matrix of geographic distances, depending on the input:
#'
#' **Case 1: Argument `x` is a `GRaster` and `y` is missing:** By default, this function replaces values in all `NA` cells with the distance between them and their closest non-`NA` cell. Alternatively, all non-`NA` cells can have their values replaced by the distance to `NA` cells. You can also specify which cells (by value) have their values replaced by distance to other cells.
#'
#' **Case 2: Argument `x` is a `GRaster` and `y` is a `GVector`:** All cells in the raster have their value replaced by the distance to the nearest features in the `GVector`. Alternatively, calculate the distance from any cell covered by a vector object and the nearest cell *not* covered by a vector object. Note that the vector is rasterized first.
#'
#' **Case 3: Argument `x` is a `GVector` and `y` is a `GVector`:** A matrix of pairwise distances between all features in one versus the other `GVector` is returned.
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
#' * `"meters"`, "metres", or `"m"`
#' * `"kilometers"` or `"km"`
#' * `"miles"`
#' * `"nautical miles"` or `"nm"`
#' * `"yards"` or `"yds"`
#' * `"survey feet"` or `"sft"` -- US survey, obsolete after January 1, 2023, 1 foot approximately equal to 0.304800000001219 meters
#' * `"feet"` or `"ft"` -- international, 1 foot exactly equal to 0.3048 meters
#' Partial matching is used.
#'
#' @param dense Logical: Only applicable for case 2, when `x` is a `GRaster` and `y` is a `GVector`. If `TRUE` (default), then the vector will be represented by "densified" lines (i.e., any cell that the line/boundary touches, not just the ones on the rendering path).
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
#' @return If `x` is a `GRaster`, then the output is a `GRaster`. If `x` is a `GVector`, then the output is a numeric vector.
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
	function(x, y, target = NA, fillNA = TRUE, unit = "meters", method = ifelse(is.lonlat(x), "geodesic", "Euclidean"), minDist = NULL, maxDist = NULL) {
	
	if (!is.null(minDist) && minDist < 0) stop("Argument ", sQuote("minDist"), " must be positive or NULL.")
	if (!is.null(maxDist) && maxDist < 0) stop("Argument ", sQuote("maxDist"), " must be positive or NULL.")
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop("Argument ", sQuote("minDist"), " is greater than ", sQuote("maxDist"), ".")

	metric <- tolower(method)
	metric <- pmatchSafe(metric, c("euclidean", "squared", "maximum", "manhattan", "geodesic"))
	if (is.lonlat(x) & metric != "geodesic") warning("Argument ", sQuote("method"), " should be ", sQuote("geodesic"), " for rasters with longitude/latitude coordinate reference systems.")

	.restore(x)
	region(x)
	
	src <- sources(x)
	
	# create mask
	if (!is.na(target)) {
		
		src <- .makeSourceName("distMask", "raster") # note: redefining "src"
		ex <- paste0(src, " = if(", sources(x), " == ", target, ", 1, null())")
  		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c("quiet", "overwrite"))
		
		fillNA <- !fillNA
		
	}
	
	gnOut <- .makeSourceName(NULL, "raster")
	args <- list(
		cmd = "r.grow.distance",
		input = src,
		distance = gnOut,
		metric = metric,
		flags = c("quiet", "overwrite", "m"),
		intern = TRUE
	)
	if (!fillNA) args$flags <- c(args$flags, "n")
	if (!is.null(minDist)) args <- c(args, minimum_distance = minDist)
	if (!is.null(maxDist)) args <- c(args, maximum_distance = maxDist)
	
	do.call(rgrass::execGRASS, args)
	
	# convert units
	src <- .convertRastUnits(gnOut, unit)
	.makeGRaster(src, "distance")
	
	} # EOF
)

#' @aliases distance
#' @rdname distance
#' @exportMethod distance
methods::setMethod(
	"distance",
	signature(x = "GRaster", y = "GVector"),
	function(x, y, fillNA = TRUE, dense = TRUE, unit = "meters", method = ifelse(is.lonlat(x), "geodesic", "Euclidean"), minDist = NULL, maxDist = NULL) {
	
	if (!is.null(minDist) && minDist < 0) stop("Argument ", sQuote("minDist"), " must be positive or NULL.")
	if (!is.null(maxDist) && maxDist < 0) stop("Argument ", sQuote("maxDist"), " must be positive or NULL.")
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop("Argument ", sQuote("minDist"), " is greater than ", sQuote("maxDist"), ".")

	metric <- tolower(method)
	metric <- pmatchSafe(metric, c("euclidean", "squared", "maximum", "manhattan", "geodesic"))
	if (is.lonlat(x) & metric != "geodesic") warning("Argument ", sQuote("method"), " should be ", sQuote("geodesic"), " for rasters with longitude/latitude coordinate reference systems.")

	compareGeom(x, y)
	.restore(x)
	
 	gtype <- geomtype(y, grass = TRUE)

	gnRasterized <- .makeSourceName("rasterized", "raster")
	args <- list(
		cmd = "v.to.rast",
		input = sources(y),
		output = gnRasterized,
		use = "val",
		value = 1,
		type = gtype,
		flags = c("quiet", "overwrite"),
		intern = TRUE
	)
	if (dense) args$flags <- c(args$flags, "d")
	do.call(rgrass::execGRASS, args = args)
	
	src <- .makeSourceName("distance", "raster")
	args <- list(
		cmd = "r.grow.distance",
		input = gnRasterized,
		distance = src,
		metric = metric,
		flags = c("m", "quiet", "overwrite"),
		intern = TRUE
	)
	if (!fillNA) args$flags <- c(args$flags, "n")
	do.call(rgrass::execGRASS, args = args)
	
	# convert units
	src <- .convertRastUnits(src = src, unit = unit)
	.makeGRaster(src, "distance")
	
	} # EOF
)

#' @aliases distance
#' @rdname distance
#' @exportMethod distance
methods::setMethod(
	"distance",
	signature(x = "GVector", y = "GVector"),
	function(x, y, unit = "meters", minDist = NULL, maxDist = NULL) {
	
	if (!is.null(minDist) && minDist < 0) stop("Argument ", sQuote("minDist"), " must be positive or NULL.")
	if (!is.null(maxDist) && maxDist < 0) stop("Argument ", sQuote("maxDist"), " must be positive or NULL.")
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop("Argument ", sQuote("minDist"), " is greater than ", sQuote("maxDist"), ".")

	compareGeom(x, y)
	.restore(x)

	if (is.null(minDist)) minDist <- -1
	if (is.null(maxDist)) maxDist <- -1

	args <- list(
		cmd = "v.distance",
		from = sources(x),
		to = sources(y),
		upload = "dist",
		dmin = minDist,
		dmax = maxDist,
		flags = c("p", "quiet", "overwrite"),
		intern = TRUE
	)
	dists <- do.call(rgrass::execGRASS, args = args)
	
	dists <- dists[-which(dists=="from_cat|dist")]
	dists <- strsplit(dists, split="\\|")
	
	out <- rep(NA, nrow(x))
	for (i in seq_along(out)) out[i] <- dists[[i]][2L]
	out <- as.numeric(out)
	
	# convert units
    unit <- pmatchSafe(unit, c("meters", "metres", "kilometers", "km", "miles", "nautical miles", "nm", "yards", "yds", "feet", "ft"))
	if (unit %in% c("kilometers", "km")) {
		out <- out / 1000
	} else if (unit == "miles") {
		out <- out * 0.0006213712
	} else if (unit %in% c("nautical miles", "nm")) {
		out <- out * 0.0005399568
	} else if (unit %in% c("yards", "yds")) {
		out <- out * 1.0936132983
	} else if (unit %in% c("survey feet", "sft")) {
		out <- out * 3.2804
	} else if (unit %in% c("feet", "ft")) {
		out <- out * 3.280839895
	}

	out
	
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

#' @importFrom st_distance
#' export
sf::st_distance

.convertRastUnits <- function(src, unit) {
    
	if (inherits(src, "GRaster")) src <- sources(src)

    # convert raster units
    unit <- pmatchSafe(unit, c("meters", "metres", "kilometers", "km", "miles", "nautical miles", "nm", "yards", "yds", "feet", "ft"))
    if (!(unit %in% c("meters", "metres"))) {

        gnIn <- src
        gnOut <- .makeSourceName("unitConvert", "raster")

        ex <- if (unit %in% c("kilometers", "km")) {
            paste0(gnOut, " = ", gnIn, " / 1000")
        } else if (unit == "miles") {
            paste0(gnOut, " = ", gnIn, " * 0.0006213712")
        } else if (unit %in% c("nautical miles", "nm")) {
            paste0(gnOut, " = ", gnIn, " * 0.0005399568")
        } else if (unit %in% c("yards", "yds")) {
            paste0(gnOut, " = ", gnIn, " * 1.0936132983")
        } else if (unit %in% c("feet", "ft")) {
            paste0(gnOut, " = ", gnIn, " * 3.28084")
        } else if (unit %in% c("survey feet", "sft")) {
            paste0(gnOut, " = ", gnIn, " * 3.280839895")
        }

        args <- list(
            "r.mapcalc",
            expression = ex,
            flags = c("quiet", "overwrite"),
            intern = TRUE
        )
        do.call(rgrass::execGRASS, args = args)
        out <- gnOut
    } else {
        out <- src
    }
    out
}
