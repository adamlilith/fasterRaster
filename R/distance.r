
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
#' @param target Numeric: Only applicable for case 1, when `x` is a `GRaster` and `y` is missing.  If this is `NA` (default), then cells with `NA`s have their values replaced with the distance to the nearest non-`NA` cell. If this is another value, then cells with these values have their values replaced with the distance to any other cell (`NA` and non-`NA`, except for cells with this value).
#'
#' @param fillNA Logical: Determines which raster cells to fill with distances.
#' * Case 1, when `x` is a `GRaster` and `y` is missing: If `TRUE` (default), fill values of `NA` cells with distances to non-`NA` cells. If `FALSE`, fill non-`NA` cells width distance to `NA` cells.
#' * Case 2, when `x` is a `GRaster` and `y` is a `GVector`: If `TRUE` (default), then the returned raster will contain the distance from the cell to the closest feature in the vector. If `FALSE`, then cells covered by the vector will have their values replaced with the distance to the nearest cell not covered, and cells that are not covered by the vector will have values of 0.
#' * Case 3, when `x` is a `GVector` and `y` is a `GVector`: This argument is not used in this case.
#'
#' @param unit Character: Units of the output. Either of `'meters'` or `'kilometers' (or '`km'`). Partial matching is used.
#'
#' @param dense Logical: Only applicable for case 2, when `x` is a `GRaster` and `y` is a `GVector`. If `TRUE` (default), then the vector will be represented by "densified" lines (i.e., any cell that the line/boundary touches, not just the ones on the rendering path).
#'
#' @param method Character: The type of distance to calculate. Partial matching is used and capitalization is ignored. Possible values include:
#' * `Euclidean` (default): Euclidean distance
#' * `squared`: Squared Euclidean distance (faster than just Euclidean distance but same rank--good for cases where only order matters)
#' * `maximum`: Maximum Euclidean distance
#' * `geodesic`: Geographic distance
#' * `Manhattan`: Manhattan distance (i.e., "taxicab" distance, distance along cells going only north-south and east-west and never along a diagonal).
#'
#' @param minDist,maxDist Either `NULL` (default) or numeric values: Ignore distances less than or greater than these distances.
#'
#' @return If `x` is a `GRaster`, then the output is a `GRaster`. If `x` is a `GVector`, then the output is a numeric vector.
#'
#' @seealso [terra::distance()]; [sf::st_distance()], modules `r.grow.distance` and `v.distance` in **GRASS** 
#'
#' @example man/examples/ex_distance.r
#'
#' @aliases distance
#' @rdname distance
#' @exportMethod distance
methods::setMethod(
	'distance',
	signature(x = 'GRaster', y = 'missing'),
	function(x, y, target = NA, fillNA = TRUE, unit = 'meters', method = 'Euclidean', minDist = NULL, maxDist = NULL) {
	
	if (!is.null(minDist) && minDist < 0) stop('Argument ', sQuote('minDist'), ' must be positive or NULL.')
	if (!is.null(maxDist) && maxDist < 0) stop('Argument ', sQuote('maxDist'), ' must be positive or NULL.')
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop('Argument ', sQuote('minDist'), ' is greater than ', sQuote('maxDist'), '.')

	metric <- tolower(method)
	metric <- .pmatch(metric, c('euclidean', 'squared', 'maximum', 'manhattan', 'geodesic'))

	.restore(x)
	region(x)
	
	gn <- gnames(x)
	
	# create mask
	if (!is.na(target)) {
		
		gn1 <- .makeGname(NULL, 'raster')
		ex <- paste0(gn1, ' = if(', gnames(x), ' == ', target, ')')
		rgrass::execGRASS('r.mapcalc', expression=ex, flags=c('quiet', 'overwrite'))
		
		gn <- .makeGname(NULL, 'raster')
		ex <- paste0(gn, ' = ', gn1, ' / ', gn1)
		rgrass::execGRASS('r.mapcalc', expression=ex, flags=c('quiet', 'overwrite'))
		
		fillNA <- !fillNA
		
	}
	
	flags <- c('quiet', 'overwrite', 'm')
	if (!fillNA) flags <- c(flags, 'n')
	
	gnOut <- .makeGname(NULL, 'raster')
	args <- list(
		cmd = 'r.grow.distance',
		input = gn,
		distance = gnOut,
		metric = metric,
		flags = flags,
		intern = TRUE
	)
	
	if (!is.null(minDist)) args <- c(args, minimum_distance = minDist)
	if (!is.null(maxDist)) args <- c(args, maximum_distance = maxDist)
	
	input <- do.call(rgrass::execGRASS, args)
	
	# convert to kilometers
	unit <- tolower(unit)
	unit <- .pmatch(unit, c('meters', 'kilometers', 'km'))
	if (unit %in% c('kilometers', 'km')) {
	
		gnIn <- gnOut
		gnOut <- .makeGname(NULL, 'rast')
		ex <- paste0(gnOut, ' = ', gnIn, ' / 1000')
		rgrass::execGRASS('r.mapcalc', expression=ex, flags=c('quiet', 'overwrite'), intern=TRUE)
		
	}
	
	makeGRaster(gn, 'distance')
	
	} # EOF
)

#' @aliases distance
#' @rdname distance
#' @export
#' @exportMethod distance
methods::setMethod(
	'distance',
	signature(x = 'GRaster', y = 'GVector'),
	function(x, y, fillNA = TRUE, dense = TRUE, unit = 'meters', method = 'Euclidean', minDist = NULL, maxDist = NULL) {
	
	if (!is.null(minDist) && minDist < 0) stop('Argument ', sQuote('minDist'), ' must be positive or NULL.')
	if (!is.null(maxDist) && maxDist < 0) stop('Argument ', sQuote('maxDist'), ' must be positive or NULL.')
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop('Argument ', sQuote('minDist'), ' is greater than ', sQuote('maxDist'), '.')

	metric <- tolower(method)
	metric <- .pmatch(metric, c('euclidean', 'squared', 'maximum', 'manhattan', 'geodesic'))

	comparable(x, y)
	.restore(x)
	
	flags <- 'quiet'
	
	gt <- geomtype(y)
    type <- if (gt == 'points') {
        'point'
    } else if (gt == 'lines') {
        'line'
    } else if (gt == 'polygons') {
        'boundary'
	} else {
        stop('Unknown vector data type.')
	}

	flags <- c('quiet', 'overwrite')
	if (dense) flags <- c(flags, 'd')
	
	gnameRasterized <- .makeGname(NULL, 'raster')
	rgrass::execGRASS('v.to.rast', input=gnames(y), output=gnameRasterized, use='val', value=1, type=type, flags=flags, intern=TRUE)
	
	flags <- c('quiet', 'overwrite', 'm')
	if (!fillNA) flags <- c(flags, 'n')
	
	gnOut <- .makeGname(NULL, 'raster')
	rgrass::execGRASS('r.grow.distance', input=gnameRasterized, distance=gn, metric=metric, flags=flags, intern=TRUE)
	
	# convert meters to kilometers
	unit <- .pmatch(unit, c('meters', 'kilometers', 'km'))
	if (unit %in% c('kilometers', 'km')) {
	
		gnIn <- gnOut
		gn <- .makeGname(NULL, 'rast')
		ex <- paste0(gn, ' = ', gnIn, ' / 1000')
		rgrass::execGRASS('r.mapcalc', expression=ex, flags=c('quiet', 'overwrite'), intern=TRUE)
		
	}

	makeGRaster(gn, 'distance')
	
	} # EOF
)

#' @aliases distance
#' @rdname distance
#' @export
#' @exportMethod distance
methods::setMethod(
	'distance',
	signature(x = 'GVector', y = 'GVector'),
	function(x, y, unit = 'meters', minDist = NULL, maxDist = NULL) {
	
	if (!is.null(minDist) && minDist < 0) stop('Argument ', sQuote('minDist'), ' must be positive or NULL.')
	if (!is.null(maxDist) && maxDist < 0) stop('Argument ', sQuote('maxDist'), ' must be positive or NULL.')
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop('Argument ', sQuote('minDist'), ' is greater than ', sQuote('maxDist'), '.')

	comparable(x, y)
	.restore(x)

	if (is.null(minDist)) minDist <- -1
	if (is.null(maxDist)) maxDist <- -1

	flags <- c('quiet', 'overwrite', 'p')
	dists <- rgrass::execGRASS('v.distance', from=gnames(x), to=gnames(y), upload='dist', flags=flags, dmin=minDist, dmax=maxDist, intern=TRUE)
	
	dists <- dists[-which(dists=='from_cat|dist')]
	dists <- strsplit(dists, split='\\|')
	
	out <- rep(NA, nrow(x))
	for (i in seq_along(out)) out[i] <- dists[[i]][2L]
	out <- as.numeric(out)
	
	# convert to kilometers
	unit <- .pmatch(unit, c('meters', 'kilometers', 'km'))
	if (unit %in% c('kilometers', 'km')) out <- out / 1000
	
	out
	
	} # EOF
)

#' @aliases distance
#' @rdname distance
#' @exportMethod st_distance
methods::setMethod(
	'st_distance',
	signature(x = 'GVector', y = 'GVector'),
	function(x, y, unit = 'meters', minDist = NULL, maxDist = NULL) {
		distance(x=x, y=y, unit=unit, minDist=minDist, maxDist=maxDist)
	}
)
