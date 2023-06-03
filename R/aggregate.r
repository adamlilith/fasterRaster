#' Aggregate values of raster cells into larger cells
#'
#' @description `aggregate()` creates a new raster with cells that are an even multiple of the size of the cells of the original raster.
#'
#' @param x A `GRaster`.
#' @param fact Numeric vector: One, two, or three positive values. These reflect the size of the new cells as multiples of the size of the old cells. If just one value is supplied, this is used for all two or three dimensions. If two values are supplied, the first is multiplied by the east-west size of cells, and the second north-south size of cells (the raster must be 2D). If three values are supplied, the third value is used as the multiplier of the vertical dimension of cells. Values are calculated using all cells that have their centers contained by the target cell.
#'
#' Note that unlike [terra::aggregate()], these values need not be even integers. Aggregation
#' @param fun Character: Name of the function used to aggregate:
#' * `mean``: Average (default)
#' * `median`: Median
#' * `mode`: Most common value
#' * `min`: Minimum
#' * `max`: Maximum
#' * `range`: Difference between maximum and minimum
#' * `sum`: Sum
#' * `varpop`: Population variance
#' * `sdpop`: Population standard deviation
#' * `quantile`: Quantile (see argument `prob`)
#' * `count`: Number of non-`NA` cell
#' * `diversity`: Number of unique values
#'
#' @param prob Numeric: Quantile at which to calculate `quantile`.
#'
#' @param na.rm Logical: If `FALSE` (default), propagate `NA` cells.
#'
#' @param weight Logical: If `FALSE`, each source cell that has its center in the destination cell will be counted equally. If `TRUE`, the value of each source will be weighted the proportion of the destination cell the source cell covers.
#'
#' @returns A `GRaster`.
#' 
#' @seealso [terra::aggregate()]
#'
#' @example man/examples/ex_aggregate.r
#'
#' @aliases aggregate
#' @rdname aggregate
#' @exportMethod aggregate
methods::setMethod(
	f = 'aggregate',
	signature = c(x = 'GRaster'),
	definition = function(x, fact = 2, fun = 'mean', weight = FALSE, prob = NULL, na.rm = FALSE) {

	if (any(fact < 0)) stop('Values of ', sQuote('fact'), ' must be > 0.')

	funs <- c('mean', 'median', 'mode', 'min', 'maximum', 'range', 'quantile', 'sum', 'var', 'sd', 'count', 'diversity')
	fun <- .pmatch(tolower(fun), funs)
	
	if (fun == 'mean') {
		fun <- 'average'
	} else if (fun == 'min') {
		fun <- 'minimum'
	} else if (fun == 'max') {
		fun <- 'maximum'
	} else if (fun == 'varpop') {
		fun <- 'variance'
	} else if (fun == 'sdpop') {
		fun <- 'stdev'
	}
	
	if (fun == 'quantile') {
		
		if (is.null(prob)) stop('A value must be specified for ', sQuote('prob'), ' if the aggregating function is ', sQuote('quantile'), '.')
		
		if (prob < 0 | prob > 1) stop('Argument ', sQuote('prob'), ' must be in the range [0, 1].')
	
	}
	
	.restore(x)
	region(x)

	if (is.2d(x)) {
		
		if (length(fact) == 1L) fact <- rep(fact, 2L)
		if (length(fact) == 3L) stop('This is a 2D raster. Only 1 or 2 values are allowed for ', sQuote('resol'), '.')
	
		resol <- regionRes()
		resol <- resol * fact
		regionRes(resol)
	
	} else if (is.3d(x)) {
	
		if (length(fact) == 1L) fact <- rep(fact, 3L)
		if (length(fact) == 2L) {
			warning('This is a 3D raster, but ', sQuote('resol'), ' has only 2 values.\n  Assuming third dimension will not be aggregated.')
			fact[3L] <- 1
		}
	
		resol <- regionRes3d()
		resol <- resol * fact
		regionRes3d(resol)

	}

	args <- list(
		cmd = 'r.resamp.stats',
		input = NA_character_,
		output = NA_character_,
		method = fun,
		flags = c('quiet', 'overwrite'),
		intern = TRUE
	)
	
	if (weight) args$flags <- c(args$flags, 'w')
	if (!na.rm) args$flags <- c(args$flags, 'n')
	if (fun == 'quantile') args <- c(args, quantile = prob)
	
	nLayers <- nlyr(x)
	for (i in seq_len(nLayers)) {
		
		name <- names(x)[i]
		gn <- .makeGname(names(x)[i], 'rast')
		
		args$input <- gnames(x)[i]
		args$output <- gn
		
		do.call(rgrass::execGRASS, args=args)
		
		thisOut <- makeGRaster(gn, name)
		if (i == 1L) {
			out <- thisOut
		} else {
			out <- c(out, thisOut)
		}
		
	}
	out

	} # EOF
)
