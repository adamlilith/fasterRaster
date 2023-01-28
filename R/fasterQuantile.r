#' Quantiles for a raster
#'
#' This function is a potentially faster version of the function \code{quantile(raster, probs)} for calculating the quantiles of a raster. This function will also work on rasters too big to load into memory using the \pkg{terra} package.
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @param probs A numeric list of quantiles to calculate.
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @return A named vector of the values for each quantile named in \code{probs}.
#'
#' @seealso \code{\link[stats]{quantile}} in the \pkg{base} package; \code{\link[terra]{quantile}} in \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/r.quantile.html}{\code{r.quantile}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterQuantile.r
#'
#' @export

fasterQuantile <- function(
	rast,
	inRastName,
	probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### begin common
	flags <- .getFlags(replace=replace)
	inRastName <- .getInRastName(inRastName, rast)
	if (is.null(inVectName)) inVectName <- 'vect'
	
	# region settings
	success <- .rememberRegion()
	# on.exit(.restoreRegion(inits), add=TRUE)
	# on.exit(.revertRegion(), add=TRUE)
	# on.exit(regionResize(), add=TRUE)
	
	if (is.null(inits)) inits <- list()
	### end common

	probs <- 100 * probs

	# initialize GRASS
	inits <- c(inits, list(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, replace=replace, grassDir=grassDir))
	input <- do.call('initGrass', inits)
	
	# temp file for output
	tempFile <- tempfile(pattern = 'file', tmpdir = tempdir(), fileext = '.csv')

	# calculate
	rgrass::execGRASS('r.quantile', input=input, file=tempFile, percentiles=probs, flags=flags)

	# get output
	grassQuants <- read.csv(tempFile, header=FALSE)

	out <- rep(NA, length(probs))
	names(out) <- paste0('prob_', probs / 100)
	for (i in 1:nrow(grassQuants)) {
		thisRow <- grassQuants[i, ]
		thisRow <- strsplit(thisRow, split=':')
		out[i] <- as.numeric(thisRow[[1]][3])
	}

	out

}
