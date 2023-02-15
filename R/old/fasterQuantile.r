#' Quantiles for a raster
#'
#' Calculate quantiles for across all cells of a raster.
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
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### commons v1
	##############

		### arguments
		if (!missing(rast)) {
			if (!inherits(rast, 'character') & !inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
			inRastName <- .getInRastName(inRastName, rast=rast)
			.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=NULL, ...)
		} else {
			rast <- inRastName <- NULL
		}

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterQuantile')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### function-specific
	probs <- 100 * probs

	### function-specific
	tempFile <- tempfile(pattern = 'file', tmpdir = tempdir(), fileext = '.csv')

	args <- list(
		cmd = 'r.quantile',
		input = inRastName,
		file = tempFile,
		percentiles = probs,
		flags = flags,
		intern = TRUE
	)
	args <- c(args, dots)

	### initialize GRASS
	input <- do.call('startFaster', inits)
	
	### execute
	if (autoRegion) regionReshape(inRastName)
	do.call(rgrass::execGRASS, args=args)

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
