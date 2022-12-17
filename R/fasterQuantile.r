#' Quantiles for a raster
#'
#' This function is a potentially faster version of the function \code{quantile(raster, probs)} for calculating the quantiles of a raster. This function will also work on rasters too big to load into memory using the \pkg{terra} package.
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_grassDir
#' @param probs A numeric list of quantiles to calculate.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}}.
#' @return A named vector of the values for each quantile named in \code{probs}.
#' @details See the documentation for the \code{GRASS} module \code{r.quantile}{https://grass.osgeo.org/grass82/manuals/r.quantile.html}.
#' @seealso \code{\link[stats]{quantile}} in the \pkg{base} package and \code{\link[terra]{quantile}} in the \pkg{terra} package
#'
#' @examples man/examples/ex_fasterQuantile.r
#'
#' @export

fasterQuantile <- function(
	rast,
	probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
	grassDir = options()$grassDir,
	...
) {

	flags <- c('quiet', 'overwrite')

	probs <- 100 * probs

	# initialize GRASS
	input <- initGrass(rast=rast, vect=NULL, grassDir=grassDir)

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
