#' Get one of the example rasters or spatial vectors
#'
#' This function is a simple way to get one the several example rasters or spatial vector datasets that come with \pkg{fasterRaster}. These include.
#'
#' @param x The name of the raster or spatial vector to get. All of these represent a portion of the eastern coast of Madagascar. Options include:
#'	\itemize{
#'		\item	\code{'madCoast0'}: Outline of the region (\code{sf} spatial vector).
#'		\item	\code{'madCoast4'}: Outlines of the Fokontanies (Communes) of the region (\code{sf} spatial vector).
#'		\item	\code{'madRivers'}: Major rivers (\code{sf} spatial vector).
#'		\item	\code{'madElev'}: Elevation (\code{SpatRaster}).
#'		\item	\code{'madForest2000'}: Forest cover in year 2000 (\code{SpatRaster}).
#'		\item	\code{'madForest2014'}: Forest cover in year 2014 (\code{SpatRaster}).
#' }
#'
#' @return A \code{SpatRaster} or \code{sf} spatial vector.
#'
#' @seealso \code{\link{madCoast0}}, \code{\link{madCoast4}}, \code{\link{madElev}}, \code{\link{madForest2000}}, \code{\link{madForest2014}}, \code{\link{madRivers}}
#'
#' @example man/examples/ex_madData.r
#'
#' @export

fasterData <- function(x) {

	if (!inherits(x, 'character')) {
		stop('Please supply the name of an example raster or spatial vector in fasterRaster.')
	} else {
		if (x %in% c('madCoast0', 'madCoast4', 'madRivers')) {
			if (x == 'madCoast0') {
				madCoast0 <- NULL
			} else if (x == 'madCoast4') {
				madCoast4 <- NULL
			} else if (x == 'madRivers') {
				madRivers <- NULL
			}
			out <- do.call(utils::data, list(x, envir=environment(), package='fasterRaster'))
			if (x == 'madCoast0') {
				out <- madCoast0
			} else if (x == 'madCoast4') {
				out <- madCoast4
			} else if (x == 'madRivers') {
				out <- madRivers
			}
		} else if (x %in% c('madElev', 'madForest2000', 'madForest2014')) {
			rastFile <- system.file('extdata', paste0(x, '.tif'), package='fasterRaster')
			out <- terra::rast(rastFile)
		} else {
			stop('Please supply the name of an example raster or spatial vector in fasterRaster.')
		}
	}
	
	out
	
}
