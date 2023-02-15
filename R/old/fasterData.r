#' Get one of the example rasters or spatial vectors
#'
#' This function is a simple way to get one the several example rasters or spatial vector datasets that come with \pkg{fasterRaster}. These include.
#'
#' @param x The name of the raster or spatial vector to get. All of these represent a portion of the eastern coast of Madagascar. \cr
#' Spatial vectors (objects of class \code{sf} from the \pkg{sf} package):
#'	\itemize{
#'		\item	\code{'madCoast0'}: Outline of the region (polygon).
#'		\item	\code{'madCoast4'}: Outlines of the Fokontanies (Communes) of the region (polygons).
#'		\item	\code{'madDypsis'}: Records of plants of the genus \emph{Dypsis} (points).
#'		\item	\code{'madRivers'}: Major rivers (lines).
#' }
#' Rasters (objects of class \code{SpatRaster} from the \pkg{terra} package):
#' \itemize{
#'		\item	\code{'madChelsa'}: Bioclimatic variables (\code{SpatRaster}).
#'		\item	\code{'madElev'}: Elevation (\code{SpatRaster}).
#'		\item	\code{'madElevAnt'}: Elevation of the Antanambe Commune(\code{SpatRaster}).
#'		\item	\code{'madElevMan'}: Elevation of the Manompana Commune (\code{SpatRaster}).
#'		\item	\code{'madForest2000'}: Forest cover in year 2000 (\code{SpatRaster}).
#'		\item	\code{'madForest2014'}: Forest cover in year 2014 (\code{SpatRaster}).
#' }
#'
#' @return A \code{SpatRaster} or \code{sf} spatial vector.
#'
#' @seealso \code{\link{madCoast0}}, \code{\link{madCoast4}}, \code{\link{madDypsis}}, \code{\link{madElev}}, \code{\link{madElevAnt}}, \code{\link{madElevMan}}, \code{\link{madForest2000}}, \code{\link{madForest2014}}, \code{\link{madRivers}}
#'
#' @example man/examples/ex_madData.r
#'
#' @export

fasterData <- function(x) {

	vectors <- c('madCoast0', 'madCoast4', 'madDypsis', 'madRivers')
	rasters <- c('madChelsa', 'madElev', 'madElevAnt', 'madElevMan', 'madForest2000', 'madForest2014')

	if (!inherits(x, 'character')) {
		stop('Please supply the name of an example raster or spatial vector in fasterRaster.')
	} else {
		if (x %in% vectors) {
			if (x == 'madCoast0') {
				madCoast0 <- NULL
			} else if (x == 'madCoast4') {
				madCoast4 <- NULL
			} else if (x == 'madDypsis') {
				madDypsis <- NULL
			} else if (x == 'madRivers') {
				madRivers <- NULL
			}
			out <- do.call(utils::data, list(x, envir=environment(), package='fasterRaster'))
			if (x == 'madCoast0') {
				out <- madCoast0
			} else if (x == 'madCoast4') {
				out <- madCoast4
			} else if (x == 'madDypsis') {
				out <- madDypsis
			} else if (x == 'madRivers') {
				out <- madRivers
			}
		} else if (x %in% rasters) {
			rastFile <- system.file('extdata', paste0(x, '.tif'), package='fasterRaster')
			out <- terra::rast(rastFile)
		} else {
			stop('Please supply the name of a data object available in "fasterRaster".')
		}
	}
	
	out
	
}
