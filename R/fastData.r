#' Get one of the example rasters or spatial vectors
#'
#' This function is a simple way to get example rasters or spatial vector datasets that come with **fasterRaster**.
#'
#' @param x The name of the raster or spatial vector to get. All of these represent a portion of the eastern coast of Madagascar.
#'
#' Spatial vectors (objects of class `sf` from the **sf** `package):
#' * `madCoast0: Outline of the region (polygon)
#' * `madCoast4`: Outlines of the Fokontanies (Communes) of the region (polygons)
#' * `madDypsis`: Records of plants of the genus *Dypsis* (points)
#' * `madRivers`: Major rivers (lines)
#'
#' Rasters (objects of class `SpatRaster` from the **terra** package):
#' * `madChelsa`: Bioclimatic variables
#' * `madElev`: Elevation
#' * `madElevAnt`: Elevation of the Antanambe Commune
#' * `madElevMan`: Elevation of the Manompana Commune
#' * `madForest2000`: Forest cover in year 2000
#' * `madForest2014`: Forest cover in year 2014
#'
#' @return A `SpatRaster` or `sf` spatial vector.
#'
#' @seealso [madCoast0], [madCoast4], [madDypsis], [madElev], [madElevAnt], [madElevMan], [madForest2000], [madForest2014], [madRivers]
#'
#' @example man/examples/ex_fastData.r
#'
#' @export

fastData <- function(x) {

	vectors <- c("madCoast0", "madCoast4", "madDypsis", "madRivers")
	rasters <- c("madChelsa", "madElev", "madElevAnt", "madElevMan", "madForest2000", "madForest2014")

	if (!inherits(x, "character")) {
		stop("Please supply the name of an example raster or spatial vector in fasterRaster.")
	} else {
		if (x %in% vectors) {

			madCoast0 <- madCoast4 <- madDypsis <- madRivers <- NULL
			out <- do.call(utils::data, list(x, envir=environment(), package="fasterRaster"))
			out <- get(out)

		} else if (x %in% rasters) {
			rastFile <- system.file("extdata", paste0(x, ".tif"), package="fasterRaster")
			out <- terra::rast(rastFile)
		} else {
			stop("Please supply the name of a data object available in fasterRaster.")
		}
	}

	out

}
