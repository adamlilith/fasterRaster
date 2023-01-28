#' Import a raster or vector from a GRASS session into R
#'
#' This function imports a raster or vector that is in an active \code{GRASS} session back to \code{R}. It is a wrapper for the \code{\link[rgrass]{read_RAST}} and \code{\link[rgrass]{read_VECT}} functions in the \pkg{rgrass} package. However, for rasters, it resizes and resamples the \link{region} so that the raster is not inadvertently cropped, expanded, or resampled. Vectors are unaffected by the region, so it is otherwise a plain wrapper for that function.
#'
#' @inheritParams .sharedArgs_outVectClass
#' @param x The name of a raster or vector in the active \code{GRASS} session.
#' @param rastOrVect If \code{NULL} (default), then the function will attempt to guess whether \code{x} refers to a raster or vector. In \code{GRASS}, it is possible to have a raster and a vector of the same name. If this is the case, then you can specify whether you want to resize the region based on the raster or vector by setting this argument to \code{'raster'} or \code{'vector'} (partial matching is supported).
#' @param trim If \code{TRUE} (default), runs \code{\link{fasterTrim}} on the raster first.
#'
#' @seealso \code{\link[rgrass]{read_RAST}} and \code{\link[rgrass]{read_VECT}} in package \pkg{rgrass}
#'
#' @return A \code{SpatRaster}.
#'
#' @example man/examples/ex_fasterRast_fasterVect.r

importFromGrass <- function(
	x,
	rastOrVect = NULL,
	trim = TRUE,
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector')
) {

	rastOrVect <- .getRastOrVect(rastOrVect, n=1, nullOK=TRUE)
	if (is.null(rastOrVect)) {
	
		spatials <- fasterLs()
		matches <- spatials[spatials %in% x]
		if (length(matches) > 1L) stop('More than one match for "x". Please specify\nif you want the raster or vector using "rastOrVect".')
		rastOrVect <- names(matches)

	}
	
	if (any(rastOrVect == 'raster')) {
		out <- fasterWriteRaster(x, paste0(tempfile(), '.tif'), overwrite=TRUE)
		if (trim) out <- terra::trim(out)
		out <- terra::setMinMax(out)
		out
	} else if (any(rastOrVect == 'vector')) {
		out <- rgrass::read_VECT(x, flags='quiet')
		if (.getOutVectClass(outVectClass) == 'sf') out <- sf::st_as_sf(out)
	} else {
		stop('Argument "x" does not match a raster or vector in the active GRASS session.')
	}
	
	out

}
