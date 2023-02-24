#' Is GRASS session started? Looks in .fasterRaster environment
#'
#' Private.
#' @keywords internal

.isGrassStarted <- function() {

	if (exists('.fasterRaster', where=globalenv())) {
		get('grassStarted', pos=.fasterRaster)
	} else {
		FALSE
	}

}

.grassIsStarted <- function() {

	if (exists('.fasterRaster', where=globalenv())) {
		local({grassStarted <- TRUE}, envir=.fasterRaster)
		invisible(TRUE)
	} else {
		invisible(FALSE)
	}

}
