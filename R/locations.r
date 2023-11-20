#' Meta-data on active GRASS locations
#'
#' @description **GRASS** uses ["locations"][tutorial_sessions] to store sets of rasters and vectors with the same coordinate reference system (CRS). These rasters and vectors may or may not be in the same actual location on Earth--they just have the same CRS. This function returns information on all of the **GRASS** "locations" that have been initialized.
#'
#' @returns A `data.frame`.
#'
#' @example man/examples/ex_sessions.r
#'
#' @aliases locations
#' @rdname locations
#' @export
locations <- function() {

	if (!.fasterRaster$grassStarted) {
	
		warning("No GRASS sessions have been initialized.")
		out <- NULL
		
	} else {
	
		locs <- .fasterRaster$locations
		n <- length(locs)
		
		locLocs <- sapply(locs, location)
		locMapsets <- sapply(locs, mapset)
		locCRS <- lapply(locs, crs)
		locCRS <- sapply(locCRS, .showCRS)
		locWorkDirs <- sapply(locs, workDir)
		
		active <- locLocs == location()
		
		out <- data.frame(
			active = active,
			location = locLocs,
			mapset = locMapsets,
			crs = locCRS,
			workDir = locWorkDirs,
			row.names = NULL
		)
		
	}
	out

}
