#' Meta-data on active GRASS locations
#'
#' @description **GRASS** uses "locations"to store sets of rasters and vectors with the same coordinate reference system (CRS). These rasters and vectors may or may not be in the same actual location on Earth--they just have the same CRS. This function returns information on all of the **GRASS** "locations" that have been initialized. It is mainly useful for developers.
#'
#' NB: **fasterRaster** always uses the "PERMANENT" mapset.
#'
#' @param warn Logical: If `TRUE` (default), display a warning if no **GRASS** "locations" have been created.
#'
#' @returns A named `list`. The names are the "location's" names and the values are the coordinate reference strings.
#'
#' @example man/examples/ex_location_mapset.r
#'
#' @aliases .locations
#' @rdname locations
#' @keywords internal
.locations <- function(warn = TRUE) {

	if (!grassStarted()) {
	
		if (warn) warning("No GRASS locations have been created.")
		out <- NULL
		
	} else {
		out <- .fasterRaster$locations
	}
	out

}
