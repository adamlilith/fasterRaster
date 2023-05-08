#' GRASS name of data table associated with a GVector
#'
#' Gets the @layerName slot of a `GRaster`, which contains the **GRASS** name of the data table associated with a `GVector`. Note that more than one table can be associated with a **GRASS** vector, so there may be issues with this. This function is mainly for internal use.
#'
#' @param x A `GVector`.
#'
#' @returns Character or NULL if no table is associated with the vector.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @noRd
layerName <- function(x) {
	if (inherits(x@df, 'GFullMetaTable')) {
		x@df@layerName
	} else {
		NULL
	}
}
