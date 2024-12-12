#' Does the "GRASS" representation of a GRaster or GVector exist?
#'
#' @description `GRaster`s and `GVector`s are **R** objects that contain a pointer to a raster or vector in **GRASS**. Thus, for a `GRaster` or `GVector` to be functional, the **GRASS** file must exist. This function indicates if that is so.
#'
#' @param x A `GRaster`, `GVector`, or the [sources()] name of one.
#'
#' @returns Logical.
#'
#' @aliases .exists
#' @rdname exists
#' @keywords internal
methods::setMethod(
	f = ".exists",
	signature = c(x = "GRaster"),
	function(x) {
	
	.locationRestore(x)
	src <- sources(x)
	src %in% .ls(type = "raster")
	
	} # EOF
)

#' @aliases .exists
#' @rdname exists
#' @keywords internal
methods::setMethod(
	f = ".exists",
	signature = c(x = "GVector"),
	function(x) {
	
	.locationRestore(x)
	src <- sources(x)
	src %in% .ls(type = "vector")
	
	} # EOF
)

#' @aliases .exists
#' @rdname exists
#' @keywords internal
methods::setMethod(
	f = ".exists",
	signature = c(x = "character"),
	function(x) x %in% .ls()
)
