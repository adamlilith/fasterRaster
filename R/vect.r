#' Convert a GVector to a SpatVector or sf vector
#'
#' @description The **fasterRaster** version of the `vect()` function converts a `GVector` to a `SpatVector` (from the **terra** package). The **fasterRaster** version of the `st_as_sf()` function converts a `GVector` to an `sf` object (**sf** package).
#'
#' @param x A `GVector`.
#' @param ... Additional arguments to send to [writeVector()].
#'
#' @returns `vect()` returns a `SpatVector` (**terra** package), and `st_as_sf()` returns an `sf` vector (**sf** package).
#' 
#' @seealso [terra::vect()], [sf::st_as_sf()]
#' 
#' @example man/examples/ex_GVector.r
#'
#' @aliases vect
#' @rdname vect
#' @exportMethod vect
setMethod(
	"vect",
	signature(x = "GVector"),
	function(x, ...) {

	filename <- paste0(omnibus::forwardSlash(tempfile()), ".gpkg")
	writeVector(x, filename = filename, ...)
	out <- terra::vect(filename)
	out
	
	} # EOF
)

#' @aliases st_as_sf
#' @rdname vect
#' @exportMethod st_as_sf
setMethod(
	"st_as_sf",
	signature(x = "GVector"),
	function(x) {

	out <- vect(x)
	sf::st_as_sf(out)

	} # EOF
)

st_as_sf <- function(x) UseMethod("st_as_sf", x)
