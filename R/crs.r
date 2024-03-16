#' Coordinate reference system of a GRaster or GVector
#' 
#' @description Get the coordinate reference system (CRS) of a `GLocation` object, or any that contain it (especially `GRaster`s and `GVector`s), or from the currently active **GRASS** [location][tutorial_locations_mapsets].
#'
#' @param x An object that inherits from a `GLocation` (i.e., a `GRaster` or `GVector`) or missing. If missing, the coordinate reference system of the currently active **GRASS** [location][tutorial_locations_mapsets] is reported.
#' 
#' @param ... Other arguments (generally unused).
#'
#' @return Character.
#' 
#' @seealso [terra::crs()], [sf::st_crs()]
#' 
#' @example man/examples/ex_GRaster.r
#'
#' @aliases crs
#' @rdname crs
#' @exportMethod crs
methods::setMethod(
    f = "crs",
    signature = c(x = "missing"),
    definition = function(x) {

	locs <- .locations()
	al <- .fasterRaster$activeLocation
	locs[[al]]

    } # EOF
)

#' @aliases crs
#' @rdname crs
#' @exportMethod crs
methods::setMethod(
	f = "crs",
	signature = "GLocation",
	definition = function(x) x@crs
)

#' @rdname crs
#' @aliases st_crs
#' @exportMethod st_crs
methods::setMethod(
	f = "st_crs",
	signature = "missing",
	definition = function(x, ...) {
		
	out <- crs()
	sf::st_crs(out)

	} # EOF
)

#' @rdname crs
#' @aliases st_crs
#' @exportMethod st_crs
methods::setMethod(
	f = "st_crs",
	signature = "GLocation",
	definition = function(x, ...) {

	out <- x@crs
	out <- sf::st_crs(out)
	out

	} # EOF
)

#' @aliases st_crs
#' @rdname crs
#' @export
st_crs <- function(x, ...) UseMethod("st_crs", x)
