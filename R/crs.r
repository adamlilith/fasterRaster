#' Coordinate reference system of a GRaster or GVector
#' 
#' @description Get the coordinate reference system (CRS) of a `GRaster`, `GVector`, `GSpatial`, or `GSession` object, or from the currently active **GRASS** location.
#'
#' @param x An object that inherits from a `GSession` (i.e., a `GRaster` or `GVector`) or missing. If missing, the coordinate reference system of the currently active **GRASS** [location][tutorial_sessions] is reported.
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
	signature = "GSession",
	definition = function(x) x@crs
)

#' @aliases crs
#' @rdname crs
#' @exportMethod crs
methods::setMethod(
	f = "crs",
	signature = c(x = "missing"),
 	definition = function(x) {
    workDir <- getFastOptions("workDir")
	loc <- location()
 	crsFile <- file.path(workDir, loc, "crs.rds")
	readRDS(crsFile)
	} # EOF
)

st_crs <- function(x, ...) UseMethod("st_crs", x)
setMethod("st_crs", definition = function(x, ...) st_crs(x, ...))

#' @rdname crs
#' @export
st_crs <- function(x, ...) {
	if (inherits(x, "GSpatial")) {
		out <- x@crs
		out <- sf::st_crs(out)
	} else {
		out <- sf::st_crs(x, ...)
	}
	out
}

#' @rdname crs
#' @aliases st_crs
#' @exportMethod st_crs
methods::setMethod(
	f = "st_crs",
	signature = "GSession",
	definition = function(x) {
		out <- x@crs
		out <- sf::st_crs(out)
		out
	}
)
#' @rdname crs
#' @aliases st_crs
#' @exportMethod st_crs
methods::setMethod(
	f = "st_crs",
	signature = c(x = "missing"),
	definition = function(x) {
		x <- region()
		out <- sf::st_crs(x)
		out
	}
)
