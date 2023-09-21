#' Coordinate reference system of a GRaster or GVector
#' 
#' @description Get the coordinate reference system (CRS) of a `GSession` object, or any that contain it (especially `GRaster`s and `GVector`s), or from the currently active **GRASS** [location][tutorial_sessions].
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
    signature = c(x = "missing"),
    definition = function(x) {
        workDir <- getFastOptions("workDir")
        loc <- location()
        crsFile <- file.path(workDir, loc, "crs.rds")
        readRDS(crsFile)
    } # EOF
)

#' @aliases crs
#' @rdname crs
#' @exportMethod crs
methods::setMethod(
	f = "crs",
	signature = "GSession",
	definition = function(x) x@crs
)

#' @rdname crs
#' @aliases st_crs
#' @exportMethod st_crs
methods::setMethod(
	f = "st_crs",
	signature = "missing",
	definition = function(x, ...) {
		
	out <- crs(location())
	sf::st_crs(out)

	} # EOF
)

#' @rdname crs
#' @aliases st_crs
#' @exportMethod st_crs
methods::setMethod(
	f = "st_crs",
	signature = "GSession",
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
