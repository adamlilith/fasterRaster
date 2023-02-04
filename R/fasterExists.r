#' Is there a raster or vector of the given name in the active 'GRASS' session?
#'
#' Indicates if a spatial object of the given class exists in the active \code{GRASS} session.
#'
#' @param x Name(s) of the object(s).
#' @param rastOrVect The type of object(s) of \code{x}. This can be \code{'raster'} and/or \code{'vector'}, or \code{NULL} (default). If \code{NULL}, then the function will attempt to ascertain the type of \code{x}. Partial matching is allowed.
#' @param ... Additional arguments to pass to \code{\link{fasterLs}}.
#'
#' @return Logical for each value of \code{x}.
#'
#' @seealso \code{\link{fasterLs}} in \pkg{fasterRaster}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export

fasterExists <- function(
	x,
	rastOrVect = NULL,
	...
) {

	spatials <- fasterLs(rastOrVect=rastOrVect, ...)
	x %in% spatials

}
