#' Information on a raster or vector in a \code{GRASS} session
#'
#' Displays information on a raster or vector already in an R session. For further
#'
#' @param name The name of the raster or vector in the R session. This must be in quotes.
#' @param flags Either \code{NULL} (default) or flags to affect the information shown. Flags should be in quotes and without the preceeding "-" sign (example: \code{flags = 'e'}).
#' @return Nothing (displays information on the raster or vector).
#'
#' @seealso \code{\link{fasterLs}}, \href{https://grass.osgeo.org/grass82/manuals/r.info.html}{\code{r.info}} and \href{https://grass.osgeo.org/grass82/manuals/v.info.html}{\code{v.info}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export
fasterInfoRast <- function(name, flags = NULL) {
	rgrass::execGRASS('r.info', map=name, flags=NULL)
}

#' @name fasterInfoVect
#' @title Information on a raster or vector in a \code{GRASS} session
#' @rdname fasterInfoRast
#' @export
fasterInfoVect <- function(name, flags = NULL) {
	rgrass::execGRASS('v.info', map=name, flags=NULL)
}
