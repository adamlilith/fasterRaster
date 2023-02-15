#' Coordinate reference system of the current GRASS sesion
#'
#' Display information on the coordinate reference system of the current \code{GRASS} sesion.
#'
#' @param nice If \code{TRUE}, then print the CRS in a formatted manner and return it invisibly. Default is \code{FALSE}.
#'
#' @return Nothing (displays information on the coordinate reference system of the current \code{GRASS} session).
#'
#' @seealso \href{https://grass.osgeo.org/grass82/manuals/g.proj.html}{\code{g.list}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export
fasterCRS <- function(nice = FALSE) {

	location <- .getLocation()
	if (is.na(location)) location <- 'default'

	mapset <- .getMapset()
	if (is.na(mapset)) mapset <- 'PERMANENT'
	
	wkt <- readLines(file.path(tempdir(), location, mapset, 'PROJ_WKT'))
	wkt <- paste(wkt, collapse='\n')
	
	if (nice) {
		cat(wkt)
		cat('\n')
		utils::flush.console()
		invisible(wkt)
	} else {
		wkt
	}

}
