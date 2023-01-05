#' Display the coordinate reference system of the current GRASS sesion
#'
#' Display the coordinate reference system of the current \code{GRASS} sesion.
#'
#' @return Nothing (displays information on the coordinate reference system of the current \code{GRASS} session).
#'
#' @seealso \href{https://grass.osgeo.org/grass82/manuals/g.proj.html}{\code{g.list}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export
fasterCRS <- function() {
	rgrass::execGRASS('g.proj', flags=c('p'))
}
