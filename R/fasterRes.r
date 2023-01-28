#' Information on a raster or vector in a GRASS session
#'
#' Displays information on a raster or vector already in an R session. For further
#'
#' @param x The name of a raster in the active \code{GRASS} session. This must be in quotes.
#' @param names If \code{TRUE} (default), return a named vector.
#'
#' @return A numeric vector with cell dimensions in the east-west and north-south directions.
#'
#' @seealso \code{\link{fasterLs}}, \code{\link{fasterExt}}, \code{\link{fasterDim}}, and \code{\link{fasterRes}} in \pkg{fasterRaster}; \href{https://grass.osgeo.org/grass82/manuals/r.info.html}{\code{r.info}} and \href{https://grass.osgeo.org/grass82/manuals/v.info.html}{\code{v.info}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export

fasterRes <- function(
	rast,
	names = TRUE
) {

	suppressMessages(
		info <- rgrass::execGRASS(
			'r.info',
			flags = 'g',
			map = rast,
			intern = TRUE,
			Sys_show.output.on.console = FALSE,
			echoCmd = FALSE
		)
	)

	ewres <- info[grepl('ewres=', info)]
	nsres <- info[grepl('nsres=', info)]

	ewres <- sub(ewres, pattern='ewres=', replacement='')
	nsres <- sub(nsres, pattern='nsres=', replacement='')

	ewres <- as.numeric(ewres)
	nsres <- as.numeric(nsres)

	out <- c(ewres, nsres)
	if (names) names(out) <- c('ewres', 'nsres')
	out

}
