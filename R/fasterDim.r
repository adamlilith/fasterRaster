#' Number of rows and columns of a raster
#'
#' Reports the number of cells, rows, and/or columns of a raster in a \code{GRASS} session.
#'
#' @param rast Name of a raster in the active \code{GRASS} session.
#' @param names If \code{TRUE} (default), return a named raster.
#'
#' @return A one- or two-element numeric vector with number of rows and columns, or number of cells in the raster.
#'
#' @seealso \code{\link{fasterInfo}}, \code{\link{fasterExt}}, and \code{\link{fasterRes}} in \pkg{fasterRaster}; \code{\link[terra]{dim}}, \code{\link[terra]{nrow}}, \code{\link[terra]{ncol}}, and \code{\link[terra]{ncell}} in package \pkg{terra}; \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.info.html}{\code{r.info}}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export

fasterDim <- function(rast, names = TRUE, ...) {

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

	# dimensions
	rows <- info[grepl(info, pattern='rows=')]
	cols <- info[grepl(info, pattern='cols=')]

	rows <- sub(rows, pattern='rows=', replacement='')
	cols <- sub(cols, pattern='cols=', replacement='')
	
	rows <- as.numeric(rows)
	cols <- as.numeric(cols)

	out <- c(rows, cols)
	if (names) names(out) <- c('rows', 'cols')
	
	out

}

#' @name fasterNcell
#' @title Number of cells of a raster in a GRASS session
#' @rdname fasterDim
#' @export
fasterNcell <- function(rast) {

	# doing this because number of cells is sometimes estimated
	n <- fasterDim(rast)
	n <- prod(n)
	n
	
}

#' @name fasterNcol
#' @title Number of columns in a raster
#' @rdname fasterDim
#' @export
fasterNcol <- function(rast) {

	fasterDim(rast, names=FALSE)[2L]
	
}

#' @name fasterNrow
#' @title Number of rows in a raster
#' @rdname fasterDim
#' @export
fasterNrow <- function(rast) {

	fasterDim(rast, names=FALSE)[1L]
	
}
