#' Get or set the number of rows and/or columns of a GRASS region
#'
#' This function either reports the number of rows and columns of a \code{GRASS} \link{region}, or sets the number of rows and columns. \emph{NOTE}: The spatial resolution of the region may be changed to accommodate the desired number of rows and columns.
#'
#' @param x Any of:
#' \itemize{
#'	\item \code{NULL} (default): Reports the number of rows and columns of the current region. Also see argument \code{names}.
#'	\item A \code{SpatRaster} object: Sets the region's number of rows and columns to the dimensions of the raster. Note that this does not export the raster to the \code{GRASS} session.
#'	\item A vector of two numbers representing the number of rows and columns, respectively.
#'	\item The name of a raster in the active \code{GRASS} session: Uses the number of rows and columns of this raster.
#' }
#' @param names If \code{TRUE} (default), then the returned vector will have names. Ignored if \code{x} is non-\code{NULL}.
#'
#' @return Number of rows and columns, or \code{TRUE} (invisibly) if the dimensions of the region were changed. Also resamples the resolution of the \link{region} in the active \code{GRASS} session.
#'
#' @seealso \code{\link{regionRes}}, \code{\link{regionExt}}, and \code{\link{regionReshape}} in \pkg{fasterRaster}; \code{\link[terra]{dim}}, \code{\link[terra]{nrow}}, \code{\link[terra]{ncol}}, and \code{\link[terra]{ncell}} in the \pkg{terra} package; \code{GRASS} module \code{\href{https://grass.osgeo.org/grass82/manuals/g.region.html}{g.region}}
#'
#' @example man/examples/ex_regions.r
#'
#' @export

regionDim <- function(
	x = NULL,
	names = TRUE
) {

	# report region
	if (is.null(x)) {

		info <- rgrass::execGRASS('g.region', flags=c('p', 'u'), intern=TRUE)

		rows <- info[grepl('rows:', info)]
		cols <- info[grepl('cols:', info)]

		rows <- sub(rows, pattern='rows:', replacement='')
		cols <- sub(cols, pattern='cols:', replacement='')

		rows <- trimws(rows)
		cols <- trimws(cols)

		rows <- as.numeric(rows)
		cols <- as.numeric(cols)

		out <- c(rows, cols)
		if (names ) names(out) <- c('rows', 'cols')
		out

	# define region
	} else if (inherits(x, c('SpatRaster', 'character'))) {
	
		if (inherits(x, 'SpatRaster')) {
			dim <- terra::dim(x)
		} else if (inherits(x, 'character'))  {
			if (length(x) > 1L) stop('Argument "x" can only refer to a single raster.')
			dim <- fasterDim(x)
		}

		rows <- rows[1L]
		cols <- cols[2L]

		rgrass::execGRASS('g.region', rows=rows, cols=cols, flags='quiet')
		invisible(TRUE)
		
	
	} else {
		stop('Region dimensions were not changed.')
	}

}
