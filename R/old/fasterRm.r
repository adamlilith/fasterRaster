#' Remove rasters/vectors from a GRASS session
#'
#' Remove rasters or vectors from a \code{GRASS} session. This will remove them from \code{GRASS}, but not from R if they exists in R.
#'
#' @param x Any of:
#' \itemize{
#'		\item Name(s) of the raster(s) and/or vector(s) to remove.
#'		\item \code{'*'}: \emph{Everything} will be removed.
#'		\item \code{'*rasters*'}: \emph{All} rasters.
#'		\item \code{'*vectors*'}: \emph{All} vectors.
#' }
#' @param rastOrVect Only used if \code{x} is a character vector. The type of object to delete: either \code{'rasters'} and/or \code{'vectors'}. If \code{NULL} (default), then the function will attempt to guess whether \code{x} refers to a raster or vector. However, in \code{GRASS}, it is possible to have a raster and a vector of the same name. If this is the case, then you can specify whether \code{x} is a raster or vector (partial matching is supported).
#' @param ... Additional arguments to send to \code{\link{fasterLs}}. This includes argument \code{temps} (\code{TRUE} or \code{FALSE}), which determines if temporary files are removed when using \code{*}, \code{*rasters*}, or \code{*vectors}. Temporary files begin with "\code{TEMPTEM_}".
#'
#' @return Invisibly returns the number of rasters and/or vectors removed. More notably, this function also removes rasters and/or vectors from a \code{GRASS} session.
#'
#' @seealso \code{\link{fasterLs}}, \href{https://grass.osgeo.org/grass82/manuals/g.remove.html}{\code{g.remove}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterOperations.r
#'
#' @export
fasterRm <- function(
	x,
	rastOrVect = NULL,
	...
) {
	
	# getting all of a type
	if (missing(x)) stop('Argument "x" cannot be missing.')
	
	rmAll <- FALSE
	if (length(x) == 1L) {
		if (x == '*') {
			rmAll <- TRUE
			x <- fasterLs(rastOrVect=rastOrVect, ...)
			rastOrVect <- names(x)
		} else if (x == '*rasters*') {
			x <- fasterLs(rastOrVect='rasters', ...)
			rastOrVect <- names(x)
		} else if (x == '*vectors*') {
			x <- fasterLs(rastOrVect='vectors', ...)
			rastOrVect <- names(x)
		}
	}
	
	# clean and validate inputs
	info <- .rastOrVectAndX(x=x, rastOrVect=rastOrVect, ...)
	x <- info$x
	rastOrVect <- info$rastOrVect
	
	spatials <- fasterLs(rastOrVect=rastOrVect, temps=TRUE)
	numFilesStart <- length(spatials)
	
	# any to remove
	if (length(x) > 0L) {
		
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials
		for (i in seq_along(x)) {
		
			spatial <- x[i]
			rov <- rastOrVect[i]
			
			rgrass::execGRASS('g.remove', flags=c('quiet', 'f'), type=rov, name=spatial)
		
		}
	
	} # any to remove
	
	# resize region to encompass all
	if (!rmAll) {
		numFilesEnd <- length(fasterLs(rastOrVect=rastOrVect, ...))
	} else {
		numFilesEnd <- 0
	}
	
	out <- numFilesStart - numFilesEnd
	invisible(out)

}
