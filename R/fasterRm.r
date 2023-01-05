#' Remove rasters/vectors from a GRASS session
#'
#' Remove rasters or vectors from a \code{GRASS} session. This will remove them from \code{GRASS}, but not from R if they exists in R.
#'
#' @param x Name(s) of the rasters and/or vectors to remove. If this is \code{'*'}, then \emph{everything} will be removed.
#' @param pattern If \code{TRUE}, then \code{x} is used as a pattern so that any rasters or vectors matching this pattern will be removed. For example, if an existing \code{GRASS} session has rasters named \code{bio1}, \code{bio2}, and \code{bio3}, plus a vector named \code{bioVector}, then this command will remove them all: \code{fasterRm('bio', pattern = TRUE)}. By default this is \code{FALSE}, so the value(s) in \code{x} must be exact.
#' @param rasters,vectors If \code{TRUE}, remove only files of this type.
#'
#' @return Invisibly returns the number of rasters and/or vectors removed. More notably, this function also removes rasters and/or vectors from a \code{GRASS} session.
#'
#' @seealso \code{\link{fasterLs}}, \href{https://grass.osgeo.org/grass82/manuals/g.remove.html}{\code{g.remove}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterInfo.r
#'
#' @export
fasterRm <- function(x, pattern = FALSE, rasters = TRUE, vectors = TRUE) {
	
	flags <- c('quiet')
	flags <- c(flags, 'f')
	
	files <- fasterLs()
	numFilesStart <- length(files)

	if (x == '*') {
		x <- fasterLs()
	} else if (pattern) {
		theseFiles <- numeric()
		for (i in seq_along(x)) theseFiles <- c(theseFiles, which(grepl(files, pattern=x[i])))
		x <- files[sort(unique(theseFiles))]
	}
	
	types <- names(files)[match(x, files)]
	
	# remove types not wanting to be deleted
	if (!rasters) {
		keeps <- which(types != 'raster')
		x <- x[keeps]
		types <- types[keeps]
	}

	if (!vectors & length(x) > 0L) {
		keeps <- which(types != 'vectors')
		x <- x[keeps]
		types <- types[keeps]
	}

	if (length(x) > 0L) {

		for (i in seq_along(x)) {
			name <- x[i]
			type <- types[i]
			rgrass::execGRASS('g.remove', flags=flags, name=name, type=type)
		}
		
	}

	files <- fasterLs()
	numFilesEnd <- length(files)
	
	out <- numFilesStart - numFilesEnd
	invisible(out)

}
