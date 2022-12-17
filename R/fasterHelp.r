#' Lookup equivalent functions in fasterRaster, \code{GRASS}, and terra
#'
#' Lookup equivalent functions in fasterRaster, \pkg{GRASS}, and terra, plus get the URL of the help file for the \code{GRASS} function, if it exists.
#'
#' @param x Name of the function to lookup. This can be a function in \pkg{fasterRaster}, \pkg{terra}, or \pkg{GRASS}. By default, it will be assumed to be the \pkg{fasterRaster} function. If this is \code{NULL} (default), it will return the entire crosswalk table.
#' @param terra,grass,sf If \code{TRUE}, the function named in \code{x} is from \pkg{terra}, \pkg{GRASS}, or \pkg{sf}. Only one of \code{terra}, \code{grass}, and \code{sf} can be \code{TRUE}.
#'
#' @returns Nothing (displays a equivalent functions on the screen).
#'
#' @examples man/examples/ex_fasterHelp.r
#'
#' @export

fasterHelp <- function(x = NULL, terra = FALSE, grass = FALSE, approx = FALSE) {

	utils::data('fasterFunctions', envir=environment(), package='fasterRaster')
	if (is.null(x)) {
		print(fasterFunctions)
	} else if (sum(c(terra, grass, sf)) > 1L) {
		warning('Only one argument of terra, grass, and sf can be TRUE.')
	} else {

		lookup <- if (all(!terra, !grass, !sf)) {
			'fasterRaster'
		} else if (terra) {
			'terra'
		} else if (grass) {
			'grass'
		} else if (sf) {
			'sf'
		}

		this <- if (approx) {
			agrepl(x, fasterFunctions[ , lookup, drop = TRUE])
		} else {
			grepl(x, fasterFunctions[ , lookup, drop = TRUE])
		}
		
		this <- which(this)
	
		if (length(this) > 0L) {
			out <- fasterFunctions[this, , drop = FALSE]
			rownames(out) <- NULL
			print(out)
		} else if (!approx) {
			cat('Function not found. You could try searching using an approximate match (set approx = TRUE).')
		} else {
			cat('Function not found. You could try looking through the functions table: data(fasterFunctions)).')
		}
	
	}

}
