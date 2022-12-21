#' Lookup equivalent functions in fasterRaster, \code{GRASS}, and terra
#'
#' Lookup equivalent functions in fasterRaster, \pkg{GRASS}, and terra, plus get the URL of the help file for the \code{GRASS} function, if it exists.
#'
#' @param x Name of the function to lookup. This can be a function in \pkg{fasterRaster}, \pkg{terra}, or \pkg{GRASS}. By default, it will be assumed to be the \pkg{fasterRaster} function. If this is \code{NULL} (default), it will return the entire crosswalk table.
#' @param what Which sofware/package does the function appear in?  Partial matches are OK, and case does not matter. This can be any of:
#' \itemize{
#'	\item \code{'fasterRaster'}: Default
#'	\item \code{'terra'}
#'	\item \code{'GRASS'}
#'	\item \code{'sf'}
#'}
#'
#' @returns Nothing (displays a equivalent functions on the screen).
#'
#' @example man/examples/ex_fasterHelp.r
#'
#' @export

fasterHelp <- function(x = NULL, what = 'fasterRaster', approx = FALSE) {

	fasterFunctions <- NULL # need to do this because to R-CMD-check, it looks like this is not defined anywhere
	utils::data('fasterFunctions', envir=environment(), package='fasterRaster')
	
	softs <- c('fasterRaster', 'GRASS', 'terra', 'sf')
	what <- tolower(what)
	if (!is.na(pmatch(what, 'fasterraster'))) what <- 'fasterRaster'
	if (!is.na(pmatch(what, 'grass'))) what <- 'GRASS'
	
	whichSoft <- pmatch(what, softs)
	if (is.na(whichSoft)) stop('Argument "what" must be any of "fasterRaster", "GRASS", "terra", or "sf".')
	soft <- softs[whichSoft]
	
	if (is.null(x)) {
		out <- fasterFunctions
		out <- out[order(out[ , soft, drop = TRUE]), ]
		rownames(out) <- NULL
		print(out)
		invisible(out)
	} else {

		this <- if (approx) {
			agrepl(x, fasterFunctions[ , soft, drop = TRUE])
		} else {
			grepl(x, fasterFunctions[ , soft, drop = TRUE])
		}
		
		this <- which(this)
	
		if (length(this) > 0L) {
			out <- fasterFunctions[this, , drop = FALSE]
			out <- out[order(out[ , soft, drop = TRUE]), ]
			rownames(out) <- NULL
			print(out)
			invisible(out)
		} else if (!approx) {
			cat('Function not found. You could try searching using an approximate match (set approx = TRUE).')
		} else {
			cat('Function not found. You could try looking through the functions table: data(fasterFunctions)).')
		}
	
	}

}
