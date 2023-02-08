#' Lookup equivalent functions in fasterRaster, terra, sf, and GRASS
#'
#' Lookup equivalent functions in \pkg{fasterRaster}, \pkg{terra}, \pkg{sf}, and \code{GRASS}, plus get the URL of the help file(s) for the relevant \code{GRASS} modules.
#'
#' @param x Name of the function to lookup. This can be a function in \pkg{fasterRaster}, \pkg{terra}, or \pkg{GRASS}. By default, it will be assumed to be the \pkg{fasterRaster} function. If this is \code{NULL} (default), it will return the entire crosswalk table.
#' @param what Which software/package does the function appear in?  Partial matches are OK, and case does not matter. This can be any of:
#' \itemize{
#'	\item \code{'fasterRaster'}: Default
#'	\item \code{'terra'}
#'	\item \code{'GRASS'}
#'	\item \code{'sf'}
#'}
#' @param approx If \code{FALSE} (default), look for exact match. If \code{FALSE}, use fuzzy matching.
#' @param grassVer Version of \code{GRASS}. This is used to get the correct help page URLs. This should be a character like \code{'8.2'} or \code{'82'}. It is used to ensure the \code{GRASS} help page URLs are correct for your version of \code{GRASS}. You can set this once for all uses of \code{fasterHelp} using \code{\link{fasterSetOptions}}.
#'
#' @returns A \code{data.frame}.
#'
#' @example man/examples/ex_fasterHelp.r
#'
#' @export

fasterHelp <- function(x = NULL, what = 'fasterRaster', approx = FALSE, grassVer=NULL) {

	fasterFunctions <- NULL # need to do this because to R-CMD-check, it looks like this is not defined anywhere
	utils::data('fasterFunctions', envir=environment(), package='fasterRaster')
	
	grassVer <- fasterGetOptions('grassVer')
	fasterFunctions$grassHelp1 <- gsub(fasterFunctions$grassHelp1, pattern='82', replacement=grassVer)
	fasterFunctions$grassHelp2 <- gsub(fasterFunctions$grassHelp2, pattern='82', replacement=grassVer)
	
	softs <- c('fasterRaster', 'GRASS', 'terra', 'sf')
	what <- tolower(what)
	if (!is.na(pmatch(what, 'fasterraster'))) what <- 'fasterRaster'
	if (!is.na(pmatch(what, 'grass'))) what <- 'GRASS'
	
	whichSoft <- pmatch(what, softs)
	if (is.na(whichSoft)) stop('Argument "what" must be "fasterRaster", "GRASS", "terra", or "sf".')
	soft <- softs[whichSoft]
	
	if (is.null(x)) {
		out <- fasterFunctions
		out <- out[order(out[ , soft, drop = TRUE]), ]
		rownames(out) <- NULL
		out
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
			out
		} else if (!approx) {
			cat('Function not found. You could try searching using an approximate match (set approx = TRUE).')
		} else {
			cat('Function not found. You could try looking through the functions table: data(fasterFunctions)).')
		}
	
	}

}
