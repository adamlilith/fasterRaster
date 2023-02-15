#' Lookup equivalent functions in fasterRaster, terra, sf, and GRASS
#'
#' Lookup equivalent functions in \pkg{fasterRaster}, \pkg{terra}, \pkg{sf}, and \code{GRASS}, plus get the URL of the help file(s) for the relevant \code{GRASS} modules.
#'
#' @param x Name of the function to lookup. This can be a function in \pkg{fasterRaster}, \pkg{terra}, or \pkg{GRASS}. By default, it will be assumed to be the \pkg{fasterRaster} function. If this is \code{NULL} or missing (default), it will return the entire crosswalk table, sorted by \code{where}.
#' @param what Which software/package(s) does the search string appear in? Partial matches for packages/software are OK, and case does not matter. This can be any of:
#' \itemize{
#'	\item \code{'fasterRaster'}: Default
#'	\item \code{'terra'}
#'	\item \code{'GRASS'}
#'	\item \code{'sf'}
#'	\item \code{'*'}: All of the above
#'	\item Any other column name in the \code\link{{fasterFunctions}} table
#'	\item \code{NULL}: Show the \emph{entire} set of columns in the table (but you can still subset it using a search with \code{x})
#'}
#' @param approx If \code{FALSE} (default), look for exact match. If \code{FALSE}, use fuzzy matching.
#'
#' @param grassVer Version of \code{GRASS}. This should be a character like \code{'8.2'} or \code{'82'}. It is used to ensure the \code{GRASS} help page URLs are correct for your version of \code{GRASS}. You can set this once for all uses of \code{fasterHelp} using \code{\link{fasterSetOptions}}.
#'
#' @returns A \code{data.frame}.
#'
#' @example man/examples/ex_fasterHelp.r
#'
#' @export

fasterHelp <- function(x, where = 'fasterRaster', approx = FALSE, grassVer=fasterGetOptions('grassVer', 82)) {

	### functions table
	fasterFunctions <- NULL # need to do this because to R-CMD-check, it looks like this is not defined anywhere
	utils::data('fasterFunctions', envir=environment(), package='fasterRaster')
	out <- fasterFunctions

	### what columns to display or search?
	wheres <- names(out)
	if ('*' %in% where) where <- c('fasterRaster', 'GRASS', 'terra', 'sf')

	if (!is.null(where)) {

		where <- tolower(where)
		match <- pmatch(tolower(where), tolower(wheres))
		# if (is.na(match)) stop('Argument "where" must be "fasterRaster", "GRASS", "terra", "sf", or "*" (everything).')
		where <- wheres[match]
		
	} else {
		where <- wheres
	}

	### fix URLs
	out$grassHelp1 <- gsub(out$grassURL1, pattern='82', replacement=grassVer)
	out$grassHelp2 <- gsub(out$grassURL2, pattern='82', replacement=grassVer)
	
	if ((missing(x) || is.null(x)) & is.null(where)) {

		out <- out[order(out$fasterRaster), ]
		out
		
	} else {
		
		if (missing(x) || is.null(x)) {
		
			out <- out[order(out[ , where[1L], drop = TRUE]), ]
			if (!('fasterRaster' %in% where)) where <- c(where, 'fasterRaster')
			if (!('Description' %in% where)) where <- c(where, 'Description')
			out <- out[ , where, drop=FALSE]
			rownames(out) <- NULL
			out
		
		### searching and showing *all* columns
		} else if (is.null(where)) {

			this <- numeric()
			for (thisWhere in names(fasterFunctions)) {
				
				thisThis <- if (approx) {
					agrepl(tolower(x), tolower(fasterFunctions[ , thisWhere, drop = TRUE]))
				} else {
					grepl(x, fasterFunctions[ , thisWhere, drop = TRUE])
				}
				
				if (sum(thisThis) > 0L) {
					thisThis <- which(thisThis)
					this <- c(this, thisThis)
				}
				
			}
			
			out <- out[this, , drop=FALSE]
			rownames(out) <- NULL
			out

		### searching and showing select columns
		} else {

			# search across package columns
			this <- numeric()
			for (thisWhere in where) {
				
				thisThis <- if (approx) {
					agrepl(tolower(x), tolower(fasterFunctions[ , thisWhere, drop = TRUE]))
				} else {
					grepl(x, fasterFunctions[ , thisWhere, drop = TRUE])
				}

				if (sum(thisThis) > 0L) {
					thisThis <- which(thisThis)
					this <- c(this, thisThis)
				}
				
			}
			
			if (length(this) > 0L) {

				this <- sort(unique(this))

				if (!('fasterRaster' %in% where)) where <- c(where, 'fasterRaster')
				if (!('Description' %in% where)) where <- c(where, 'Description')
				out <- fasterFunctions[this, where, drop=FALSE]
				out <- out[order(out[ , where[1L], drop = TRUE]), , drop=FALSE]
				
				rownames(out) <- NULL
				out
				
			} else if (!approx) {
				cat('Function not found. You could try searching using an approximate match (set approx = TRUE).\n')
			} else {
				cat('Function not found. You could try looking through the functions table: data(fasterFunctions)).\n')
			}
		
		}
		
	} # if x is NULL/missing and where is NULL
	
}
