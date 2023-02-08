#' Partial matching of "rastOrVect" input
#'
#' Private. For one or more partial strings for "rasters", "vectors", or NULL, returns "raster", "vector", or NULL.
#'
#' @param rastOrVect String.
#' @param n Number of valid returned values.
#' @param naOK If TRUE, returning NA is OK if indeterminate answer
#'
#' @keywords internal
.matchRastOrVect <- function(rastOrVect, n, naOK, ...) {

	if (length(rastOrVect) > n) stop(paste0('Can only have up to ', n, ' values in "rastOrVect".'))

	# NULL
	if (length(rastOrVect) == 0 && is.null(rastOrVect)) {
		if (naOK) {
			out <- NA
		} else {
			stop('Cannot accept a NULL value for "rastOrVect".')
		}
		
	# NA
	} else if (anyNA(rastOrVect) && !naOK) {
		stop('Cannot accept an NA value in "rastOrVect".')
	
	# strings for rastOrVect
	} else if (!inherits(rastOrVect, 'character')) {
		stop('Argument "rastOrVect" must be a character or character vector.')
	} else {
		
		out <- rep(NA, length(rastOrVect))
		for (i in seq_along(rastOrVect)) {
			match <- pmatch(rastOrVect[i], c('rasters', 'vectors'))
			out[i] <- if (is.na(match)) {
				NA
			} else if (match == 1L) {
				'raster'
			} else if (match == 2L) {
				'vector'
			}
		}
		
		if (anyNA(out) && !naOK) stop(paste0('No match for these values of "rastOrVect": ', paste(rastOrVect[is.na(out)], collapse=' ')))

	}
	
	out

}

#' Discovers whether a named object is a raster or vector
#'
#' @param x 	A SpatRaster, SpatVector, sf, or named object in a GRASS session.
#' @param errorNotFound	If TRUE, then if the object is not found, an error ensues.
#' @param dupsOK	If FALSE, then if there is more than one object of the given name in GRASS, return an error.
#'
#' @keywords internal
.determineRastOrVect <- function(x, errorNotFound = TRUE, dupsOK = FALSE, ...) {

	# x is NA or NULL
	if (is.null(x)) {
	
		if (errorNotFound) {
			stop('Cannot accept NULL or NA for "x".')
		} else {
			out <- NULL
		}
		
	}
	
	# x is not a character string
	if (!inherits(x, 'character')) {
	
		if (inherits(x, 'SpatRaster')) {
			out <- 'raster'
		} else if (inherits(x, c('SpatVector', 'sf'))) {
			out <- 'vector'
		} else {
			stop('Cannot determine if "x" is a raster or vector.')
		}
		
	# partial match of rastOrVect string
	} else {
	
		spatials <- fasterLs(...)

		matches <- match(x, spatials)
		if (errorNotFound && any(is.na(matches))) stop(paste0('Cannot find any object(s) with these name(s): ', paste(x[is.na(matches)], collapse=' ')))

		if (!dupsOK && length(matches) != length(x)) {
		
			dups <- spatials[duplicated(spatials)]
			stop('Ambiguous matches disallowed. At least one raster and\n one vector in GRASS have the same name: ', paste(dups, collapse = ' '))
		
		}
		
		out <- names(spatials[matches])
		
	}
	
	out

}

#' A helper function for gettting argumemnts "x" and "rastOrVect"
#'
#' Private. Used to get proper names for "x" and "rastOrVect".
#'
#' @param x		Names of object(s) in GRASS.
#' @param rastOrVect Either NULL or one value per object in x.
#' @param ... Arguments to pass to fasterLs()
#'
#' @return List.
#'
#' @keywords internal
.rastOrVectAndX <- function(x, rastOrVect, ...) {

	### missing x? use all objects
	if (missing(x)) {
		x <- fasterLs(rastOrVect=rastOrVect, ...)

		if (length(x) > 0L) {
			rastOrVect <- names(x)
		} else {
			stop('No objects are in the current GRASS session.')
		}

	}
	if (!is.null(rastOrVect) && length(rastOrVect) != length(x)) stop('Argument "rastOrVect" must be NULL or have one value per value in "x".')

	rastOrVect <- .matchRastOrVect(rastOrVect=rastOrVect, n=Inf, naOK=TRUE)
	if (all(is.na(rastOrVect))) rastOrVect <- rep(NA, length(x))
	if (anyNA(rastOrVect)) {
		nas <- which(is.na(rastOrVect))
		rastOrVect[nas] <- .determineRastOrVect(x[nas], errorNotFound=TRUE, dupsOK=FALSE, ...)
	}

	list(x = x, rastOrVect = rastOrVect)

}
