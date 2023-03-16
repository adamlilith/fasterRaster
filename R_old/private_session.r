#' Get session-wide arguments
#'
#' For internal use only.
#' @param x TRUE or FALSE.
#'
#' @return A character or logical.
#'
#' @details
#' .getSessionStarted / .setSessionStarted: Sets flag in .fasterRaster indicating if GRASS started or not
#' .setSession / .getSession sets/gets session variables
#' .getDir / .getLocation / .getMapset Get session variables
#' @keywords internal 
.getSessionStarted <- function() {
	getOption('sessionStarted', default = FALSE)
}

#' @keywords internal
.setSessionStarted <- function(x) {
	options(sessionStarted = x)
	invisible(x)
}

#' @keywords internal
.setSession <- function(dir, location, mapset) {
	
	old <- getOption('session', NA)

	dir <- if (!missing(dir)) {
		dir
	} else if (!is.null(old$dir)) {
		old$dir
	} else {
		NA
	}

	location <- if (!missing(location)) {
		location
	} else if (!is.null(old$location)) {
		old$location
	} else {
		NA
	}

	mapset <- if (!missing(mapset)) {
		mapset
	} else if (!is.null(old$mapset)) {
		old$mapset
	} else {
		NA
	}

	session <- list(dir = dir, location = location, mapset = mapset)
	
	options(session = session)
	invisible(old)
	
}

#' @keywords internal
.getSession <- function(dir, location, mapset) {
	getOption('session', NA)
}

#' @keywords internal
.getDir <- function() {
	x <- getOption('session', NULL)
	if (!is.null(x)) {
		x$dir
	} else {
		.dirDefault()
	}
}

#' @keywords internal
.getLocation <- function() {
	x <- getOption('session', NULL)
	if (!is.null(x)) {
		x$location
	} else {
		.locationDefault()
	}
}

#' @keywords internal
.getMapset <- function() {
	x <- getOption('session', NULL)
	if (!is.null(x)) {
		x$mapset
	} else {
		.mapsetDefault()
	}
}
