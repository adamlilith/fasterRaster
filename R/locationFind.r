#' Match CRS of a GSpatial object and an existing "GRASS" location
#'
#' @description The function searches the set of available **GRASS** "locations" for one that has a coordinate reference system matching a `GSpatial` object. If none are found, or if no connection with **GRASS** has yet been made, then it returns `NULL`. Otherwise, it returns either the index or the name of the matching location.
#'
#' @param x Either:
#' * Missing: Returns names and coordinate reference system strings of all "locations".
#' * A character representing a coordinate reference system in WKT format
#' * A `SpatRaster`, `SpatVector`, or `sf` vector
#' * A `GSpatial` object (usually a `GRaster` or `GVector`)
#'
#' @param return Either:
#' * `"name"` (default): Returns the name of the "location" with a coordinate reference system the same as `x`.
#' * `"index"`: Returns the index of this "location".
#' * `"crs"`: Returns the coordinate reference system of this "location".
#'
#' @param match Character: Method used to find the location. If `match` is "`name`"" (default), then the name of the location is used. If `match` is "`crs`", then the coordinate reference system of each location is checked for a match.
#'
#' @returns Character, integer, or `NULL` (if no match is found).
#'
#' @examples
#'
#' if (grassStarted) {
#'
#' .locationFind()
#' .locationFind("xyz")
#'
#' }
#'
#' @aliases .locationFind,missing-method
#' @rdname locationFind
#' @keywords internal
methods::setMethod(
	f = ".locationFind",
	signature = c(x = "missing"),
	function(x, return = "name") {

	if (!grassStarted()) {
		out <- NULL
	} else {
	
		return <- omnibus::pmatchSafe(return, c("name", "index", "crs"), nmax = 1L)
		if (return == "name") {
			out <- names(.fasterRaster$locations)
		} else if (return == "index") {
			out <- seq_along(.fasterRaster$locations)
		} else if (return == "crs") {
			out <- .fasterRaster$locations
		}
	
	}
	out
	
	} # EOF
)

#' @aliases .locationFind,GLocation-method
#' @rdname locationFind
#' @keywords internal
methods::setMethod(
	f = ".locationFind",
	signature = c(x = "GLocation"),
	function(x, return = "name", match = "name") ..locationFind(x = x, return = return, match = match)
)

#' @aliases .locationFind,SpatRaster-method
#' @rdname locationFind
#' @keywords internal
methods::setMethod(
	f = ".locationFind",
	signature = c(x = "SpatRaster"),
	function(x, return = "name", match = "name") ..locationFind(x = x, return = return, match = match)
)

#' @aliases .locationFind,SpatVector-method
#' @rdname locationFind
#' @keywords internal
methods::setMethod(
	f = ".locationFind",
	signature = c(x = "SpatVector"),
	function(x, return = "name", match = "name") ..locationFind(x = x, return = return, match = match)
)

#' @aliases .locationFind,sf-method
#' @rdname locationFind
#' @keywords internal
methods::setMethod(
	f = ".locationFind",
	signature = c(x = "sf"),
	function(x, return = "name", match = "name") ..locationFind(x = x, return = return, match = match)
)

#' @aliases .locationFind,character-method
#' @rdname locationFind
#' @keywords internal
methods::setMethod(
	f = ".locationFind",
	signature = c(x = "character"),
	function(x, return = "name", match = "name") ..locationFind(x = x, return = return, match = match)
)

#' @noRd
..locationFind <- function(x, return, match) {

	if (!grassStarted()) {
		out <- NULL
	} else {

		return <- omnibus::pmatchSafe(return, c("name", "index", "crs"), nmax = 1L)
		match <- omnibus::pmatchSafe(match, c("name", "crs"), nmax = 1L)

		if (inherits(x, "character") & match == "name") {
			matches <- names(.fasterRaster$locations) == x
		} else if (inherits(x, c("character", "CRS")) & match == "crs") {
			matches <- sapply(.fasterRaster$locations, terra::same.crs, y = x)
		} else if (inherits(x, c("GLocation")) & match == "name") {
			matches <- names(.fasterRaster$locations) == .location(x)
		} else if (inherits(x, c("GLocation", "SpatRaster", "SpatVector", "sf")) & match == "crs") {
			matches <- sapply(.fasterRaster$locations, terra::same.crs, y = x)
		} else {
			stop("Cannot match the input to a searchable aspect of .fasterRaster$locations.")
		}
		
		index <- which(matches)
		
		if (length(index) == 0L) {
			out <- NULL
		} else {
			
			if (return == "name") {
				out <- names(.fasterRaster$locations)[index]
			} else if (return == "index") {
				out <- index
			} else if (return == "crs") {
				out <- .fasterRaster$locations[[index]]
			}
		}
		
	}
	out

}
