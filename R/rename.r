#' Rename a raster or vector in an existing GRASS session
#'
#' Rename a raster or vector in an existing **GRASS** session.
#'
#' @param from,to `gnames` of the raster or vector to rename.
#' @param rastOrVect Either `NULL` (default), `"raster"`, or `"vector"`. This specifies the type of object to be renamed. Partial matching is allowed. If left as `NULL` (default), the function will try to identify if the object is a raster or vector, and return an error if there is both a raster and vector of given name. Note that unlike in **R**, **GRASS** can have rasters and vector"s with the same name.
#' 
#' @return The function invisibly returns \code{TRUE} if the desired rasters and/or vectors were named, and \code{FALSE} if raster and/or vector to be renamed did not exist in the \code{GRASS} session. Notably, a raster or vector or both are renamed in an existing \code{GRASS} session.
#'
#' @aliases .rename
#' @noRd
.rename <- function(
	from,
	to,
	rastOrVect = NULL
) {

	### function globals
	####################
	
	flags <- c("quiet", "overwrite")
	rastOrVect <- pmatchSafe(rastOrVect, c("raster", "vector"), error=TRUE)
	
	fromTo <- c(from, to)
	success <- FALSE

	### rename raster
	if (rastOrVect == "raster") {
	
		spatials <- .ls("rasters")

		rgrass::execGRASS("g.rename", raster=fromTo, flags=flags, intern=TRUE)
		success <- TRUE
		
	} else if (rastOrVect == "vector") {
	
		rgrass::execGRASS("g.rename", vector=fromTo, flags=flags, intern=TRUE)
		success <- TRUE
	
	}

	invisible(success)

}
