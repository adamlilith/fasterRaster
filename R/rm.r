#' Delete objects in the active GRASS session
#'
#' @description Delete the names of all rasters and/or vectors that have been exported to or created in the active **GRASS** session's location and mapset.
#'
#' @param x Character, a `GSpatial` object, or missing (default):
#' * Character:
#'      * Any of `"rasters"` (all rasters), `"vectors"` (all spatial vectors), `"rasters3d"` (3D-rasters), and/or `"groups"` (groups), and all of these types will be deleted. Partial matching is supported.
#'      * The `sources` of the object to be deleted. Argument `type` must be specified.
#' * `GSpatial` object (i.e., a `GRaster` or `GVector`): Delete this object.
#' * Missing: Delete everything in the active **GRASS** session.
#'
#' @param type The type of spatial objects to delete. This can include `"rasters"` (all rasters), `"vectors"` (all spatial vectors), `"rasters3d"` (3D-rasters), and/or `"groups"` (groups). Partial matching is supported. If missing, all objects are candidates for deletion if they match `x`.
#'
#' @param warn Logical: If `TRUE` (default), display warning if no matches or if everything is to be deleted.
#'
#' @return `TRUE` (invisibly).
#'
#' @seealso [rm()]
#'
#' @aliases .rm
#' @rdname .rm
#' @export
#' @noRd
.rm <- function(x, type = NULL, warn = TRUE) {

	if (missing(x)) {
		if (warn) {
			ans <- readline(prompt="Delete everything in the current GRASS session (Y/n)? ")
			if (ans != "Y") {
				message("Nothing deleted.")
				return(invisible(FALSE))
			}
		}
		dels <- .ls()
		type <- names(dels)
	} else if (inherits(x, c("GSpatial"))) {
		dels <- sources(x)
		type <- if (inherits(x, "GRaster")) {
			"raster"
		} else if (inherits(x, "GVector")) {
			"vector"
		}
	} else if (inherits(x, "character")) {
		dels <- .ls(x)
		if (is.null(dels)) {
			dels <- x
		} else {
			type <- names(dels)
		}
	}

	for (i in seq_along(dels)) {
		
		del <- dels[i]
		typ <- type[i]
		
		rgrass::execGRASS(
			"g.remove",
			name = del,
			type = typ,
			flags = c(.quiet(), "f")
		)
	
	}
	
	invisible(TRUE)

}
