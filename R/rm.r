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
#' @param warn Logical: If `TRUE` (default), display warning if no matches or if everything in **GRASS** is to be deleted.
#'
#' @param verify Logical: If `TRUE` (default), the function will search for the item(s) to be deleted first to verify they exist. If this is `FALSE`, then `x` and MUST be specified and `type` must be '`raster`' or `'vector'` (one value per value in `x`). This has no effect if `x` is a `GSpatial` object. It's main use is to save a bit of time.
#'
#' @return `TRUE` (invisibly).
#'
#' @seealso [rm()]
#'
#' @aliases .rm
#' @noRd
.rm <- function(x, type = NULL, warn = TRUE, verify = TRUE) {

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
		
		if (verify) {
			
			dels <- .ls()
			dels <- dels[dels %in% x]
			if (length(dels) == 0L) {
				warning("Object(s) are not in GRASS. Nothing deleted.")
				return(invisible(FALSE))
			}
			type <- names(dels)
		
		} else {
			dels <- x
			type <- rep(type, length(dels))
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
