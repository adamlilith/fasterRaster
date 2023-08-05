#' List or delete objects in the active GRASS session
#'
#' Display or delete the names of all rasters and/or vectors that have been exported to or created in the active **GRASS** session"s location and mapset.
#'
#' @param type Character: One or more of `"rasters"`, `"vectors"`, `"rasters3d"`, `"groups"`, or missing: Type of object(s) to display. If missing, all objects are displayed.
#' 
#' @param x Character, a `GSpatial` object, or missing (default):
#' * Character:
#'      * Any of `"rasters"` (all rasters), `"vectors"` (all spatial vectors), `"rasters3d"` (3D-rasters), and/or `"groups"` (groups), and all of these types will be deleted. Partial matching is supported. 
#'      * The `gnames` of the object to be deleted. Argument `type` must be specified.
#' * `GSpatial` object (i.e., a `GRaster` or `GVector`): Delete this object.
#' * Missing: Delete everything in the active **GRASS** session.
#' @param type The type of spatial objects to display or delete. This can include `"rasters"` (all rasters), `"vectors"` (all spatial vectors), `"rasters3d"` (3D-rasters), and/or `"groups"` (groups). Partial matching is supported.
#'
#' @param warn Logical: If `TRUE` (default), display warning if no matches or if everything is to be deleted.
#' 
#' @return Character vector of names of **GRASS** objects.
#'
#' @seealso [ls()]
#'
#' @aliases .ls
#' @rdname .ls
#' @export
#' @noRd
.ls <- function(
	type = c("rasters", "vectors", "rasters3d", "groups")
) {

	rov <- c("rasters", "vectors", "rasters3d", "groups")
	if (is.null(type)) type <- rov

	# find rasters and vector
	rasts <- vects <- rasts3d <- vects3d <- groups <- character()
	match <- pmatch(type, rov)
	
	# failure to match
	if (anyNA(match)) {

		warning("No matches.", immediate.=TRUE)
		out <- NULL
		
	# specific match
	} else {
		
		# rasters
		if (any(match == 1L)) {
			rasts <- rgrass::execGRASS("g.list", flags="quiet", type="raster", intern=TRUE, echoCmd=FALSE)
			if (length(rasts) > 0L) names(rasts) <- rep("raster", length(rasts))
			rasts <- sort(rasts)
		}
		
		# 3D rasters
		if (any(match == 3L)) {
			rasts3d <- rgrass::execGRASS("g.list", flags="quiet", type="raster_3d", intern=TRUE, echoCmd=FALSE)
			if (length(rasts3d) > 0L) names(rasts3d) <- rep("raster3d", length(rasts3d))
			rasts3d <- sort(rasts3d)
		}
		
		# vectors
		if (any(match == 2L)) {
			vects <- rgrass::execGRASS("g.list", flags="quiet", type="vector", intern=TRUE, echoCmd=FALSE)
			if (length(vects) > 0L) names(vects) <- rep("vector", length(vects))
			vects <- sort(vects)
		}
		
		# groups
		if (any(match == 4L)) {
			groups <- rgrass::execGRASS("g.list", flags="quiet", type="group", intern=TRUE, echoCmd=FALSE)
			if (length(groups) > 0L) names(groups) <- rep("group", length(groups))
			groups <- sort(groups)
		}
		
		out <- c(rasts, rasts3d, vects, groups)

	}
		
	out
	
}

#' @aliases .rm
#' @rdname .ls
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
		dels <- .gnames(x)
		type <- if (inherits(x, "GRaster")) {
			"raster"
		} else if (inherits(x, "GVector")) {
			"vector"
		}
	} else if (inherits(x, "character")) {
		dels <- .ls(x, warn = FALSE)
		if (is.null(dels)) {
			dels <- x
		} else {
			type <- names(dels)
		}
	}

	for (i in seq_along(dels)) {
		del <- dels[i]
		typ <- type[i]
		rgrass::execGRASS("g.remove", name=del, type=typ, flags=c("quiet", "f"))
	}
	
	invisible(TRUE)

}
