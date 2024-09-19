#' Delete a "GRASS" "location"
#'
#' @description This function deletes a **GRASS** "project"/"location", rasters, and vectors therein. This function should be used cautiously and is mainly of use to developers. See `vignette("10_projects_locations_mapsets", package = "fasterRaster")`.
#'
#' @param location Character: Name of the **GRASS** "location".
#' @param mapset Character or `NULL` (default): Name of the mapset to delete. If `NULL`, then all mapsets in the given "location" will be deleted.
#' @param workDir Character: Either `NULL` (default) or a character string of the directory in which the location to be removed resides. If `NULL`, then the working directory will be obtained from `faster("workDir")`.
#'
#' @returns If successful, `TRUE` (invisibly). If not, then `FALSE` (also invisibly), plus a warning.
#'
#' @example man/examples/ex_location_mapset.r
#'
#' @rdname location
#' @keywords internal
.locationDelete <- function(
	location,
	mapset = NULL,
	workDir = NULL
) {

	if (is.null(workDir)) workDir <- faster("workDir")

	path <- if (is.null(mapset)) {
		file.path(workDir, location)
	} else {
		file.path(workDir, location, mapset)
	}

	if (!file.exists(path)) {
		warning("The specified <location>, <mapset>, and/or <workDir> do not exist.")
		out <- FALSE
	} else {

		rgrass::unset.GIS_LOCK()
		rgrass::remove_GISRC()
		rgrass::unlink_.gislock()

		files <- list.files(path, include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
		files <- rev(files)
		unlink(files, recursive = TRUE)
		unlink(path, recursive = TRUE)

		out <- TRUE

		# remove location from set of available ones
		if (is.null(mapset)) {
		
			index <- .locationFind(location, match = "name")
	
			.fasterRaster[[index]] <- NULL
			
			if (.location() == location) .fasterRaster$activeLocation <- NA_character_
	
		}

	}

	invisible(out)

}
