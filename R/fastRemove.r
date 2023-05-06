#' Delete a GRASS session (location, mapset(s), and all associated files)
#'
#' @description `fastRemove()` deletes a **GRASS** "session's" [location and mapsets][tutorial_sessions] and all rasters and vectors therein. This function should be used cautiously, and is mainly provided as a way to clean up after running examples.
#'
#' @param location Character: Name of the **GRASS** location.
#' @param mapset Character: Either `NULL` (default) or the name of the mapset to delete. If `NULL`, then all mapsets in the given location will be deleted.
#' @param workDir Character: Either `NULL` (default) or a character string of the directory in which the location to be removed resides. If `NULL`, then the working directory will be obtained from `getFastOptions('workDir')`.
#'
#' @returns If successful, `TRUE` (invisibly). If not, then `FALSE` (also invisibly), plus a warning.
#'
#' @examples
#'
#' # See nearly any example using a fasterRaster function for
#' # how fastRemove() can be used.
#'
#' @export
fastRemove <- function(
	location,
	mapset = NULL,
	workDir = NULL
) {

	if (is.null(workDir)) workDir <- getFastOptions('workDir')

	path <- if (is.null(mapset)) {
		file.path(workDir, location)
	} else {
		file.path(workDir, location, mapset)
	}

	if (!file.exists(path)) {
		warning('The specified <location>, <mapset>, and/or <workDir> do not exist.')
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

	}

	invisible(out)

}
