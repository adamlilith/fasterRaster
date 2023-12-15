#' Revert to a previously-created "GRASS" "location"
#'
#' @description This function resets the connection to a previously-created **GRASS** "location". The session must have been already created using [fast()] in the current **R** session. This function is typically only of use to developers.
#'
#' @param x Either:
#' * A character: Name of the "location" in **GRASS**.
#' * An integer: Index of the "location" in `.fasterRaster$locations`.
#' * A `GSpatial` object (usually a `GRaster` or `GVector`).
#'
#' @param match Character: Method used to find the location. If `match` is "`name`"" (default), then the name of the location is used. If `match` is "`crs`", then the coordinate reference system of each location is checked for a match.
#'
#' @return An object of class `GLocation` (invisibly) if successful. An error will likely result if not.
#'
#' @aliases .locationRestore
#' @noRd
methods::setMethod(
	f = ".locationRestore",
	signature = c(x = "character"),
	function(x, match = "name") ..restoreLocation(x, match = match)
)

#' @aliases .locationRestore
#' @noRd
methods::setMethod(
	f = ".locationRestore",
	signature = c(x = "integer"),
	function(x, match = "name") ..restoreLocation(x, match = match)
)

#' @aliases .locationRestore
#' @noRd
methods::setMethod(
	f = ".locationRestore",
	signature = c(x = "numeric"),
	function(x, match = "name") ..restoreLocation(x, match = match)
)

#' @aliases .locationRestore
#' @noRd
methods::setMethod(
	f = ".locationRestore",
	signature = c(x = "GSpatial"),
	function(x, match = "name") ..restoreLocation(x, match = match)
)

#' @noRd
..restoreLocation <- function(x, match) {

	index <- .locationFind(x = x, return = "index", match = match)

	if (is.null(index)) {
		stop("Location has not been created.")
	}

	location <- names(.fasterRaster$locations)[index]
	coordRef <- .fasterRaster$locations[[index]]

	opts <- faster()
	grassDir <- opts$grassDir
	addonsDir <- opts$addonsDir
	workDir <- 	opts$workDir
	mapset <- "PERMANENT"

	if (location != .fasterRaster$activeLocation) {

		### reconnect to location
		emptyRast <- terra::rast(matrix(1L), type = "xy", crs = coordRef)

		### start the GRASS session
		suppressWarnings(
			session <- rgrass::initGRASS(
				gisBase = grassDir,
				addon_base = addonsDir,
				home = workDir,
				SG = emptyRast,
				location = location,
				mapset = mapset,
				override = TRUE, # must be TRUE to restart, even in different location/mapset
				remove_GISRC = FALSE, # ???
				ignore.stderr = TRUE
			)
		)

		.fasterRaster$activeLocation <- location

	}

	session <- GLocation(
		location = location,
		mapset = mapset,
		crs = coordRef,
		workDir = workDir
	)

	invisible(session)

}
