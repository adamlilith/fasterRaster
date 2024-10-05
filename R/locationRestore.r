#' Revert to a previously-created "GRASS" "location"
#'
#' @description This function resets the connection to a previously-created **GRASS** "location". The session must have been already created using [fast()] in the current **R** session. This function is typically only of use to developers.
#'
#' @param x Either:
#' * A character: Name of the "location" in **GRASS**.
#' * An integer: Index of the "location" in `.fasterRaster$locations`.
#' * A `GSpatial` object (usually a `GRaster` or `GVector`).
#'
#' Any of these can be found using `.locationFind()`.
#'
#' @return An object of class `GLocation` (invisibly) if successful. An error will likely result if not.
#'
#' @example man/examples/ex_location_mapset.r
#'
#' @aliases .locationRestore
#' @rdname locationRestore
#' @keywords internal
methods::setMethod(
	f = ".locationRestore",
	signature = c(x = "character"),
	function(x) ..locationRestore(x)
)

#' @aliases .locationRestore
#' @rdname locationRestore
#' @keywords internal
methods::setMethod(
	f = ".locationRestore",
	signature = c(x = "integer"),
	function(x) ..locationRestore(x)
)

#' @aliases .locationRestore
#' @rdname locationRestore
#' @keywords internal
methods::setMethod(
	f = ".locationRestore",
	signature = c(x = "numeric"),
	function(x) ..locationRestore(x)
)

#' @aliases .locationRestore
#' @rdname locationRestore
#' @keywords internal
methods::setMethod(
	f = ".locationRestore",
	signature = c(x = "GSpatial"),
	function(x) ..locationRestore(x)
)

#' @keywords internal
..locationRestore <- function(x) {

	if (inherits(x, "character")) {
		location <- x
	} else if (inherits(x, c("numeric", "integer"))) {
		location <- names(.fasterRaster$locations)[x]
	} else if (inherits(x, "GSpatial")) {
		location <- x@location
	}

	index <- .locationFind(location, return = "index")

	if (is.null(index)) {
		stop("Location has not been created. You must use faster() to set the installation directory where GRASS is installed, then use fast() at least once to start GRASS.")
	}

	opts <- faster()
	grassDir <- opts$grassDir
	addonsDir <- opts$addonsDir
	workDir <- 	opts$workDir
	mapset <- "PERMANENT"

	coordRef <- .fasterRaster$locations[[index]]

	if (location != .fasterRaster$activeLocation) {

		### reconnect to location
		emptyRast <- terra::rast(matrix(1L), crs = coordRef)

		### start the GRASS session
		suppressWarnings(
			session <- rgrass::initGRASS(
				gisBase = grassDir,
				addon_base = addonsDir,
				home = workDir,
				gisDbase = workDir, # ?
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
