#' Connect to "GRASS"
#'
#' @description This function initializes a **GRASS** "project" (previously known in **GRASS** as a "location"; see `vignette("projects_mapsets", package = "fasterRaster")`). You need to run this function (often just once) before you use most functions in **fasterRaster**. This function is of use to developers, not most users.
#'
#' @param x Any object from which a coordinate reference system (CRS) can be acquired. Ergo, any of:
#' * A `SpatRaster`, `SpatVector`, `SpatExtent`, `stars`, or `sf` object
#' * A `crs` object (i.e., from [sf::st_crs()]).
#' * A CRS (coordinate reference system) WKT string. Some PROJ4 strings *might* work, too.
#'
#' @param location Character or `NULL` (default): Name of the location.
#'
#' @param overwrite Logical: If `FALSE` (default), and a **GRASS** "coordinate reference frame" with the given name has already been created, then the function will fail. If `TRUE`, then the existing **GRASS** "coordinate reference frame" of the same name will be overwritten. *NOTE*: This will **not** remove any **R** objects associated with rasters or vectors in the "location", but they will no longer work because the objects they point to will be overwritten.
#
#' @param warn Logical: If `TRUE` (default) and `overwrite` is `TRUE`, then display a warning.
#'
#' @return A [GLocation] object (invisibly).
#'
#' @aliases .locationCreate
#' @rdname locationCreate
#' @keywords internal
methods::setMethod(
	f = ".locationCreate",
	signature = c(x = "character"),
	function(x, location = NULL, overwrite = FALSE, warn = TRUE) ..connectToGRASS(x = x, location = location, overwrite = overwrite, warn = warn)
)

#' @aliases .locationCreate
#' @rdname locationCreate
#' @keywords internal
methods::setMethod(
	f = ".locationCreate",
	signature = c(x = "SpatRaster"),
	function(x, location = NULL, overwrite = FALSE, warn = TRUE) ..connectToGRASS(x = x, location = location, overwrite = overwrite, warn = warn)
)

#' @aliases .locationCreate
#' @rdname locationCreate
#' @keywords internal
methods::setMethod(
	f = ".locationCreate",
	signature = c(x = "SpatVector"),
	function(x, location = NULL, overwrite = FALSE, warn = TRUE) ..connectToGRASS(x = x, location = location, overwrite = overwrite, warn = warn)
)

#' @aliases .locationCreate
#' @rdname locationCreate
#' @keywords internal
methods::setMethod(
	f = ".locationCreate",
	signature = c(x = "sf"),
	function(x, location = NULL, overwrite = FALSE, warn = TRUE) ..connectToGRASS(x = x, location = location, overwrite = overwrite, warn = warn)
)

#' @noRd
..connectToGRASS <- function(
	x,
	location = NULL,
	overwrite = FALSE,
	warn = TRUE
) {

	# for debugging
	if (FALSE) {

		x <- madElev
		location <- NULL
		overwrite <- TRUE
		warn <- TRUE
	
	}

	# rgrass::set.ignore.stderrOption(!faster("debug"))

	### CRS
	if (inherits(x, c("SpatRaster", "SpatVector", "SpatExtent"))) {
		coordRef <- terra::crs(x)
	} else if (inherits(x, c("sf", "stars"))) {
		coordRef <- sf::st_crs(x)
	} else if (inherits(x, "crs")) {
		coordRef <- unclass(x)$wkt
	} else if (inherits(x, "character")) {
		coordRef <- x
	}

	if (inherits(coordRef, "crs")) coordRef <- coordRef$wkt

	if (is.null(location)) {
	
		string <- sf::st_crs(x)$input
		niceCRS <- gsub(string, pattern = "\\/", replacement = "")
		niceCRS <- gsub(niceCRS, pattern = "\\(", replacement = "")
		niceCRS <- gsub(niceCRS, pattern = "\\)", replacement = "")
		niceCRS <- gsub(niceCRS, pattern = " ", replacement = "_")
		niceCRS <- gsub(niceCRS, pattern = "__", replacement = "_")
		
		# sf::st_crs(x)$input sometimes returns entire thing
		if (nchar(niceCRS) > 50) {
			between <- gregexpr(string, pattern = "\"")[[1L]]
			niceCRS <- substr(string, between[1L] + 1L, between[2L] - 1L)
		}

		rand <- omnibus::rstring(1L)
		location <- paste0(niceCRS, "_", rand)

	}

	locs <- .locations(warn = FALSE)
	locationExists <- !is.null(locs) && any(location == locs)

	opts <- faster()
	grassDir <- opts$grassDir
	addonsDir <- opts$addonsDir
	workDir <- opts$workDir

	if (is.na(grassDir)) stop("You must specify the folder in which GRASS is installed using faster().")
	
	### do we need a new GRASS session or to switch the location/working directory?
	if (!overwrite & locationExists) {

		stop("Location with same name already exists. If you want to overwrite it, use `overwrite = TRUE`.")

	} else if (overwrite & locationExists) {
	
		if (warn) warning(paste0("The GRASS session with these properties has been overwritten:\n  * location: ", location, "\n  * workDir: ", workDir, ".\n  All previously existing files have been removed."), immediate. = TRUE)
		
		rgrass::unset.GIS_LOCK()
		rgrass::remove_GISRC()
		rgrass::unlink_.gislock()

		fp <- file.path(workDir, location)
		files <- list.files(fp, include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
		files <- rev(files)
		unlink(files, recursive = TRUE)
		unlink(fp, recursive = TRUE)
	
	}
		
	### start new GRASS session
	emptyRast <- terra::rast(matrix(1L), crs = coordRef)

	dir.create(workDir, showWarnings = FALSE, recursive = TRUE)

	### start the GRASS session
	suppressWarnings(
		session <- rgrass::initGRASS(
			gisBase = grassDir,
			addon_base = addonsDir,
			home = workDir,
			gisDbase = workDir, # ?
			SG = emptyRast,
			location = location,
			mapset = "PERMANENT",
			override = TRUE, # must be TRUE to restart, even in different location/mapset
			remove_GISRC = overwrite, # ???
			ignore.stderr = TRUE
		)
	)

	rgrass::set.ignore.stderrOption(!faster("debug"))
	.fasterRaster$grassStarted <- TRUE
	.fasterRaster$grassVersion <- grassInfo("versionNumber")
	
	n <- length(.fasterRaster$locations)
	.fasterRaster$locations[[n + 1L]] <- coordRef
	names(.fasterRaster$locations)[n + 1L] <- location

	.fasterRaster$activeLocation <- location

	session <- GLocation(
		location = location,
		mapset = "PERMANENT",
		crs = coordRef,
		workDir = workDir
	)

	invisible(session)

}
