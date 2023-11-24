#' Initialize a "GRASS" session
#'
#' @description This function initializes a **GRASS** session in a particular folder. You need to run this function (usually just once) before you use most functions in **fasterRaster**. You can use [restoreSession()] to switch between [working folders, locations, and mapsets][tutorial_sessions].
#'
#' @param x Any object from which a coordinate reference system (CRS) can be acquired. Ergo, any of:
#' * A `SpatRaster`, `SpatVector`, `SpatExtent`, `stars`, or `sf` object
#' * A `crs` object (i.e., from [sf::st_crs()]).
#' * A CRS (coordinate reference system) WKT string. Some PROJ4 strings *might* work, too.
#'
#' @param grassDir Character or `NULL` (default): Folder in which **GRASS** is installed on your computer. This will look different depending on the operating system and version of **GRASS** you have installed. Here are some examples:
#' * Windows: `"C:/Program Files/GRASS GIS 8.3"`
#' * Mac OS: `"/Applications/GRASS-8.3.app/Contents/Resources"`
#' * Linux: `"/usr/local/grass"`
#' If `NULL`, then the function will use [getFastOptions()] to attempt to get it. If it fails, `grassDir` will stay as `NULL` and likely result in an error.
#'
#' @param addonDir Character or `NULL` (deafult): Folder in which **GRASS** add-ons are stored. If `NULL`, this is assumed to be in `file.path(grassDir, "addons")`.
#' 
#' @param workDir `NULL` or character: The name of the folder in which **GRASS** will store rasters and vectors. If this is `NULL` (default), then the [tempdir()] on the user"s system will be used. If users wish to create persistent **GRASS** sessions that can be used in a different instance of **R** (i.e., if **R** is stopped then restarted), then this needs to be specified.
#'
#' @param overwrite Logical: If `FALSE` (default), and a **GRASS** session in the stated (or default) location and mapset has already been started, then the function will fail. If `TRUE`, then any existing **GRASS** session will be overwritten. *NOTE*: This will **not** remove any **R** objects associated with rasters or vectors in the session, but they will no longer work because the objects they point to will be overwritten.
#
#' @param warn Logical: If `TRUE` (default) and `overwrite` is `TRUE`, then display a warning.
#'
#' @param ... Options to send to [setFastOptions()]. These should be in `option = value` format.
#'
#' @return A [GSession] object (invisibly).
#'
#' @seealso [Guide][tutorial_getting_started] to getting started with **fasterRaster**.
#'
#' @example man/examples/ex_faster.r
#'
#' @export
faster <- function(
	x,
	grassDir = NULL,
	addonDir = NULL,
	workDir = NULL,
	overwrite = FALSE,
	warn = TRUE,
	...
) {

	# for debugging
	if (FALSE) {

		grassDir <- "C:/Program Files/GRASS GIS 8.3" # Windows
		addonDir <- NULL
		dots <- list()
		workDir <- NULL
		x <- madElev
		overwrite <- TRUE
		warn <- TRUE
	
	}

	rgrass::set.ignore.stderrOption(!getFastOptions("verbose"))

	### function globals
	dots <- list(...)
	if (is.null(workDir)) workDir <- getFastOptions("workDir")
	workDir <- forwardSlash(workDir)
	dir.create(workDir, showWarnings=FALSE, recursive=TRUE)

	if (is.null(grassDir)) grassDir <- getFastOptions("grassDir")
	if (is.na(grassDir)) grassDir <- NULL
	if (!is.null(grassDir) && is.null(addonDir) && is.null(getFastOptions("addonDir"))) addonDir <- file.path(grassDir, "addons")

	mapset <- if (!("mapset" %in% names(dots))) {
		mapset()
	} else {
		dots$mapset
	}

	location <- if (!("location" %in% names(dots))) {
		location()
	} else {
		dots$location
	}

	### CRS
	if (inherits(x, c("SpatRaster", "SpatVector", "SpatExtent"))) {
		x <- terra::crs(x)
	} else if (inherits(x, c("sf", "stars"))) {
		x <- sf::st_crs(x)
	} else if (inherits(x, "crs")) {
		x <- unclass(x)$wkt
	}
	crsFile <- file.path(workDir, location, "crs.rds")

	### do we need a new GRASS session or to switch the location/working directory?
	if (overwrite & .fasterRaster$grassStarted) {
	
		if (warn) warning(paste0("The GRASS session with these properties has been overwritten:\n  * location: ", location, "\n  * mapset: ", mapset, "\n  * workDir: ", workDir, ".\n  All previously existing files have been removed."), immediate.=TRUE)
		
		rgrass::unset.GIS_LOCK()
		rgrass::remove_GISRC()
		rgrass::unlink_.gislock()

		files <- list.files(file.path(workDir, location, mapset), include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
		files <- rev(files)
		unlink(files, recursive = TRUE)
		unlink(file.path(workDir, location, mapset), recursive = TRUE)
	
	}
		
	# are we trying to restart same folder, etc. but with a different CRS?
	if (!overwrite & length(crsFile) > 0L && file.exists(crsFile)) {
		
		existingCrs <- readRDS(crsFile)
		if (existingCrs != x) {

			stop("The active GRASS session has a different coordinate reference system.\n  Either use the same CRS or a different GRASS ", sQuote("location"), " (see ?location)\n  or use argument overwrite = TRUE (which unlinks all previous GRasters and GVectors).")

		}
	}
		
	### start new GRASS session
	if (inherits(x, "crs")) x <- x$wkt
	emptyRast <- terra::rast(matrix(1L), type="xy", crs=x)

	### start the GRASS session
	suppressWarnings(
		session <- rgrass::initGRASS(
			gisBase = grassDir,
			addon_base = addonDir,
			home = workDir,
			SG = emptyRast,
			location = location,
			mapset = mapset,
			override = TRUE, # must be TRUE to restart, even in different location/mapset
			remove_GISRC = overwrite, # ???
			ignore.stderr = TRUE
		)
	)
		
	### set options
	setFastOptions(grassDir = grassDir, addonDir = addonDir, workDir = workDir)
	if (length(dots) > 0L) setFastOptions(...)
	.fasterRaster$grassStarted <- TRUE
	
	saveRDS(x, file=crsFile)
	
	session <- GSession(
		location = location,
		mapset = mapset,
		crs = x,
		workDir = workDir
	)

	### remember session
 	sessionNames <- names(.fasterRaster$locations)
	thisSessionName <- paste0(location, "_", mapset)

 	match <- which(sessionNames == thisSessionName)
	if (length(match) == 0L) {
		put <- length(.fasterRaster$locations) + 1L
	} else {
		put <- match
	}

	.fasterRaster$locations[[put]] <- session
	names(.fasterRaster$locations)[put] <- thisSessionName

	invisible(session)

}
