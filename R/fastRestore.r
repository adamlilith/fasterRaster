#' Revert to a previous 'GRASS' session (working folder, location, and mapset)
#'
#' This function is useful for reverting to a previous **GRASS** session (`workDir`, `location`, and/or `mapset`). The session must have been already initiated using [faster()] in the current **R** session or a previous one.
#'
#' @param ... Either a sequence of arguments with the pattern `<argument = value>`, or a list of arguments. These arguments can be any option available in [setFastOptions()]. Of particular note, the `workDir`, `location`, and `mapset` options allow one to revert to a previous **GRASS** session. The current session's `workDir`, `location`, and `mapset` can be seen using [getFastOptions()], plus  [location()] and [mapset()].
#'
#' @return An object of class `gmeta` (invisibly) if successful. An error will likely result if not.
#'
#' @seealso **GRASS** [locations and mapsets](https://grass.osgeo.org/grass82/manuals/grass_database.html)
#'
#' @example man/examples/ex_sessions.r
#'
#' @export
fastRestore <- function(...) {

	dots <- list(...)
	if (length(dots) == 1L && inherits(dots[[1L]], 'list')) dots <- dots[[1L]]

	grassDir <- if ('grassDir' %in% names(dots)) {
		dots$grassDir
	} else {
		getFastOptions('grassDir')
	}

	addonDir <- if ('addonDir' %in% names(dots)) {
		dots$addonDir
	} else {
		getFastOptions('addonDir')
	}

	workDir <- if ('workDir' %in% names(dots)) {
		dots$workDir
	} else {
		getFastOptions('workDir')
	}
	workDir <- forwardSlash(workDir)

	location <- if ('location' %in% names(dots)) {
		dots$location
	} else {
		getFastOptions('location')
	}

	mapset <- if ('mapset' %in% names(dots)) {
		dots$mapset
	} else {
		getFastOptions('mapset')
	}

	# are we trying to restart same folder, etc. but with a different CRS?
	workLocMap <- file.path(workDir, location, mapset)
	if (!file.exists(workLocMap)) {
		stop('This location/mapset does not exist. Use faster() to create a new location/mapset.')
	}

	### start new GRASS session
	file <- file.path(workDir, location, 'crs.rds')
	crs <- readRDS(file)
	emptyRast <- terra::rast(matrix(1), type='xy', crs=crs)

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
			remove_GISRC = FALSE, # ???
			ignore.stderr = TRUE
		)
	)

	### set options
	setFastOptions(dots)
	.fasterRaster$grassStarted <- TRUE

	session <- GSession(
		location = location,
		mapset = mapset,
		crs = crs
	)
	
	invisible(session)

}

#' Hidden function to restore location/mapset based on a GSession object
#'
#' @param x `GSpatial` object. The session will be restored to the location and mapset of this object.
#'
#' @return Session (invisibly).
#'
#' @noRd
.restore <- function(x) {

	xloc <- location(x)
	xms <- mapset(x)

	loc <- getFastOptions('location')
	ms <- getFastOptions('mapset')

	if (loc != xloc | ms != xms) {
		session <- fastRestore(location=xloc, mapset=xms)
	}

	session <- GSession(
		location = location(),
		mapset = mapset(),
		crs = crs()
	)

	invisible(session)

}
