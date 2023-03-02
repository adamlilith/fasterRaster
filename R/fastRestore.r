#' Revert to a previous `GRASS` session
#'
#' This function is useful for reverting to a previous **GRASS** session (`workDir`, `location`, and/or `mapset`). The session must have been already initiated using [startFast()].
#'
#' @param ... Either a sequence of arguments with the pattern `<argument = value>`, or a list of arguments. These arguments can be any option available in [setFastOptions()]. Of particular note, the `workDir`, `location`, and `mapset` options allow one to revert to a previous **GRASS** session. The current session's information can be seen using [getFastOptions()].
#'
#' @return An object of class `gmeta` (invisibly) if successful. An error will likely result if not.
#'
#' @seealso **GRASS** [https://grass.osgeo.org/grass82/manuals/grass_database.html](locations and mapsets)
#'
#' @example man/examples/examples_fastStart.r
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

	workDir <- if ('workDir' %in% names(dots)) {
		dots$workDir
	} else {
		getFastOptions('workDir')
	}

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
		stop('This location/mapset does not exist. Use startFast() to create new locations/mapsets.')
	}
	
	### start new GRASS session
	emptyRast <- terra::rast(matrix(1), type='xy', crs=crs)

	### start the GRASS session
	suppressWarnings(
		session <- rgrass::initGRASS(
			gisBase = grassDir,
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

	invisible(session)
	
}

#' Hidden function to restore location/mapset based on a GLocation object
.restore(x) {

	xloc <- fastLocation(x)
	xms <- fastMapset(x)
	
	loc <- getFastOptions('location')
	ms <- getFastOptions('mapset')
	
	if (loc != xloc | ms != xms) session <- fastRestore(location=xloc, mapset=xms)
	invisible(session)
	
}
