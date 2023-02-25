#' Initialize a 'GRASS' session
#'
#' This function initializes a **GRASS** session in a particulatr folder. You need to run this function before you use most functions in **fasterRaster** (just once). You can also use the function to switch your **GRASS** session to a different folder, even if one is already started.
#'
#' @param grassDir Character: Folder in which **GRASS** is installed on your computer. This will look different depending on the operating system and verison of **GRASS** you have installed. Here are some examples:
#' * Windows: `'C:/Program Files/GRASS GIS 8.3'`
#' * Mac OS: `"/Applications/GRASS-8.3.app/Contents/Resources"`
#' * Linux: `'/usr/local/grass'`
#'
#' @param crs Any object from which a coordinate reference system (CRS) can be acquired. Ergo, any of:
#' * A `SpatRaster`, `SpatVector`, `SpatExtent`, `stars` or `sf` object
#' * A CRS (coordinate reference system) string
#'
#' @param workDir `NULL` or character: The name of the folder in which **GRASS** will store rasters and vectors. If this is `NULL` (default), then the [tempdir()] on the user's system will be used. If users wish to create persistent **GRASS** sesssions that can be used in a different instance of **R** (i.e., if **R** is stopped then restarted), then this needs to be specified.
#'
#' @param ... Options to send to [setFastOptions()]. These should be in `option = value` format.
#'
#' @return `TRUE` if successful (invisibly). If not successful, the function will usually either fail or return `FALSE` with a warning.
#'
#' @seealso Guide to getting [started](tutorial_starting) with **fasterRaster**.
#'
#' @example man/examples/ex_startFast.r
#'
#' @export

startFast <- function(
	grassDir,
	crs,
	workDir = NULL,
	...
) {

	# for debugging
	if (FALSE) {

		grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
		dots <- list()
		workDir <- NULL
		crs <- madRivers
	
	}

	### function globals
	dots <- list(...)
	if (is.null(workDir)) workDir <- rightSlash(tempdir())
	dir.create(workDir, showWarnings=FALSE, recursive=TRUE)

	### CRS
	crs <- if (inherits(crs, c('SpatRaster', 'SpatVector', 'SpatExtent'))) {
		terra::crs(crs)
	} else if (inherits(crs, 'sf')) {
		sf::st_crs(crs)
	} else if (inherits(crs, 'stars')) {
		stars::st_crs(crs)
	} # else, we assume crs is a string

	### do we need a new GRASS session or to swicth the location/working directory?

	mapset <- .getHiddenOptions('mapset', default=TRUE)
	location <- .getHiddenOptions('location', default=TRUE)
	currentWorkDir <- .getHiddenOptions('workDir', default=TRUE)

	# # have we started yet?
	# if (!.isGrassStarted()) {
		# start <- TRUE
		
	# # is working directory the same?
	# } else {
		
		FRfile <- file.path(workDir, location, mapset, '.fasterRaster.rds')
		
		# working directory has .fasterRaster file
		if (file.exists(FRfile)) {
			existingFR <- readRDS(FRfile)
			if (get('location', existingFR) != location) {
				# different location
				start <- TRUE
			} else if (get('crs', existingFR) != crs) {
				# different CRS
				start <- FALSE
			} else {
				# same CRS
				start <- TRUE
			}
		} else {
			# no existing .fasterRaster file in proposed workinng directory
			start <- TRUE
		}
	# different working directory
	# }
			
	### start new GRASS session
	# if (!started | newLoc | newWorkDir) {
	if (start) {

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
				remove_GISRC = TRUE, # ???
				ignore.stderr = TRUE
			)
		)
		
		### create environment and set options
		# .fasterRaster <<- new.env()
		# opts <- .namesOfOptions()
		
		# setFastOptions(restore=TRUE)
		setFastOptions(grassDir=grassDir)
		if (length(dots) > 0L) setFastOptions(...)
		.setHiddenOptions(workDir=workDir)
		.setHiddenOptions(crs=crs)
		
		# .grassIsStarted()

		# local({

			# locMapCrs <- GLocation(
				# location = location,	# GRASS location
				# mapset = mapset,		# GRASS MAPSET
				# crs = crs				# coordinate reference system
			# )
			
			# }, envir=.fasterRaster
		# )
		
		envSave <- get('.fasterRaster', globalenv())
		FRfile <- file.path(workDir, location, mapset, '.fasterRaster.rds')
		saveRDS(envSave, file=FRfile)
		
	### if already started GRASS
	} else {
	
		warning('GRASS has already been initialized. If you want to change options\n  in the existing session, use setFastOptions(). No action taken.')
		locMapCrs <- FALSE
		
	}
	
	invisible(locMapCrs)

}
