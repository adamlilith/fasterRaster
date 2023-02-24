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
#' @param workDir Character: The name of the folder in which **GRASS** will store rasters and vectors. Nearly all users will want to use the [tempdir()] on their system (the default), but this is provided here as an option for cases where a putting these files on a seperate drive may be useful.
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
	workDir = rightSlash(tempdir()),
	...
) {

	# for debugging
	if (FALSE) {

		grassDir <- 'C:/Program Files/GRASS GIS 8.2' # Windows
		dots <- list()
	
	}

	### function globals
	dots <- list(...)

	started <- .isGrassStarted()
	
	### start new GRASS session
	if (!started) {

		mapset <- .getHiddenOptions('mapset', default=TRUE)
		location <- .getHiddenOptions('location', default=TRUE)

		if (inherits(crs, c('SpatRaster', 'SpatVector', 'SpatExtent'))) {
			crs <- terra::crs(crs)
		} else if (inherits(crs, 'sf')) {
			crs <- sf::st_crs(crs)
		} else if (inherits(crs, 'stars')) {
			crs <- stars::st_crs(crs)
		}
		emptyRast <- terra::rast(matrix(1), type='xy', crs=crs)

		### start the GRASS session
		suppressWarnings(
			session <- rgrass::initGRASS(
				gisBase = grassDir,
				home = workDir,
				SG = emptyRast,
				location = location,
				mapset = mapset,
				override = TRUE,
				remove_GISRC = TRUE, # ???
				ignore.stderr = TRUE
			)
		)
		
		### create environment and set options
		.fasterRaster <<- new.env()
		opts <- .namesOfOptions()
		
		setFastOptions(restore=TRUE)
		setFastOptions(grassDir=grassDir)
		if (length(dots) > 0L) setFastOptions(...)
		.setHiddenOptions(workDir=workDir)
		
		.grassIsStarted()
		
		local({
		
			locTable <- data.frame(
				location = location,
				mapset = mapset,
				crs = crs
			)
		
		}, envir=.fasterRaster)
		
		# ### environment variables
		# local({
			
			# grassStarted <- TRUE

			# # crosswalk table
			# # holds name in R and in GRASS and other metadata
			# # GRASS name is usually hidden to user
			# xtable <- data.frame(matrix(nrow=0, ncol=20))
			# names(xtable) <- c(
				# 'type',
				# 'source'
				# 'rname',
				# 'gname',
				# 'topology',
				# 'west',
				# 'east',
				# 'south',
				# 'north',
				# 'bottom',
				# 'top',
				# 'rows',
				# 'cols',
				# 'ewres',
				# 'nsres',
				# 'grassDatatype',
				# 'terraDatatype',
				# 'gdalDatatype',
				# 'numCategories',
				# 'minVal',
				# 'maxVal'
			# )
		
		# }, envir=.fasterRaster)
		
		out <- GLocation(
			location = location,
			mapset = mapset,
			crs = crs
		)
		
	### if already started GRASS
	} else {
	
		warning('GRASS has already been initialized. If you want to change options\n  in the existing session, use setFastOptions(). No action taken.')
		out <- FALSE
		
	}
	
	invisible(out)

}
