#' Initialize 'GRASS' session and import raster and/or vector(s)
#'
#' This function generally initializes or re-initializes a \code{GRASS} session. Specifically, it can:
#' \itemize{
#' 	\item	Initiate a new \code{GRASS} session and import a raster and/or vector into it;
#'	\item 	Switch to another, pre-existing \code{GRASS} \link{location} started with the same session of \code{R} by changing the \code{location} argument; or
#' 	\item	Reset an existing\code{GRASS} session by removing all existing files, and import a raster and/or vector into it by setting argument \code{restartGrass} to \code{TRUE};
#'	\item   Initiate a pre-existing \code{GRASS} session saved to disk by supplying a folder path with \code{dir} and possibly the \code{location}.
#' }
#'
#' @inheritParams .sharedArgs_inRastName_multiple
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_autoRegion
#' @inheritParams .sharedArgs_grassDir
#'
#' @param rast Either: a \code{SpatRaster} object with one or more layers \emph{or} the name of a raster already imported into \code{GRASS} \emph{or} \code{NULL} (default) in which case no raster is exported into \code{GRASS}. Typically, either \code{rast} and/or \code{vect} are specified. However, you can set both to \code{NULL} to switch to different \code{GRASS} \link{location}s without importing anything into the session. You cannot set one equal to a name and the other to a raster/vector.
#'
#' @param vect Either: a \code{SpatVector}, or \code{sf} object, \emph{or} the name of a vector dataset already imported into \code{GRASS}, \emph{or} \code{NULL} (default) in which case no vector is exported into \code{GRASS}. Either \code{rast} or \code{vect} (or both) must be non-\code{NULL}. You cannot set one equal to a name and the other to a raster/vector.
#'
#' @param restartGrass If \code{TRUE}, then remove any existing \code{GRASS} session and restart a new one. This deletes all rasters and vectors that may already exist in \code{GRASS}, but does not affect anything in R. By default, this is \code{FALSE}, so if there is an existing session, it is used instead.
#'
#' @param warn If \code{TRUE}, then print a warning if the \code{GRASS} session has been restarted. This is only used if \code{restartGrass} is \code{TRUE}.
#'
#' @param location Character. Name of \code{GRASS} location. The default name is \code{'default'}. You can switch between locations by using \code{startFaster(location='newLocation')}.
#'
#' @param dir Character. Path name of directory in which to create the \code{GRASS} mapset. The default is to create a directory in the location for temporary files for the operating system.
#'
#' @param ... Additional arguments (unused). Useful for passing arguments in from other functions when they call this one.
#'
#' @return One or two-element character vector. If one element long, then it will be the name of raster \emph{or} vector exported or already in a \code{GRASS} session. If two elements long, then it will be the name of the raster \emph{and} the vector exported or already in a \code{GRASS} session.
#'
#' @seealso \code{\link{fasterRast}} and \code{\link{fasterVect}} in \pkg{fasterRaster}
#'
#' @example man/examples/ex_initGrass.r
#'
#' @export

startFaster <- function(
	rast,
	vect,
	inRastName,
	inVectName,
	restartGrass = FALSE,
	warn = TRUE,
	dir = tempdir(),
	location = 'default',
	# mapset = 'PERMANENT',
	
	replace = fasterGetOptions('replace', FALSE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### function globals
	mapset <- 'PERMANENT'

	# for debugging
	if (FALSE) {
	
		location <- 'default'
		mapset <- 'PERMANENT'
		inRastName <- NULL
		inVectName <- NULL
		replace <- TRUE
		restartGrass <- FALSE
		warn <- TRUE
		dir <- tempdir()
	
	}

	### commons
	###########

		### arguments
		if (missing(rast)) {
			rast <- inRastName <- NULL
		} else {
			inRastName <- .getInRastName(inRastName, rast=rast)
		}

		if (missing(vect)) {
			vect <- inVectName <- NULL
		} else {
			if (missing(inVectName)) inVectName <- NULL
			inVectName <- .getInVectName(inVectName, vect=vect)
		}

		### flags
		flags <- .getFlags(replace=replace)

		# ### restore
		# # on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

	###############
	### end commons

	### setup
	#########

	# NULL and NULL
	if (is.null(rast) & is.null(vect)) {

		# GRASSstarted--just change location
		if (.getSessionStarted()) {

			if (!file.exists(file.path(dir, location))) stop(paste0('Location ', location, ' does not exist.'))
			suppressMessages(rgrass::execGRASS('g.mapset', mapset=mapset, location=location, flags=flags, intern=TRUE))

			.setSession(dir=dir, location=location, mapset=mapset)
			input <- character()
	
		# start empty session
		} else {
			stop('To start a GRASS session, you need to import a raster and/or vector.')
		}

	# NAME and NAME
	} else if (inherits(rast, 'character') & inherits(vect, 'character')) {
	
		input <- c(rast, vect)
		nr <- length(rast)
		names(input) <- c(rep('raster', nr), 'vector')
		
	# NAME and NULL
	} else if (inherits(rast, 'character') & is.null(vect)) {
	
		input <- rast
		nr <- length(rast)
		names(input) <- rep('raster', nr)
		
	# NULL and NAME
	} else if (is.null(rast) & inherits(vect, 'character')) {
	
		input <- vect
		names(input) <- c('vector')
	
	# RASTER and/or VECTOR
	} else {

		# RASTER and NULL
		if (inherits(rast, c('SpatRaster', 'Raster')) & is.null(vect)) {

			if (!inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
			nr <- terra::nlyr(rast)
			input <- inRastName
			names(input) <- rep('raster', nr)

		# NULL and VECTOR
		} else if (is.null(rast) & inherits(vect, c('SpatVector', 'sf', 'Spatial'))) {
			
			input <- inVectName
			names(input) <- 'vector'

		# RASTER and VECTOR
		} else if (inherits(rast, c('SpatRaster', 'Raster')) & inherits(vect, c('SpatVector', 'sf', 'Spatial'))) {
			
			if (!inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
			nr <- terra::nlyr(rast)
			input <- c(inRastName, inVectName)
			names(input) <- c(rep('raster', nr), 'vector')

		} else {
		
			stop('Argument "rast" must be:\n* NULL;\n* the name of a raster already in a GRASS session; or\n* a SpatRaster.\nArgument "vect" must be:\n* NULL;\n* the name of a vector already in a GRASS session; or\n* a SpatVector or sf object.\nOne cannot be a name and the other a raster/vector.')

		}

		### refresh any existing GRASS session
		if (restartGrass && file.exists(file.path(dir, location))) {
		
			.setSessionStarted(FALSE)

			if (warn) warning(paste0('The GRASS session with these properties has been restarted:\n  * location: ', location, '\n  * directory: ', dir, '.\n  All previously existing files have been removed.'), immediate.=TRUE)
			
			rgrass::unset.GIS_LOCK()
			rgrass::remove_GISRC()
			rgrass::unlink_.gislock()

			files <- list.files(file.path(dir, location), include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
			files <- rev(files)
			unlink(files, recursive = TRUE)
			unlink(file.path(dir, location), recursive = TRUE)
			
		}

		### initialize session
		dir.create(dir, recursive=TRUE, showWarnings=FALSE)

		# session not started yet
		if (!.getSessionStarted() || restartGrass || is.na(.getLocation()) || location != .getLocation()) {

			SG <- if (!is.null(rast)) {
				rast
			} else if (!is.null(vect)) {
				terra::rast(matrix(1), type='xy', crs=terra::crs(vect), extent=terra::ext(vect))
			} else {
				NULL
			}

			suppressWarnings(
				session <- rgrass::initGRASS(
					gisBase = grassDir,
					home = dir,
					SG = SG,
					location = location,
					mapset = mapset,
					override = TRUE,
					remove_GISRC = restartGrass,
					ignore.stderr = TRUE,
					pid = as.integer(sample(1000, 1))
				)
			)

		# using different mapset or location
		} else if (is.na(.getLocation()) || is.na(.getMapset()) || mapset != .getMapset()) {

			success <- rgrass::execGRASS(
				'g.mapset',
				mapset = mapset,
				location = location,
				flags = 'quiet',
				intern = TRUE,
				ignore.stderr = TRUE
			)

		}

		.setSessionStarted(TRUE)
		.setSession(dir=dir, location=location, mapset=mapset)

		# define parameters of this GRASS session
		if (!is.null(rast)) {

			ext <- terra::ext(rast)
			xmin <- as.character(ext@ptr$vector[1L])
			xmax <- as.character(ext@ptr$vector[2L])
			ymin <- as.character(ext@ptr$vector[3L])
			ymax <- as.character(ext@ptr$vector[4L])
		
			proj <- terra::crs(rast)
			sink(file.path(dir, location, 'wkt.txt'))
			cat(proj)
			sink()

			rows <- terra::nrow(rast)
			cols <- terra::ncol(rast)

			success <- rgrass::execGRASS('g.proj', flags=c('c','quiet'), wkt=file.path(dir, location, 'wkt.txt'), intern=TRUE)
			success <- rgrass::execGRASS('g.region', flags=c('quiet'), n=ymax, s=ymin, e=xmax, w=xmin, rows=rows, cols=cols, intern=TRUE)

		}
		
		if (!is.null(vect)) {

			if (!inherits(vect, 'SpatVector')) vect <- terra::vect(vect)

			proj <- terra::crs(vect)
			sink(file.path(dir, location, 'wkt.txt'))
			cat(proj)
			sink()
			
			# # avoid cropping any rasters to the vector
			# if (is.null(rast)) {
				
				# ext <- terra::ext(vect)
				# xmin <- as.character(ext@ptr$vector[1L])
				# xmax <- as.character(ext@ptr$vector[2L])
				# ymin <- as.character(ext@ptr$vector[3L])
				# ymax <- as.character(ext@ptr$vector[4L])
			
				# success <- rgrass::execGRASS('g.proj', flags=c('c','quiet'), wkt=file.path(dir, location, 'wkt.txt'), intern=TRUE)
				# success <- rgrass::execGRASS('g.region', flags=c('quiet'), n=ymax, s=ymin, e=xmax, w=xmin, intern=TRUE)
				
			# }
			
		}

		# export		
		if (!is.null(rast)) fasterRast(rast=rast, inRastName=inRastName, replace=replace, autoRegion=FALSE)
		if (!is.null(vect)) fasterVect(vect=vect, inVectName=inVectName, replace=replace, autoRegion=FALSE)

	} # if passing a raster and/or vector

	invisible(input)
	
}
