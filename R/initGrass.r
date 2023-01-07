#' Initialize \code{GRASS} session and import raster and/or vector(s)
#'
#' Initiate a \code{GRASS} session and import a raster and/or vector into it.
#'
#' @inheritParams .sharedArgs_inRastName_plural
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_grassDir
#'
#' @param rast Either: a \code{SpatRaster} object with one or more layers \emph{or} the name of a raster already imported into \code{GRASS} \emph{or} \code{NULL} (default) in which case no raster is exported into \pkg{GRASS}. Either \code{rast} or \code{vect} (or both) must be non-\code{NULL}. You cannot set one equal to a name and the other to a raster/vector.
#'
#' @param vect Either: a \code{SpatVector} or \code{sf} object \emph{or} the name of a vector dataset already imported into \code{GRASS} \emph{or} \code{NULL} (default) in which case no vector is exported into \pkg{GRASS}. Either \code{rast} or \code{vect} (or both) must be non-\code{NULL}. You cannot set one equal to a name and the other to a raster/vector.
#'
#' @param restartGrass If \code{TRUE}, then remove any existing \code{GRASS} session and restart a new one. This deletes all rasters and vectors that may already exist in \code{GRASS}, but does not affect anything in R. By default, this is \code{FALSE}, so if there is an existing session, it is used instead.
#'
#' @param warm If \code{TRUE}, then print a warning if the \code{GRASS} session has been restarted. This is only used if \code{restartGrass} is \code{TRUE}.
#'
#' @param mapset Character. Mapset of \code{GRASS} location. The default name is \code{'PERMANENT'}. Typically, it is not a good idea to change this, as most functions in the \pkg{fasterRaster} package assume the mapset is named "default".
#'
#' @param location Character. Name of \code{GRASS} location. The default name is \code{'default'}. Typically, it is not a good idea to change this, as most functions in the \pkg{fasterRaster} package assume the location is named "default".
#'
#' @param tempDir Character. Path name of directory in which to create the \code{GRASS} mapset. The default is to create a directory in the location for temporary files for the operating system.
#'
#' @return One or two-element character vector. If one element long, then it will be the name of raster \emph{or} vector exported or already in a \code{GRASS} session. If two elements long, then it will be the name of the raster \emph{and} the vector exported or already in a \code{GRASS} session.
#'
#' @seealso \code{\link{exportRastToGrass}} and \code{\link{exportVectToGrass}} in \pkg{fasterRaster}
#'
#' @example man/examples/ex_initGrass.r
#'
#' @export

initGrass <- function(
	rast = NULL,
	vect = NULL,
	inRastName = NULL,
	inVectName = NULL,
	restartGrass = FALSE,
	warn = TRUE,
	mapset = 'PERMANENT',
	location = 'default',
	tempDir = tempdir(),
	grassDir = options()$grassDir
) {

	# for debugging
	if (FALSE) {
	
		mapset <- 'PERMANENT'
		location <- 'default'
		inRastName <- NULL
		inVectName <- NULL
		tempDir <- tempdir()
	
	}

	inRastName <- .getInRastName(inRastName, rast)
	if (!is.null(vect) & is.null(inVectName)) inVectName <- 'vect'

	### setup
	#########

	# NULL and NULL
	if (is.null(rast) & is.null(vect)) {
	
		stop('Arguments "rast" and "vect" cannot be NULL simultaneously.')
	
	# NAME and NAME
	} else if (inherits(rast, 'character') & inherits(vect, 'character')) {
	
		input <- c(rast, vect)
		nr <- length(rast)
		names(input) <- c(rep('rastNameInGrass', nr), 'vectNameInGrass')
		
	# NAME and NULL
	} else if (inherits(rast, 'character') & is.null(vect)) {
	
		input <- rast
		nr <- length(rast)
		names(input) <- rep('rastNameInGrass', nr)
		
	# NULL and NAME
	} else if (is.null(rast) & inherits(vect, 'character')) {
	
		input <- vect
		names(input) <- c('vectNameInGrass')
	
	# RASTER and/or VECTOR
	} else {
		
		# RASTER and NULL
		if (inherits(rast, c('SpatRaster', 'Raster')) & is.null(vect)) {
			
			nr <- terra::nlyr(rast)
			input <- inRastName
			names(input) <- rep('rastNameInGrass', nr)
			
		# NULL and VECTOR
		} else if (is.null(rast) & inherits(vect, c('SpatVector', 'sf', 'Spatial'))) {
			
			input <- inVectName
			names(input) <- 'vectNameInGrass'

		# RASTER and VECTOR
		} else if (inherits(rast, c('SpatRaster', 'Raster')) & inherits(vect, c('SpatVector', 'sf', 'Spatial'))) {
			
			nr <- terra::nlyr(rast)
			input <- c(inRastName, inVectName)
			names(input) <- c(rep('rastNameInGrass', nr), 'vectNameInGrass')

		} else {
		
			stop('Argument "rast" must be:\n* NULL;\n* the name of a raster already in a GRASS session; or\n* a SpatRaster.\nArgument "vect" must be:\n* NULL;\n* the name of a vector already in a GRASS session; or\n* a SpatVector or sf object.\nBoth "rast" and "vect" cannot be NULL simultaneously.\nOne cannot be a name and the other a raster/vector.')

		}

		### refresh any existing GRASS session
		if (restartGrass) {
		
			rgrass::unset.GIS_LOCK()
			rgrass::remove_GISRC()
			rgrass::unlink_.gislock()

			files <- list.files(tempDir, include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
			files <- rev(files)
			unlink(files, recursive = TRUE)
			if (warn) warning('The GRASS session has been restarted.\nAll previously existing files have been removed.')
			
		}

		### initialize session
		dir.create(tempDir, recursive=TRUE, showWarnings=FALSE)

		# rgrass::initGRASS(gisBase=grassDir, home=tempDir, location=location, mapset=mapset, SG=rast, override=TRUE, remove_GISRC=TRUE, ignore.stderr=TRUE, pid=sample(10000, 1))
		session <- suppressMessages(rgrass::initGRASS(gisBase=grassDir, home=tempDir, location=location, mapset=mapset, override=TRUE, remove_GISRC=TRUE, ignore.stderr=TRUE, pid=sample(1000, 1)))

		# define parameters of this GRASS session
		if (!is.null(rast)) {

			if (!inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
			ewRes <- as.character(terra::res(rast)[1L])
			nsRes <- as.character(terra::res(rast)[2L])
			
			ext <- terra::ext(rast)
			xmin <- as.character(ext@ptr$vector[1L])
			xmax <- as.character(ext@ptr$vector[2L])
			ymin <- as.character(ext@ptr$vector[3L])
			ymax <- as.character(ext@ptr$vector[4L])
		
			proj <- terra::crs(rast)
			sink(paste0(tempDir, '/wkt.txt'))
			cat(proj)
			sink()

			rgrass::execGRASS('g.proj', flags=c('c','quiet'), wkt=paste0(tempDir, '/wkt.txt'))
			rgrass::execGRASS('g.region', flags=c('quiet'), n=ymax, s=ymin, e=xmax, w=xmin, nsres=nsRes, ewres=ewRes)

		}
		
		if (!is.null(vect)) {

			if (!inherits(vect, 'SpatVector')) vect <- terra::vect(vect)

			proj <- terra::crs(vect)
			sink(paste0(tempDir, '/wkt.txt'))
			cat(proj)
			sink()
			
			ext <- terra::ext(vect)
			xmin <- as.character(ext@ptr$vector[1L])
			xmax <- as.character(ext@ptr$vector[2L])
			ymin <- as.character(ext@ptr$vector[3L])
			ymax <- as.character(ext@ptr$vector[4L])
		
			rgrass::execGRASS('g.proj', flags=c('c','quiet'), wkt=paste0(tempDir, '/wkt.txt'))
			rgrass::execGRASS('g.region', flags=c('quiet'), n=ymax, s=ymin, e=xmax, w=xmin)
			
		}

		# export
		if (!is.null(rast)) exportRastToGrass(rast, inRastName=inRastName)
		if (!is.null(vect)) exportVectToGrass(vect, inVectName=inVectName)
		
	} # if passing a raster and/or vector
		
	attr(input, 'mapset') <- mapset
	attr(input, 'location') <- location
	attr(input, 'tempDir') <- tempDir
	attr(input, 'session') <- if (exists('session', inherits = FALSE)) { session } else { NA }
	invisible(input)
	
}
