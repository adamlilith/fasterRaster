#' Initialize \code{GRASS} session and import raster and/or vector(s)
#'
#' Initiate a \code{GRASS} session and import a raster and/or vector into it.
#'
#' @inheritParams .sharedArgs_grassDir
#'
#' @param rast Either: a \code{SpatRaster} object \emph{or} the name of a raster already imported into \code{GRASS} \emph{or} \code{NULL} (default) in which case no raster is exported into \pkg{GRASS}. Either \code{rast} or \code{vect} (or both) must be non-\code{NULL}. You cannot set one equal to a name and the other to a raster/vector.
#'
#' @param vect Either: a \code{Spatvector} or \code{sf} object \emph{or} the name of a vector dataset already imported into \code{GRASS} \emph{or} \code{NULL} (default) in which case no vector is exported into \pkg{GRASS}. Either \code{rast} or \code{vect} (or both) must be non-\code{NULL}. You cannot set one equal to a name and the other to a raster/vector.
#'
#' @param rastName Character. Name of the raster when exported to \pkg{GRASS}. Default is \code{'rast'}.
#' @param vectName Character. Name of the vector when exported to \pkg{GRASS}. Default is \code{'vect'}.
#'
#' @param mapset Character. Mapset of \code{GRASS} location. The default name is \code{'PERMANENT'}. Typically, it is not a good idea to change this, as most functions in the \pkg{fasterRaster} package assume the mapset is named "default".
#'
#' @param location Character. Name of \code{GRASS} location. The default name is \code{'default'}. Typically, it is not a good idea to change this, as most functions in the \pkg{fasterRaster} package assume the location is named "default".
#'
#' @param tempDir Character. Path name of directory in which to create the \code{GRASS} mapset. The default is to create a directory in the location for temporary files for the operating system.
#'
#' @return One or two-element character vector. If one element long, then it will be the name of raster \emph{or} vector exported or already in a \code{GRASS} session. If two elements long, then it will be the name of the raster \emph{and} the vector exported or already in a \code{GRASS} session.
#'
#' @examples man/examples/ex_initGrass.r
#'
#' @export

initGrass <- function(
	rast = NULL,
	vect = NULL,
	rastName = 'rast',
	vectName = 'vect',
	mapset = 'PERMANENT',
	location = 'default',
	tempDir = tempfile(),
	grassDir = options()$grassDir
) {

	# just in case a \code{GRASS} session is already running
	rgrass::unset.GIS_LOCK()
	rgrass::remove_GISRC()
	rgrass::unlink_.gislock()

	# NULL and NULL
	if (is.null(rast) & is.null(vect)) {
	
		stop('Arguments "rast" and "vect" cannot be NULL simultaneously.')
	
	# NAME and NAME
	} else if (inherits(rast, 'character') & inherits(vect, 'character')) {
	
		input <- c(rast, vect)
		names(input) <- c('rastNameInGrass', 'vectNameInGrass')
		
	# NAME and NULL
	} else if (inherits(rast, 'character') & is.null(vect)) {
	
		input <- rast
		names(input) <- c('rastNameInGrass')
		
	# NULL and NAME
	} else if (is.null(rast) & inherits(vect, 'character')) {
	
		input <- vect
		names(input) <- c('vectNameInGrass')
	
	# RASTER and/or VECTOR
	} else {
		
		# RASTER and NULL
		if (inherits(rast, c('SpatRaster', 'Raster')) & is.null(vect)) {
			
			input <- rastName
			names(input) <- 'rastNameInGrass'
			
		# NULL and VECTOR
		} else if (is.null(rast) & inherits(vect, c('SpatVector', 'sf', 'Spatial'))) {
			
			input <- vectName
			names(input) <- 'vectNameInGrass'

		# RASTER and VECTOR
		} else if (inherits(rast, c('SpatRaster', 'Raster')) & inherits(vect, c('SpatVector', 'sf', 'Spatial'))) {
			
			input <- c(rastName, vectName)
			names(input) <- c('rastNameInGrass', 'vectNameInGrass')

		} else {
		
			stop('Argument "rast" must be:\n* NULL;\n* the name of a raster already in a GRASS session; or\n* a SpatRaster.\nArgument "vect" must be:\n* NULL;\n* the name of a vector already in a GRASS session; or\n* a SpatVector or sf object.\nBoth "rast" and "vect" cannot be NULL simultaneously.\nOne cannot be a name and the other a raster/vector.')

		}
		
		.makeGrassEnv(rast=rast, vect=vect, rastName=rastName, vectName=vectName, grassDir=grassDir, location=location, mapset=mapset, tempDir=tempDir)
		
	} # if passing a raster and/or vector
		
	attr(input, 'mapset') <- mapset
	attr(input, 'location') <- location
	attr(input, 'tempDir') <- tempDir
	input
	
}


# make \code{GRASS} session		
.makeGrassEnv <- function(
	rast,			# raster or NULL
	vect,			# vector or NULL
	rastName,		# character or ignored
	vectName,		# character or ignored
	grassDir,		# \code{GRASS} install directory
	location,		# name of location
	mapset,			# name of mapset
	tempDir			# temporary directory
) {

	dir.create(tempDir, recursive=TRUE, showWarnings=FALSE)

	# setup session
	rgrass::initGRASS(gisBase=grassDir, home=tempDir, location=location, mapset=mapset, SG=rast, override=TRUE, remove_GISRC=TRUE, ignore.stderr=TRUE, pid=1)

	# define parameters of this \code{GRASS} session
	if (!is.null(rast)) {

		if (inherits(rast, 'Raster')) rast <- terra::rast(rast)
		resolution <- as.character(terra::res(rast)[1])
		
		ext <- terra::ext(rast)
		xmin <- as.character(ext@ptr$vector[1L])
		xmax <- as.character(ext@ptr$vector[2L])
		ymin <- as.character(ext@ptr$vector[3L])
		ymax <- as.character(ext@ptr$vector[4L])
	
		proj <- terra::crs(rast)
		sink(paste0(tempDir, '/wkt.asc'))
		cat(proj)
		sink()

		rgrass::execGRASS('g.proj', flags=c('c','quiet'), wkt=paste0(tempDir, '/wkt.asc'))
		rgrass::execGRASS('g.region', flags=c('quiet'), n=ymax, s=ymin, e=xmax, w=xmin, res=resolution)

	# note: if no raster, MUST have a vector
	} else {

		if (!inherits(vect, 'SpatVector')) vect <- terra::vect(vect)

		proj <- terra::crs(vect)
		sink(paste0(tempDir, '/wkt.asc'))
		cat(proj)
		sink()
		
		ext <- terra::ext(vect)
		xmin <- as.character(ext@ptr$vector[1L])
		xmax <- as.character(ext@ptr$vector[2L])
		ymin <- as.character(ext@ptr$vector[3L])
		ymax <- as.character(ext@ptr$vector[4L])
	
		rgrass::execGRASS('g.proj', flags=c('c','quiet'), wkt=paste0(tempDir, '/wkt.asc'))
		rgrass::execGRASS('g.region', flags=c('quiet'), n=ymax, s=ymin, e=xmax, w=xmin)
		
	}

	# export
	if (!is.null(rast)) exportRastToGrass(rast, grassName=rastName)
	if (!is.null(vect))	exportVectToGrass(vect, grassName=vectName)
	
	invisible(TRUE)
	
}
