#' Initialize GRASS session and import raster and/or vector(s)
#'
#' Initiate a GRASS session and import a raster and/or vector into it.
#'
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast} or the vector named in \code{vect}. If \code{FALSE}, use a raster or vector already in GRASS with the name given by \code{rast} or \code{vect}. The latter case is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#'
#' @param rast Either: a \code{SpatRaster} object \emph{or} the name of a raster already imported into GRASS \emph{or} \code{NULL} (default) in which case no raster is exported into GRASS. Either \code{rast} or \code{vect} (or both) must be non-\code{NULL}.
#'
#' @param vect Either: a \code{Spatvector} or \code{sf} object \emph{or} the name of a vector dataset already imported into GRASS \emph{or} \code{NULL} (default) in which case no vector is exported into GRASS. Either \code{rast} or \code{vect} (or both) must be non-\code{NULL}.
#'
#' @param rastName Character. Name of the raster when exported to GRASS. Default is \code{'rast'}.
#'
#' @param vectName Character. Name of the vector when exported to GRASS. Default is \code{'vect'}.
#'
#' @param mapset Character. Mapset of GRASS location. The default name is \code{'PERMANENT'}. Typically, it is not a good idea to change this, as most functions in the \pkg{fasterRaster} package assume the mapset is named "default".
#'
#' @param location Character. Name of GRASS location. The default name is \code{'default'}. Typically, it is not a good idea to change this, as most functions in the \pkg{fasterRaster} package assume the location is named "default".
#'
#' @param tempDir Character. Path name of directory in which to create the GRASS mapset. The default is to create a directory in the location for temporary files for the operating system.
#'
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example for a Windows system: \code{'C:/Program Files/GRASS GIS 8.2'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#'
#' @return One or two-element character vector. If the vector is one element long then it will be the name of raster \emph{or} vector exported or already in GRASS session. If it is two elements long then it will be the name of the raster \emph{and} the vector exported or already in the GRASS session (in that order).
#'
#' @examples
#'
#' \donttest{
#' # change this to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 8.2'
#'
#' data(madElev)
#' input <- initGrass(rast=madElev, grassDir=grassDir)
#' data(madCoast0)
#' input <- initGrass(vect=madCoast0, grassDir=grassDir)
#' }
#'
#' @export
initGrass <- function(
	rast = NULL,
	vect = NULL,
	alreadyInGrass = FALSE,
	rastName = 'rast',
	vectName = 'vect',
	mapset = 'PERMANENT',
	location = 'default',
	tempDir = tempfile(),
	grassDir = options('grassDir')
) {

	if (is.null(rast) & is.null(vect)) stop('A raster and/or a vector already in GRASS must be specified in function initGrass.')

	# just in case a GRASS session is already running
	rgrass::unset.GIS_LOCK()
	rgrass::remove_GISRC()
	rgrass::unlink_.gislock()

	# if vector/raster is already in GRASS then just return its name in GRASS
	if (alreadyInGrass) {
		
		if (!is.null(rast) & !is.null(vect)) {
			input <- c(rast, vect)
			names(input) <- c('rastNameInGrass', 'vectNameInGrass')
		} else if (is.null(rast) & !is.null(vect)) {
			input <- vect
			names(input) <- 'vectNameInGrass'
		} else if (!is.null(rast) & is.null(vect)) {
			input <- rast
			names(input) <- 'rastNameInGrass'
		}
		
	# if vector/raster is NOT already in GRASS then export it to GRASS and return its name
	} else {

		# rgrass::use_sp()
		dir.create(tempDir, recursive=TRUE, showWarnings=FALSE)

		rgrass::initGRASS(gisBase=grassDir, home=tempDir, location=location, mapset=mapset, SG=rast, override=TRUE, remove_GISRC=TRUE, ignore.stderr=TRUE, pid=1)
		
		if (!is.null(rast)) {
	
			resolution <- as.character(raster::res(rast)[1])
			
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

		} else {

			if (inherits(vect, 'sf')) vect <- terra::vect(vect)

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
	
		input <- character()
	
		# export raster
		if (!is.null(rast)) {
	
			input <- c(input, rastName)
			names(input)[length(input)] <- 'rastNameInGrass'
			exportRastToGrass(rast, grassName=rastName, tempDir=tempDir)
			
		}
		
		# export vector
		if (!is.null(vect)) {
		
			input <- c(input, vectName)
			names(input)[length(input)] <- 'vectNameInGrass'
			exportVectToGrass(vect, grassName=vectName)

		}
		
	}
	
	attr(input, 'mapset') <- mapset
	attr(input, 'location') <- location
	attr(input, 'tempDir') <- tempDir
	input
	
}
