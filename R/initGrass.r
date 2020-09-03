#' Initialize GRASS session and import raster and/or vector(s).
#'
#' Initiate a GRASS session and import a raster and/or vector into it.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast} or the vector named in \code{vect}. If \code{FALSE}, use a raster or vector already in GRASS with the name given by \code{rast} or \code{vect}. The latter case is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param rast Either: a raster object \emph{or} the name of a raster already imported into GRASS \emph{or} \code{NULL} (default) in which case no raster is exported into GRASS. Either \code{rast} or \code{vect} (or both) must be non-\code{NULL}.
#' @param vect Either: a SpatialPoints, SpatialPolygons, or SpatialLines object \emph{or} the name of a vector dataset already imported into GRASS \emph{or} \code{NULL} (default) in which case no vector is exported into GRASS. Either \code{rast} or \code{vect} (or both) must be non-\code{NULL}.
#' @param rastName Character. Name of the raster when exported to GRASS. Default is \code{'rast'}.
#' @param vectName Character. Name of the vector when exported to GRASS. Default is \code{'vect'}.
#' @param mapset Character. Mapset of GRASS location. The default name is \code{'PERMANENT'}. Typically, it is not a good idea to change this, as most functions in the \pkg{fasterRaster} package assume the mapset is named "default".
#' @param location Character. Name of GRASS location. The default name is \code{'default'}. Typically, it is not a good idea to change this, as most functions in the \pkg{fasterRaster} package assume the location is named "default".
#' @param tempDir Character. Path name of directory in which to create the GRASS mapset. The default is to create a directory in the location for temporary files for the operating system.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @return One or two-element character list. If the list is one element long then it will be the name of raster \emph{or} vector exported or already in GRASS session. If it is two elements long then it will be the name of the raster \emph{and} the vector exported or already in the GRASS session (in that order).
#' @examples
#' \donttest{
#' # change this to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8'
#'
#' data(madElev)
#' input <- initGrass(rast=madElev, grassDir=grassDir)
#' data(mad0)
#' input <- initGrass(vect=mad0, grassDir=grassDir)
#' }
#' @export
initGrass <- function(
	alreadyInGrass = FALSE,
	rast = NULL,
	vect = NULL,
	rastName = 'rast',
	vectName = 'vect',
	mapset = 'PERMANENT',
	location = 'default',
	tempDir = tempfile(),
	grassDir = NULL
) {

	rgrass7::use_sp()

	if (is.null(rast) & is.null(vect)) stop('A raster or a vector already in GRASS must be specified in function "initGrass".')

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

		dir.create(tempDir, recursive=TRUE, showWarnings=FALSE)

		rgrass7::initGRASS(gisBase=grassDir, home=tempDir, location=location, mapset=mapset, override=TRUE)
		
		if (!is.null(rast)) {
	
			resolution <- as.character(raster::res(rast)[1])
			proj4 <- raster::projection(rast)
			
			ext <- raster::extent(rast)
			xmax <- as.character(ext@xmax)
			xmin <- as.character(ext@xmin)
			ymax <- as.character(ext@ymax)
			ymin <- as.character(ext@ymin)
		
			rgrass7::execGRASS('g.proj', flags=c('c','quiet'), proj4=proj4)
			rgrass7::execGRASS('g.region', flags=c('quiet'), n=ymax, s=ymin, e=xmax, w=xmin, res=resolution)
	
		} else {

			proj4 <- raster::projection(vect)
			
			ext <- raster::extent(vect)
			xmax <- as.character(ext@xmax)
			xmin <- as.character(ext@xmin)
			ymax <- as.character(ext@ymax)
			ymin <- as.character(ext@ymin)
		
			rgrass7::execGRASS('g.proj', flags=c('c','quiet'), proj4=proj4)
			rgrass7::execGRASS('g.region', flags=c('quiet'), n=ymax, s=ymin, e=xmax, w=xmin)
			
		}
	
		input <- character()
	
		# export raster
		if (!is.null(rast)) {
	
			input <- c(input, rastName)
			names(input)[length(input)] <- 'rastNameInGrass'
			exportRastToGrass(rast, vname=rastName, tempDir=tempDir)
			
		}
		
		# export vector
		if (!is.null(vect)) {
		
			input <- c(input, vectName)
			names(input)[length(input)] <- 'vectNameInGrass'
			exportVectToGrass(vect, vname=vectName)

		}
		
	}
	
	attr(input, 'mapset') <- mapset
	attr(input, 'location') <- location
	attr(input, 'tempDir') <- tempDir
	input
	
}
