#' Initialize GRASS session and import raster and/or vector(s).
#'
#' This is a private function to start a generic GRASS session and import a raster and/or vector into it.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast} or the vector named in \code{vect}. If \code{FALSE}, use a raster or vector already in GRASS with the name given by \code{rast} or \code{vect}. The latter case is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param rast Either: a raster object **or** the name of a raster already imported into GRASS **or** \code{NULL} (default) in which case no raster is exported into GRASS. Either \code{rast} or \code{vect} (or both) but be non-\code{NULL}.
#' @param rast Either: a SpatialPoints, SpatialPolygons, or SpatialLines object **or** the name of a vector dataset already imported into GRASS **or** \code{NULL} (default) in which case no vector is exported into GRASS. Either \code{rast} or \code{vect} (or both) but be non-\code{NULL}.
#' @param grassDir Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @return One or two-element character list. If the list is one element long then it will be the name of raster **or** vector exported or already in GRASS session. If it is two elements long then it will be the name of the raster **and* the vector exported or already in the GRASS session (in that order).
#' @keywords internal
.initGrass <- function(
	alreadyInGrass = FALSE,
	rast = NULL,
	vect = NULL,
	grassDir = NULL
) {

	rgrass7::use_sp()

	if (is.null(rast) & is.null(vect)) stop('A raster or a vector already in GRASS must be specified in private function ".initGrass".')

	# if vector/raster is already in GRASS then just return its name in GRASS
	if (alreadyInGrass) {
		
		if (!is.null(rast) & !is.null(vect)) {
			input <- rast
			names(input) <- 'rastNameInGrass'
		} else if (is.null(rast) & !is.null(vect)) {
			input <- vect
			names(input) <- 'vectNameInGrass'
		} else {
			input <- c(rast, vect)
			names(input) <- c('rastNameInGrass', 'vectNameInGrass')
		}
		
	# if vector/raster is NOT already in GRASS then export it to GRASS and return its name
	} else {

		tempDir <- tempfile()
		dir.create(tempDir, recursive=TRUE, showWarnings=FALSE)

		rgrass7::initGRASS(gisBase=grassDir, home=tempDir, mapset='PERMANENT', override=TRUE)
		
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

			proj4 <- vecter::projection(vect)
			
			ext <- vecter::extent(vect)
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
	
			input <- c(input, 'rast')
			names(input)[length(input)] <- 'rastNameInGrass'
			exportRastToGrass(rast, vname='rast', tempDir=tempDir)
			
		}
		
		# export vector
		if (!is.null(vect)) {
		
			input <- c(input, 'vect')
			names(input)[length(input)] <- 'vectNameInGrass'
			exportVectToGrass(vect, vname='vect')

		}
		
	}
	
	input
	
}
