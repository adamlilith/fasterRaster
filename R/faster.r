#' Generic function to call a GRASS module
#'
#' This function is wrapper for \code{\link[grass7]{execGRASS}}, plus code necessary to initiate a GRASS session. Many of the functions in \pkg{fasterRaster} actually utilize this function.  This function works best for modules that take one raster and/or one vector as input and produce one raster or vector as output.
#' @param mod Character. Name of GRASS 7 module (e.g., \code{'r.latlong'} or \code{'v.buffer'}.
#' @param rast Either a raster or the name of a raster in an existing GRASS session. Used as input to the module.
#' @param vect Either a SpatialPoints, SpatialPointsDataFrame, SpatialLines, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame or the name of such a vector dataset already in a GRASS session. Used as input to the module.
#' @param rastName Character. Name of the input raster (if any) when exported to GRASS. Default is \code{'rast'}.
#' @param vectName Character. Name of the input vector (if any) when exported to GRASS. Default is \code{'vect'}.
#' @param outType \code{NULL}, \code{'raster'}, or \code{'vector'} (partial string matching is used). Type of output expected from the GRASS module.
#' @param flags List of flags for the GRASS module. The default (\code{c('quiet', 'overwrite')}) causes the module to report little/no messages and to overwrite existing files of the same name. For more flags, see the help documentation for the respective GRASS module.
#' @param ... Arguments to pass to the GRASS module through \code{\link[grass7]{execGRASS}}.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{vectToRast}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @return A raster, vector, or some other product of the GRASS module.
#' @details For a list of vector commands, please see <https://grass.osgeo.org/grass78/manuals/vector.html>. For a list of raster commands, please see <https://grass.osgeo.org/grass78/manuals/raster.html>.
#' @seealso \code{\link[grass7]{execGRASS}}
#' @examples
#' \donttest{
#' # change this to where GRASS 7 is installed on your system:
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8' # example for a PC
#' grassDir <- "/Applications/GRASS-7.8.app/Contents/Resources" # for a Mac
#'
#' data(madForest2000)
#' latRast <- faster('r.latlong', rast=madForest2000, outType='rast',
#' flags=c('quiet', 'overwrite'), grassDir=grassDir)
#' longRast <- faster('r.latlong', rast=madForest2000, outType='rast',
#' flags=c('quiet', 'overwrite', 'l'), grassDir=grassDir)
#' ll1 <- stack(latRast, longRast)
#' 
#' # same as:
#' ll2 <- fasterLongLatRasters(madForest2000, grassDir=grassDir)
#' 
#' # Example of chaining (ie, not reinitializing GRASS session):
#' # The second function uses the GRASS session initiated by the first function.
#' # It then uses the raster created in the GRASS session by the first function
#' # as the input for its module.
#' latRast <- faster('r.latlong', rast=madForest2000, outType='rast',
#' output='lat', flags=c('quiet', 'overwrite'), grassDir=grassDir)
#' longRast <- faster('r.latlong', input='lat', outType='rast', output='long',
#' flags=c('quiet', 'overwrite', 'l'), init=FALSE, grassDir=grassDir)
#' ll3 <- stack(latRast, longRast)
#' }
#' @export

faster <- function(
	mod,
	rast = NULL,
	vect = NULL,
	rastName = 'rast',
	vectName = 'vect',
	outType = NULL,
	output = 'output',
	flags = c('quiet', 'overwrite'),
	...,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE
) {

	args <- list(cmd=mod, flags=flags)
	args <- c(args, ...)
	args <- c(args, 'output' = output)

	# execute
	if (!alreadyInGrass) {
		input <- initGrass(alreadyInGrass, rast=rast, vect=vect, rastName=rastName, vectName=vectName, grassDir=grassDir)
		do.call(rgrass7::execGRASS, args=args)
		# rgrass7::execGRASS(mod, flags=flags, ...)
	} else {
		rgrass7::execGRASS(mod, flags=flags, ...)
	}
	
	if (grassToR) {

		if (pmatch(outType, c('raster', 'vector')) == 1) {
			
			out <- rgrass7::readRAST(output)
			out <- raster::raster(out)
			names(out) <- output
		
		} else if (pmatch(outType, c('raster', 'vector')) == 2) {
		
			out <- rgrass7::readVECT(output)
			
		}
			
		out
		
	}

}
