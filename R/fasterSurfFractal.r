#' Generate a raster with a fractal pattern
#'
#' This function creates a raster with a fractal pattern. It utilizes the GRASS function \code{r.surf.fractal}.
#' @param rast Either a raster or the name of a raster in an existing GRASS session to serve as a template.
#' @param dimension Numeric. Fractal dimension. Must be >2 and <3. Default is 2.05.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{longitude} and \code{latitude}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}}.
#' @return If \code{grassToR} if \code{TRUE}, then a raster object with the same coordinate reference system, resolution, and extent as \code{rast}. Regardless, a raster is written into the GRASS session with the name given by \code{outGrassName}.
#' @details See the documentation for the GRASS module \code{r.surf.fractal} at \url{https://grass.osgeo.org/grass78/manuals/r.surf.fractal.html}.
#' @seealso
#' @examples
#' \donttest{
#' # change this to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8' # example for a PC
#' grassDir <- "/Applications/GRASS-7.8.app/Contents/Resources" # for a Mac
#'
#' data(madElev)
#' fract1 <- fasterSurfFractal(rast=madElev, dimension=2.1, grassDir=grassDir)
#' fract2 <- fasterSurfFractal(rast='rast', dimension=2.9,
#' alreadyInGrass=TRUE, grassDir=grassDir)
#' plot(stack(fract1, fract2))
#' }
#' @export

fasterSurfFractal <- function(
	rast,
	dimension = 2.05,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	outGrassName = 'surfFractal',
	...
) {

	flags <- c('quiet', 'overwrite')
	
	if (dimension <= 2 | dimension >= 3) stop('Argument "dimension" must be >2 and <3.')
	
	# initialize GRASS
	input <- initGrass(alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)
	
	rgrass7::execGRASS('r.surf.fractal', dimension=dimension, output=outGrassName, flags=flags, ...)

	if (grassToR) {

		out <- rgrass7::readRAST(outGrassName)
		out <- raster::raster(out)
		names(out) <- outGrassName
		out
		
	}
	
}
