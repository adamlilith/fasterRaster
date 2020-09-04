#' Generate topographic wetness index raster
#'
#' This function creates a raster where cell values represent the Topographic Wetness Index (TPI), a measure of how much water drains or pools into a cell. It utilizes the GRASS function \code{r.topidx}.
#' @param rast Either a raster or the name of a raster in an existing GRASS session with values representing elevation (typically in meters).
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{longitude} and \code{latitude}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when calculating horizon height (i.e., function \code{r.horizon} in GRASS).
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same coordinate reference system, extent, and resolution as \code{rast}. Otherwise, a raster is written into the GRASS session. The name of this vector is given by \code{outGrassName}.
#' @details See the documentation for the GRASS module \code{r.topidx} at \url{https://grass.osgeo.org/grass78/manuals/r.topidx.html}.
#' @seealso
#' @examples
#' \donttest{
#' # change this to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8'
#'
#' data(madElev)
#' data(madForest2000)
#'
#' # must first project elevation raster
#' madElev_albers <- fasterProjectRaster(madElev, template=madForest2000,
#' grassDir=grassDir)
#' twi <- fasterTopidx(madElev_albers, grassDir=grassDir)
#' par(mfrow=c(1, 2))
#' plot(madElev_albers)
#' plot(twi)
#' }
#' @export

fasterTopidx <- function(
	rast,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	outGrassName = 'topoWetnessIndex',
	...
) {

	out <- faster(mod='r.topidx', rast=rast, input='rast', outType = 'raster', output=outGrassName, grassDir=grassDir, alreadyInGrass=alreadyInGrass, grassToR=grassToR)
	
	out
	
}
