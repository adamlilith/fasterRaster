#' Rasters of slope, aspect, curvature, and partial slopes
#'
#' This function is a potentially faster version of the \code{\link[terra]{terrain}} function in the \pkg{terra} package for calculating slope and aspect of a raster. It can also calculate profile curvature, tangential curvature, and slope in the east-west or north-south directions.
#' @param rast Either a raster with elevation data or the name of a raster in an existing GRASS session.
#' @param slope Logical, if \code{TRUE} (default) then calculate slope.
#' @param slopeUnits Character, "units" in which to calculate slope: either \code{degrees} for degrees or \code{percent}.
#' @param aspect Logical, if \code{TRUE} then calculate aspect. Aspect is given in degrees from North going clockwise (0 = north, 90 = east, 180 = south, 270 = west).
#' @param northIs0 Logical. If \code{TRUE} (default), aspect will be reported such that 0 is north, and degrees run clockwise (90 is east, 180 south, 270 west). If \code{FALSE}, then aspect will be reported such that 0 is east, and degrees run counterclockwise (90 is north, 180 west, 270 south). The latter is the default in GRASS7.
#' @param profileCurve Logical, if \code{TRUE}, calculate profile curvature. Default is \code{FALSE}.
#' @param tanCurve Logical, if \code{TRUE}, calculate tangential curvature. Default is \code{FALSE}.
#' @param eastWestSlope Logical, if \code{TRUE}, calculate slope in east-west direction. Default is \code{FALSE}.
#' @param northSouthSlope Logical, if \code{TRUE}, calculate slope in north-south direction. Default is \code{FALSE}.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example for a Windows system: \code{'C:/Program Files/GRASS GIS 8.2'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the output will be returned to R. If \code{FALSE}, then the output is left in the GRASS session and named the value in \code{outGrassName} \code{slope}, \code{aspect}, \code{profileCurve}, \code{tanCurve}, \code{eastWestSlope}, or \code{northSouthSlope}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassNameSlope Character. Name of slope raster in GRASS. Useful for referring to later in the same GRASS session.
#' @param outGrassNameAspect Character. Name of aspect raster in GRASS. Useful for referring to later in the same GRASS session.
#' @param outGrassNameProfileCurve Character. Name of profile curve raster in GRASS. Useful for referring to later in the same GRASS session.
#' @param outGrassNameTanCurve Character. Name of tangent curve raster in GRASS. Useful for referring to later in the same GRASS session.
#' @param outGrassNameEastWestSlope Character. Name of east-west slope raster in GRASS. Useful for referring to later in the same GRASS session.
#' @param outGrassNameNorthSouthSlope Character. Name of north-south slope raster in GRASS. Useful for referring to later in the same GRASS session.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when used for rasterizing (i.e., function \code{r.slope.aspect} in GRASS).
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, raster(s) given by the names in the \code{outGrassName*} arguments are used are written into the GRASS session.
#' @details See the documentation for the GRASS module \code{r.slope.aspect}{https://grass.osgeo.org/grass82/manuals/r.slope.aspect.html}.
#' @seealso \code{\link[terra]{terrain}}
#' @examples
#' \donttest{
#' # change this according to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
#' grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac
#' 
#' data(madElev)
#' 
#' # could also use terrain() which may be faster
#' # in this example
#' topo <- fasterTerrain(rast=madElev, slope=TRUE, aspect=TRUE,
#' grassDir=grassDir)
#'
#' # terrain function from the raster package... much slower in this example
#' # slp <- terrain(elev, opt='slope', unit='degrees')
#' # asp <- terrain(elev, opt='aspect', unit='degrees')
#' # topo <- stack(slp, asp)
#' # names(topo) <- c('slope', 'aspect')
#' plot(topo)
#' }
#' @export

fasterTerrain <- function(
	rast,
	slope = TRUE,
	slopeUnits = 'degrees',
	aspect = FALSE,
	northIs0 = TRUE,
	profileCurve = FALSE,
	tanCurve = FALSE,
	eastWestSlope = FALSE,
	northSouthSlope = FALSE,
	
	grassDir = options('grassDir'),
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	
	outGrassNameSlope = 'slope',
	outGrassNameAspect = ifelse(northIs0, 'aspectNorthIs0', 'aspectEastIs0'),
	outGrassNameProfileCurve = 'profileCurve',
	outGrassNameTanCurve = 'tanCurve',
	outGrassNameEastWestSlope = 'eastWestSlope',
	outGrassNameNorthSouthSlope = 'northSouthSlope',
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	input <- initGrass(alreadyInGrass, rast=rast, vect=NULL, grassDir=grassDir)

	# slope
	if (slope) rgrass::execGRASS('r.slope.aspect', elevation=input, slope=outGrassNameSlope, format=slopeUnits, flags=flags)
	
	# aspect (0 = east and goes counter-clockwise, so convert so 0 = north going clockwise)
	if (aspect) {
		rgrass::execGRASS('r.slope.aspect', elevation=input, aspect=outGrassNameAspect, flags=flags)
		if (northIs0) fasterConvertDegree(outGrassNameAspect, grassDir=grassDir, alreadyInGrass=TRUE, outGrassName=outGrassNameAspect, grassToR=FALSE)
	}
	
	# curvatures
	if (profileCurve) rgrass::execGRASS('r.slope.aspect', elevation=input, pcurvature=outGrassNameProfileCurve, flags=flags)
	if (tanCurve) rgrass::execGRASS('r.slope.aspect', elevation=input, tcurvature=outGrassNameTanCurve, flags=flags)
	
	# first-derivative slopes
	if (eastWestSlope) rgrass::execGRASS('r.slope.aspect', elevation=input, dx=outGrassNameEastWestSlope, flags=flags)
	if (northSouthSlope) rgrass::execGRASS('r.slope.aspect', elevation=input, dy=outGrassNameNorthSouthSlope, flags=flags)

	# return
	if (grassToR) {
	
		out <- raster::raster(rgrass::read_RAST(input))
	
		if (slope) out <- raster::stack(out, raster::raster(rgrass::read_RAST(outGrassNameSlope)))
		if (aspect) out <- raster::stack(out, raster::raster(rgrass::read_RAST(outGrassNameAspect)))
		if (profileCurve) out <- raster::stack(out, raster::raster(rgrass::read_RAST(outGrassNameProfileCurve)))
		if (tanCurve) out <- raster::stack(out, raster::raster(rgrass::read_RAST(outGrassNameTanCurve)))
		if (eastWestSlope) out <- raster::stack(out, raster::raster(rgrass::read_RAST(outGrassNameEastWestSlope)))
		if (northSouthSlope) out <- raster::stack(out, raster::raster(rgrass::read_RAST(outGrassNameNorthSouthSlope)))

		out <- raster::subset(out, 2:raster::nlayers(out))

		name <- c(
			ifelse(slope, outGrassNameSlope, NA),
			ifelse(aspect, outGrassNameAspect, NA),
			ifelse(profileCurve, outGrassNameProfileCurve, NA),
			ifelse(tanCurve, outGrassNameTanCurve, NA),
			ifelse(eastWestSlope, outGrassNameEastWestSlope, NA),
			ifelse(northSouthSlope, outGrassNameNorthSouthSlope, NA)
		)
		
		name <- stats::na.omit(name)
		names(out) <- name
		
		out
		
	}
	
}
