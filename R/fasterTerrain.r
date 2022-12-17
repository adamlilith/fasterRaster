#' Rasters of slope, aspect, curvature, and partial slopes
#'
#' This function is a potentially faster version of the \code{\link[terra]{terrain}} function in the \pkg{terra} package for calculating slope and aspect of a raster. It can also calculate profile curvature, tangential curvature, and slope in the east-west or north-south directions.
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @param slope Logical, if \code{TRUE} (default) then calculate slope.
#' @param slopeUnits Character, "units" in which to calculate slope: either \code{degrees} for degrees or \code{percent}.
#' @param aspect Logical, if \code{TRUE} then calculate aspect. Aspect is given in degrees from North going clockwise (0 = north, 90 = east, 180 = south, 270 = west).
#' @param northIs0 Logical. If \code{TRUE} (default), aspect will be reported such that 0 is north, and degrees run clockwise (90 is east, 180 south, 270 west). If \code{FALSE}, then aspect will be reported such that 0 is east, and degrees run counterclockwise (90 is north, 180 west, 270 south). The latter is the default in \code{GRASS}7.
#' @param profileCurve Logical, if \code{TRUE}, calculate profile curvature. Default is \code{FALSE}.
#' @param tanCurve Logical, if \code{TRUE}, calculate tangential curvature. Default is \code{FALSE}.
#' @param eastWestSlope Logical, if \code{TRUE}, calculate slope in east-west direction. Default is \code{FALSE}.
#' @param northSouthSlope Logical, if \code{TRUE}, calculate slope in north-south direction. Default is \code{FALSE}.
#' @param outGrassNameSlope Character. Name of slope raster in \pkg{GRASS}. Useful for referring to later in the same \code{GRASS} session.
#' @param outGrassNameAspect Character. Name of aspect raster in \pkg{GRASS}. Useful for referring to later in the same \code{GRASS} session.
#' @param outGrassNameProfileCurve Character. Name of profile curve raster in \pkg{GRASS}. Useful for referring to later in the same \code{GRASS} session.
#' @param outGrassNameTanCurve Character. Name of tangent curve raster in \pkg{GRASS}. Useful for referring to later in the same \code{GRASS} session.
#' @param outGrassNameEastWestSlope Character. Name of east-west slope raster in \pkg{GRASS}. Useful for referring to later in the same \code{GRASS} session.
#' @param outGrassNameNorthSouthSlope Character. Name of north-south slope raster in \pkg{GRASS}. Useful for referring to later in the same \code{GRASS} session.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when used for rasterizing (i.e., function \code{r.slope.aspect} in \code{GRASS}).
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, raster(s) given by the names in the \code{outGrassName*} arguments are used are written into the \code{GRASS} session.
#'
#' @details See the documentation for the \code{GRASS} module \code{r.slope.aspect}{https://grass.osgeo.org/grass82/manuals/r.slope.aspect.html}.
#'
#' @seealso \code{\link[terra]{terrain}}
#'
#' @examples man/examples/ex_fasterTerrain.r
#'
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
	
	grassDir = options()$grassDir,
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
	input <- initGrass(rast=rast, vect=NULL, grassDir=grassDir)

	# slope
	if (slope) rgrass::execGRASS('r.slope.aspect', elevation=input, slope=outGrassNameSlope, format=slopeUnits, flags=flags)
	
	# aspect (0 = east and goes counter-clockwise, so convert so 0 = north going clockwise)
	if (aspect) {
		rgrass::execGRASS('r.slope.aspect', elevation=input, aspect=outGrassNameAspect, flags=flags)
		if (northIs0) fasterConvertDegree(outGrassNameAspect, grassDir=grassDir, outGrassName=outGrassNameAspect, grassToR=FALSE)
	}
	
	# curvatures
	if (profileCurve) rgrass::execGRASS('r.slope.aspect', elevation=input, pcurvature=outGrassNameProfileCurve, flags=flags)
	if (tanCurve) rgrass::execGRASS('r.slope.aspect', elevation=input, tcurvature=outGrassNameTanCurve, flags=flags)
	
	# first-derivative slopes
	if (eastWestSlope) rgrass::execGRASS('r.slope.aspect', elevation=input, dx=outGrassNameEastWestSlope, flags=flags)
	if (northSouthSlope) rgrass::execGRASS('r.slope.aspect', elevation=input, dy=outGrassNameNorthSouthSlope, flags=flags)

	# return
	if (grassToR) {
	
		out <- terra::rast(rgrass::read_RAST(input))
	
		if (slope) out <- c(out, terra::rast(rgrass::read_RAST(outGrassNameSlope)))
		if (aspect) out <- c(out, terra::rast(rgrass::read_RAST(outGrassNameAspect)))
		if (profileCurve) out <- c(out, terra::rast(rgrass::read_RAST(outGrassNameProfileCurve)))
		if (tanCurve) out <- c(out, terra::rast(rgrass::read_RAST(outGrassNameTanCurve)))
		if (eastWestSlope) out <- c(out, terra::rast(rgrass::read_RAST(outGrassNameEastWestSlope)))
		if (northSouthSlope) out <- c(out, terra::rast(rgrass::read_RAST(outGrassNameNorthSouthSlope)))

		out <- terra::subset(out, 2:terra::nlyr(out))

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
