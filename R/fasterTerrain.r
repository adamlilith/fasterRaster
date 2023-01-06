#' Rasters of slope, aspect, curvature, and partial slopes
#'
#' Calculate topographic indices, including slope, aspect, curvature, and partial slopes (slopes in the east-west or north-south directions).
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @param metrics Name of the topographic metric(s) to calculate. Valid values include one or more of:
#' \itemize{
#' 	\item \code{'slope'}: Slope. Units are given by argument \code{slopeUnits}.
#' 	\item \code{'aspect'}: Aspect. When argument \code{northIs0} is \code{TRUE} (default), then aspect is given in degrees from north going clockwise (0 = north, 90 = east, 180 = south, 270 = west).
#' 	\item \code{'profileCurve'}: Profile curvature.
#' 	\item \code{'tanCurve'}: Tangential curvature.
#' 	\item \code{'ewSlope'}: Slope in east-west direction.
#' 	\item \code{'nsSlope'}: Slope in north-south direction.
#' }
#' @param slopeUnits Character, "units" in which to calculate slope: either \code{'degrees'} for degrees (default) or \code{'percent'}.
#' @param northIs0 Logical. If \code{TRUE} (default), aspect will be reported such that 0 is north, and degrees run clockwise (90 is east, 180 south, 270 west). If \code{FALSE}, then aspect will be reported such that 0 is east, and degrees run counterclockwise (90 is north, 180 west, 270 south). The latter is the default in \code{GRASS}, but the former is the default in \pkg{terra}'s \code{\link[terra]{terrain}} function, so is used here as the default.
#' @param outGrassName Name(s) of the rasters created in the \code{GRASS} session. Useful for chaining functions together. By default, these will be the values given in \code{metrics}.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, raster(s) given by the names in the \code{outGrassName} arguments are used are written into the \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{terrain}} in \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/r.slope.aspect.html}{\code{r.slope.aspect}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterTerrain.r
#'
#' @export

fasterTerrain <- function(
	rast,
	metrics = 'slope',
	slopeUnits = 'degrees',
	northIs0 = TRUE,
	
	grassDir = options()$grassDir,
	grassToR = TRUE,
	
	inRastName = NULL,
	outGrassName = NULL
) {

	flags <- c('quiet', 'overwrite')
	
	if (is.null(outGrassName)) outGrassName <- metrics
	if (length(metrics) != length(outGrassName)) stop('The length of "outGrassName" must be the same as the length of "metrics."')
	
	# initialize GRASS
	input <- initGrass(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, grassDir=grassDir)

	for (metric in metrics) {
	
		if (metrics == 'slope') {
			rgrass::execGRASS('r.slope.aspect', elevation=input, slope='slope', format=slopeUnits, flags=flags)
		} else if (metrics == 'aspect') {
			rgrass::execGRASS('r.slope.aspect', elevation=input, aspect='aspect', format=slopeUnits, flags=flags)
			if (northIs0) fasterConvertDegree('aspect', grassDir=grassDir, outGrassName=outGrassNameAspect, grassToR=FALSE)
		} else if (metric == 'profileCurve') {
			rgrass::execGRASS('r.slope.aspect', elevation=input, pcurvature='profileCurve', flags=flags)
		} else if (metric == 'tanCurve') {
			rgrass::execGRASS('r.slope.aspect', elevation=input, tcurvature='tanCurve', flags=flags)
		} else if (metric == 'ewSlope') {
			rgrass::execGRASS('r.slope.aspect', elevation=input, dx='ewSlope', flags=flags)
		} else if (metric == 'nsSlope') {
			rgrass::execGRASS('r.slope.aspect', elevation=input, dy='nsSlope', flags=flags)
		}
		
		if (grassToR) {
			out <- if (exists('out', inherits=FALSE)) {
				c(out, rgrass::read_RAST(metric), flags='quiet')
			} else {
				rgrass::read_RAST(metric, flags='quiet')
			}
		}
	
	} # next metric

	if (grassToR) {
		names(out) <- outGrassName
		out
	}
	
}
