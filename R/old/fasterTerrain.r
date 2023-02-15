#' Rasters of slope, aspect, curvature, and partial slopes
#'
#' Calculate topographic indices, including slope, aspect, curvature, and partial slopes (slopes in the east-west or north-south directions).
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_cores
#' @inheritParams .sharedArgs_memory
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param v Name of the topographic metric(s) to calculate. Valid values include one or more of:
#' \itemize{
#' 	\item \code{'slope'}: Slope. Units are given by argument \code{units}.
#' 	\item \code{'aspect'}: Aspect. When argument \code{northIs0} is \code{TRUE} (default), then aspect is given in degrees from north going clockwise (0 = north, 90 = east, 180 = south, 270 = west).
#' 	\item \code{'profileCurve'}: Profile curvature.
#' 	\item \code{'tanCurve'}: Tangential curvature.
#' 	\item \code{'ewSlope'}: Slope in east-west direction (\emph{dx}).
#' 	\item \code{'nsSlope'}: Slope in north-south direction  (\emph{dy}).
#' 	\item \code{'ewCurve'}: Second partial derivative in east-west direction (\emph{dxx}).
#' 	\item \code{'nsCurve'}: Second partial derivative in north-south direction (\emph{dy}).
#' 	\item \code{'nsewCurve'}: Second partial derivative along east-west and north-south direction (\emph{dxy}).
#'	\item \code{'*'}: All of the above.
#' }
#' @param units Character, "units" in which to calculate slope: either \code{'degrees'} for degrees (default) or \code{'percent'}.
#' @param northIs0 Logical. If \code{TRUE} (default), aspect will be reported such that 0 is north, and degrees run clockwise (90 is east, 180 south, 270 west). If \code{FALSE}, then aspect will be reported such that 0 is east, and degrees run counterclockwise (90 is north, 180 west, 270 south). The latter is the default in \code{GRASS}, but the former is the default in \pkg{terra}'s \code{\link[terra]{terrain}} function, so is used here as the default.
#' @param outGrassName Name(s) of the rasters created in the \code{GRASS} session. Useful for chaining functions together. By default, these will be the values given in \code{v}.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, raster(s) given by the names in the \code{outGrassName} arguments are used are written into the \code{GRASS} session.
#'
#' @seealso \code{\link{fasterTWI}} in \pkg{fasterRaster}; \code{\link[terra]{terrain}} in \pkg{terra}; \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.slope.aspect.html}{\code{r.slope.aspect}}
#'
#' @example man/examples/ex_fasterTerrain.r
#'
#' @export

fasterTerrain <- function(
	rast,
	inRastName,
	v = 'slope',
	units = 'degrees',
	northIs0 = TRUE,
	outGrassName = v,
	
	cores = fasterGetOptions('cores', 1),
	memory = fasterGetOptions('memory', 300),
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### commons v1
	##############

		### arguments
		.checkRastExists(replace=replace, rast=NULL, inRastName=NULL, outGrassName=outGrassName, ...)
		if (!missing(rast)) {
			if (!inherits(rast, 'character') & !inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
			inRastName <- .getInRastName(inRastName, rast=rast)
			.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=NULL, ...)
		} else {
			rast <- inRastName <- NULL
		}

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		# if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterTerrain')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### errors
	metrics <- c('slope', 'aspect', 'profileCurve', 'tanCurve', 'ewSlope', 'nsSlope', 'ewCurve', 'nsCurve', 'nsewCurve')
	if (length(v) > 1L & any('*' %in% v)) stop('You must use "*" in argument "v" by itself.')
	if (length(v) == 1L && v == '*') v <- metrics
	if (any(!(v %in% metrics))) stop('Argument "v" has at least on invalid values.')

	if (any(v %in% c('slope', 'aspect')) && any(!(units %in% c('degrees', 'percent')))) stop('Argument "units" must be either "degrees" or "percent".')

	if (is.null(outGrassName) || (length(outGrassName) == 1L & outGrassName == '*')) outGrassName <- v
	if (length(v) != length(outGrassName)) stop('The length of "outGrassName" must be the same as the length of "v."')

	### function-specific
	args <- list(
		cmd = 'r.slope.aspect',
		elevation = inRastName,
		nprocs = cores,
		memory = memory,
		flags = flags
	)
	args <- c(args, dots)
	
	if ('slope' %in% v) {
		args$slope = outGrassName[v %in% 'slope']
		args$format = units
	}

	if ('aspect' %in% v) {
		args$aspect = outGrassName[v %in% 'aspect']
		args$format = units
	}

	if ('profileCurve' %in% v) {
		args$pcurvature = outGrassName[v %in% 'profileCurve']
	}

	if ('tanCurve' %in% v) {
		args$tcurvature = outGrassName[v %in% 'tanCurve']
	}

	if ('ewSlope' %in% v) {
		args$dx = outGrassName[v %in% 'ewSlope']
	}

	if ('nsSlope' %in% v) {
		args$dy = outGrassName[v %in% 'nsSlope']
	}

	if ('ewCurve' %in% v) {
		args$dxx = outGrassName[v %in% 'ewCurve']
	}

	if ('nsCurve' %in% v) {
		args$dyy = outGrassName[v %in% 'nsCurve']
	}

	if ('nsewCurve' %in% v) {
		args$dxy = outGrassName[v %in% 'nsewCurve']
	}

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### execute
	if (autoRegion) regionReshape(inRastName)
	do.call(rgrass::execGRASS, args=args)
	
	if ('aspect' %in% v & northIs0) {
	
		fasterConvertDegree(
			rast = outGrassName[v %in% 'aspect'],
			outGrassName = outGrassName[v %in% 'aspect'],
			replace = TRUE,
			grassToR = FALSE,
			autoRegion = FALSE,
			grassDir = grassDir
		)
		
	}

	if (grassToR) {
		
		for (ogn in outGrassName) {
		
			thisOut <- fasterWriteRaster(ogn, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
			out <- if (exists('out', inherits=FALSE)) {
				c(out, thisOut)
			} else {
				thisOut
			}
			
		}
		out
			
	} else { invisible(TRUE) }

}
