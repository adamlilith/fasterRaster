#' Convert degrees between cases with 0 = north and 0 = east
#' 
#' This function takes either a numeric vector or a raster where values represent degrees (typically in a direction). It then converts between cases where 0 degrees (direction) is intended to point east, with degrees running counterclockwise so 90 is north, 180 is west, and 270 south (this the the default for \code{GRASS} 7 modules) and cases where 0 degrees is intended to point north, with degrees running clockwise, so 90 is east, 180 south, and 270 west.
#'
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass
#' @param rast Numeric vector \emph{or} \code{SpatRaster}. Values should represent degrees. The conversion is symmetric, so \code{rast} can represent degrees in the system where 0 is north or 0 is east.
#'
#' @return Numeric, raster, or raster stack. Regardless, a set of rasters will be written into the current \code{GRASS} session. The name(s) of the rasters in \code{GRASS} will be given by \code{outGrassName} with \code{'_', i} appended, where \code{i} starts at 1 for the first raster and increments by 1. For example, if \code{outGrassName = 'degreeConvert'}, then the raster(s) will be named \code{degreeConvert_1}, \code{degreeConvert_2}, etc.
#'
#' @example man/examples/ex_fasterConvertDegree.r
#'
#' @export

fasterConvertDegree <- function(
	rast,
	inRastName,
	outGrassName = 'degreeConvertRast',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	## scalar
	if (inherits(rast, c('numeric', 'integer'))) {
		
		out <- ((360 - rast) %% 360 + 90) %% 360
	
	## raster
	} else {
		
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

			### ellipses and initialization arguments
			initsDots <- .getInitsDots(..., callingFx = 'fasterConvertDegree')
			inits <- initsDots$inits
			dots <- initsDots$dots

		###############
		### end commons
			
		# initialize GRASS
		input <- do.call('startFaster', inits)
			
		### execute
		ex <- paste0(outGrassName, ' = ((360 - ', inRastName, ') % 360 + 90) % 360')
		
		fasterApp(
			rast = input,
			expression = ex,
			replace = replace,
			grassToR = FALSE,
			outGrassName = outGrassName,
			restartGrass = FALSE,
			autoRegion = FALSE,
			grassDir = grassDir
		)

		### export
		if (grassToR) {

			out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
			
		}
		
	}
	
	if (inherits(rast, c('numeric', 'integer')) | grassToR) {
		out
	} else { invisible(TRUE) }
	
}
