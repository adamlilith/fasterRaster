#' User-defined calculations on one or more rasters
#'
#' This function creates a raster from mathematical or logical operations on one or more rasters.
#'
#' @inheritParams .sharedArgs_rast_plural
#' @inheritParams .sharedArgs_inRastName_plural
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#'
#' @param expression Character. Formula to evaluate. Below are examples where the input raster is named "rast" in the \code{GRASS} session and the output raster will be named "out". Note that raster must be referred to in formulae using their raster name (not name in R). In other words, in the examples below we could call a raster in R \code{myRast} but \code{names(myRast)} yields \code{rast}, so we need to call it "\code{rast}" in the formulae. The raster "\code{out}" will be created by the function. You also need to put spaces before and after the equals sign (e.g., \code{out = 2 * rast}, but not \code{out=2 * rast}).
#' \itemize{
#' \item Take the input raster "rast", multiplies each cell by 0, then adds 1 to each cell: \code{'out = (rast * 0) + 1'}
#' \item Take the input raster "rast", multiplies square the value of each cell: \code{'out = rast^2'}
#' \item Take the input raster "rast" (implicitly), and sets each cell value to 17: \code{'out = 17'}
#' \item Take the input rasters "rast1" and "rast2", and multiplies them together. Note that in this case, \code{rast} will actually be a \code{SpatRaster} object with two layers named "rast1" and "rast2": \code{'out = rast1 * rast2'}
#' \item Take the input raster "rast" and redefines each cell value to equal the sum of it and the four cells in the rooks's neighborhood (north, south, east, west of it): \code{'out = rast[0, 0] + rast[-1, 0] + rast[0, -1] + rast[0, 1] + rast[1, 0]'}
#' \item Low-pass (averaging) filter across a 3x3 neighborhood: \code{'out = (rast[-1, -1] + rast[-1, 0] + rast[-1, 1] + rast[0, -1] + rast[0, 0] + rast[0, 1] + rast[1, -1] + rast[1, 0] + rast[1, 1]) / 9'}
#' \item High-pass ("edge-finding") filter across a 3x3 neighborhood: \code{'out = -0.7 * rast[-1, -1] -1 * rast[-1, 0] -0.7 * rast[-1, 1] -1 * rast[0, -1] + 6.8 * rast[0, 0] -1 * rast[0, 1] -0.7 * rast[1, -1] -1 * rast[1, 0] -0.7 * rast[1, 1]'}
#' }
#' See the documentation for \code{r.mapcalc}{https://grass.osgeo.org/grass82/manuals/r.mapcalc.html} for more information. Note that some operations that may make sense in \code{GRASS} may not make sense when exported back to R (e.g., changing color).
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster object with the same coordinate reference system, extent, and resolution as \code{rast}. Regardless, a raster by a name given in \code{expression} (before the "=" symbol) is written into the \code{GRASS} session.
#'
#' @details The function \code{\link[terra]{focal}}, \code{\link[terra]{focalCpp}}, and \code{\link{fasterFocal}} \emph{may} be faster for focal calculations. 
#'
#' @seealso \code{\link[terra]{app}} and \code{\link[terra]{focal}} in \pkg{terra}; \code{\link{fasterFocal}} in \pkg{fasterRaster}; \href{https://grass.osgeo.org/grass82/manuals/r.mapcalc.html}{\code{r.mapcalc}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterApp.r
#'
#' @export

fasterApp <- function(
	rast,
	expression,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	inRastName = NULL,
	outGrassName = 'appRast'
) {

	flags <- c('quiet', 'overwrite')
	
	if (!inherits(rast, 'SpatRaster') & !inherits(rast, 'character')) rast <- terra::rast(rast)
	
	### export rasters
	if (inherits(rast, 'SpatRaster')) {

		numRasts <- terra::nlyr(rast)
		rastNames <- names(rast)
	
		inRastName <- .getInRastName(inRastName, rast)
		input <- initGrass(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, grassDir=grassDir)
	
	}

	# execute
	rgrass::execGRASS('r.mapcalc', expression=expression, flags=flags)
	
	if (grassToR) {

		outRastName <- substr(expression, 1, regexpr(pattern='=', expression) - 1)
		outRastName <- trimws(outRastName)
		out <- rgrass::read_RAST(outRastName, flags='quiet')
		names(out) <- outGrassName
		out
		
	}
	
}
