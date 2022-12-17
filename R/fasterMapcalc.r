#' User-defined calculations on a single raster
#'
#' This function creates a raster from another raster based on a user-defined function. It utilizes the \code{GRASS} module \code{r.mapcalc}.
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @param ... A set of rasters, raster stacks, raster bricks, or the names rasters in an existing \code{GRASS} session, or any combination thereof.
#' @param expression Character. Formula to evaluate. Below are examples where the input raster is named "rast" in the \code{GRASS} session and the output raster will be named "out". Note that rasters passed as rasters/stacks/bricks must be referred to in formulae using their raster name (not name in R). In other words, in the examples below we could call a raster in R \code{myRast} but \code{names(myRast)} yields \code{rast}, so we need to call it "\code{rast}" in the formulae. The raster "\code{out}" will be created by the function.
#' \itemize{
#' \item Take the input raster "rast", multiplies each cell by 0, then adds 1 to each cell: \code{'out = (rast * 0) + 1'}
#' \item Take the input raster "rast", multiplies each cell by 0, then adds 1 to each cell: \code{'out = rast^2'}
#' \item Take the input raster "rast" and redefines each cell value to 17: \code{'out = 17'}
#' \item Take the input raster "rast" and redefines each cell value to equal the sum of it and the four cells in the rooks's neighborhood (north, south, east, west of it): \code{'out = rast[0, 0] + rast[-1, 0] + rast[0, -1] + rast[0, 1] + rast[1, 0]'}
#' \item Low-pass (averaging) filter across a 3x3 neighborhood: \code{'out = (rast[-1, -1] + rast[-1, 0] + rast[-1, 1] + rast[0, -1] + rast[0, 0] + rast[0, 1] + rast[1, -1] + rast[1, 0] + rast[1, 1]) / 9'}
#' \item High-pass ("edge-finding") filter across a 3x3 neighborhood: \code{'out = -0.7 * rast[-1, -1] -1 * rast[-1, 0] -0.7 * rast[-1, 1] -1 * rast[0, -1] + 6.8 * rast[0, 0] -1 * rast[0, 1] -0.7 * rast[1, -1] -1 * rast[1, 0] -0.7 * rast[1, 1]'}
#' }
#' See the documentation for \code{r.mapcalc}{https://grass.osgeo.org/grass82/manuals/r.mapcalc.html} for more information. Note that some operations that may make sense in \code{GRASS} may not make sense when exported back to R (e.g., changing color).
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when calculating horizon height (i.e., function \code{r.horizon} in \code{GRASS}).
#' @return If \code{grassToR} if \code{TRUE}, then a raster object with the same coordinate reference system, extent, and resolution as \code{rast}. Regardless, a raster by a name given in \code{expression} (before the "=" symbol) is written into the \code{GRASS} session.
#' @details See the documentation for the \code{GRASS} module \code{r.mapcalc}{https://grass.osgeo.org/grass82/manuals/r.mapcalc.html}. The function \code{\link{fasterFocal}} \emph{may} be faster for focal calculations.
#'
#' @seealso \code{\link[terra]{app}}, \code{\link[terra]{focal}}, \code{\link{fasterFocal}}
#'
#' @examples man/examples/ex_fasterMapcalc.r
#'
#' @export

fasterMapcalc <- function(
	...,
	expression,
	grassDir = options()$grassDir,
	grassToR = TRUE
) {

	flags <- c('quiet', 'overwrite')
	
	# get just rasters
	rasts <- list(...)
	rasts <- rasts[which(sapply(rasts, class) %in% c('RasterLayer', 'RasterBrick', 'RasterStack'))]
	numRasts <- length(rasts)
	if (numRasts > 0) rastNames <- unlist(sapply(rasts, names))
	
	# initialize GRASS
	if (numRasts > 0) {
		input <- initGrass(FALSE, rast=rasts[[1]], vect=NULL, rastName=rastNames[1], grassDir=grassDir)

		# export remaining rasters
		if (numRasts > 1) {
			for (i in 2:length(rasts)) {
				exportRastToGrass(rasts[[i]], grassName=rastNames[i])
			}
		}
		
	}
	
	# execute
	rgrass::execGRASS('r.mapcalc', expression=expression, flags=flags)
	
	if (grassToR) {

		outRastName <- substr(expression, 1, regexpr(pattern='=', expression) - 1)
		outRastName <- terra::trim(outRastName)
		out <- rgrass::read_RAST(outRastName)
		out <- terra::rast(out)
		names(out) <- outRastName
		out
		
	}
	
}
