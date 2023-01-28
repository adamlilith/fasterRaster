#' Create a raster with a fractal pattern
#'
#' Create a raster with a fractal pattern.
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @param dimension Numeric. Fractal dimension. Must be >2 and <3. Default is 2.05.
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster object with the same coordinate reference system, resolution, and extent as \code{rast}. Regardless, a raster is written into the \code{GRASS} session with the name given by \code{outGrassName}.
#'
#' @seealso \href{https://grass.osgeo.org/grass82/manuals/r.surf.fractal.html}{\code{r.surf.fractal}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterSurfFractal.r
#'
#' @export

fasterSurfFractal <- function(
	rast,
	inRastName,
	dimension = 2.05,
	outGrassName = 'fractalRast',

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	flags <- .getFlags(replace=replace)
	
	if (dimension <= 2 | dimension >= 3) stop('Argument "dimension" must be >2 and <3.')
	
	# initialize GRASS
	if (is.null(inits)) inits <- list()
	inits <- c(inits, list(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, replace=replace, grassDir=grassDir))
	input <- do.call('initGrass', inits)
	
	rgrass::execGRASS('r.surf.fractal', dimension=dimension, output=outGrassName, flags=flags, ...)

	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE)
		out
		
	}
	
}
