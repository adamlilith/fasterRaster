#' Raster with a fractal pattern
#'
#' Create a raster with a fractal pattern.
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @param dimension Numeric. Fractal dimension. Must be >2 and <3. Default is 2.05.
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}}.
#' @return If \code{grassToR} if \code{TRUE}, then a raster object with the same coordinate reference system, resolution, and extent as \code{rast}. Regardless, a raster is written into the \code{GRASS} session with the name given by \code{outGrassName}.
#'
#' @seealso \href{https://grass.osgeo.org/grass82/manuals/r.surf.fractal.html}{\code{r.surf.fractal}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterSurfFractal.r
#'
#' @export

fasterSurfFractal <- function(
	rast,
	dimension = 2.05,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	inRastName = 'rast',
	outGrassName = 'fractalRast',
	...
) {

	flags <- c('quiet', 'overwrite')
	
	if (dimension <= 2 | dimension >= 3) stop('Argument "dimension" must be >2 and <3.')
	
	# initialize GRASS
	inRastName <- .getInRastName(inRastName, rast)
	input <- initGrass(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, grassDir=grassDir)
	
	rgrass::execGRASS('r.surf.fractal', dimension=dimension, output=outGrassName, flags=flags, ...)

	if (grassToR) {

		out <- rgrass::read_RAST(outGrassName)
		out <- terra::rast(out)
		names(out) <- outGrassName
		out
		
	}
	
}
