#' Topographic wetness index raster
#'
#' This function creates a raster where cell values represent the Topographic Wetness Index (TPI), a measure of how much water drains or pools into a cell. It utilizes the \code{GRASS} function \code{r.topidx}.
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same coordinate reference system, extent, and resolution as \code{rast}. Regardless, a raster is written into the \code{GRASS} session. The name of this vector is given by \code{outGrassName}.
#'
#' @seealso \code{\link[terra]{terrain}} in \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/r.topidx.html}{\code{r.topidx}} in \code{GRASS} 
#'
#' @example man/examples/ex_fasterTopidx.r
#'
#' @export

fasterTWI <- function(
	rast,
	inRastName,
	outGrassName = 'twiRast',

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	flags <- .getFlags(replace=replace)
	
	# initialize GRASS
	if (is.null(inits)) inits <- list()
	inits <- c(inits, list(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, replace=replace, grassDir=grassDir))
	input <- do.call('startFaster', inits)

	# execute
	rgrass::execGRASS('r.topidx', input=input, output=outGrassName, flags=flags)

	# return
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		out

	} else { invisible(TRUE) }
	
}
