#' Remove all rows and columns of a raster that are entirely NA
#'
#' This function removes all rows and columns from a raster that are entirely \code{NA} values.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param warn If \code{TRUE} (default), display a warning when using this function about cropping any other rasters in the \code{GRASS} session.
#'
#' @return A \code{SpatRaster}. Also exports a raster to a new or existing \code{GRASS} session.
#'
#' @seealso \code{\link{fasterExtendRast}} and \code{\link{fasterCropExtendRast}} in \pkg{fasterRaster}; \code{\link[terra]{trim}} in \pkg{terra}; \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.clip.html}{\code{r.clip}}--but note this function does not use \code{r.clip}.
#'
#' @examples man/examples/ex_fasterResizeRast.r
#'
#' @export

fasterTrimRast <- function(
	rast,
	inRastName,

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	flags <- .getFlags(replace=replace)

	# initialize GRASS
	if (is.null(inits)) inits <- list()
	inits <- c(inits, list(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, replace=replace, grassDir=grassDir))
	input <- do.call('startFaster', inits)

say(TBD)
	# do the work
	rgrass::execGRASS('g.region', zoom=input, flags=flags)
	fasterCopy(inRastName, 'TEMPTEMP_copiedRast', replace=TRUE)
	fasterRm(inRastName)
	fasterRename('TEMPTEMP_copiedRast', inRastName)

	# return
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		out

	} else { invisible(TRUE) }
	
}
