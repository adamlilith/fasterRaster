#' Extend a raster to a larger region
#'
#' Extending a raster involves increasing the \code{GRASS} "region" into which it is imported and adding "empty" rows or columns with \code{NA} values.\cr
#' IMPORTANT: This function will extend \emph{all} rasters in the currently active \code{GRASS} session. If you want to extend a raster without affecting the current \code{GRASS} session:
#' \itemize{
#'		\item Make a new \code{GRASS} session using \code{\link{startFaster}} and change the argument \code{location} to something other than \code{default}. Use this function to export the raster to be extended to the session.
#'		\item Import the raster back to \code{R} (if desired), using \code{\link[rgrass]{read_RAST}}.
#'		\item Switch back to the original session using \code{startFaster(location='default')} (for example).
#' }
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param template A \code{SpatRaster} to define the new extent of the raster. Note that this has to be an actual raster, not the quoted name of one in an existing \code{GRASS} session.
#' @param warn If \code{TRUE} (default), display a warning when using this function about removing any existing \code{GRASS} session.
#'
#' @return A \code{SpatRaster}. Also creates a raster in a new grass session named \code{inRastName}, and "pads" all other rasters in the session with \code{NA} rows and columns to make them fit in the new region.
#' 
#' @seealso \code{\link{fasterCropExtendRast}} and \code{\link{fasterTrimRast}} in \pkg{fasterRaster}; \code{\link[terra]{crop}} and \code{\link[terra]{extend}} in \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/g.region.html}{\code{g.region}} in \code{GRASS}
#'
#' @examples man/examples/ex_fasterResizeRast.r
#'
#' @export

fasterExtendRast <- function(
	rast,
	inRastName,

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### begin common
	flags <- .getFlags(replace=replace)
	inRastName <- .getInRastName(inRastName, rast)
	# if (is.null(inVectName)) inVectName <- 'vect'
	
	# region settings
	success <- .rememberRegion()
	# on.exit(.restoreRegion(), add=TRUE)
	on.exit(regionResize(), add=TRUE)
	
	if (is.null(inits)) inits <- list()
	### end common

	inits <- c(inits, list(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, replace=replace, grassDir=grassDir))
	input <- do.call('startFaster', inits)

	
###
print(TBD)
	
	success <- regionResize()

	# return
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE)
		out

	} else { invisible(TRUE) }

}
