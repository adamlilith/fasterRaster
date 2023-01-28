#' Crop a raster to the area of overlap with another raster or vector
#'
#' Crop a raster to the extent of another raster or vector. NB: This function is actualy an alias for \code{\link{fasterMask}}, which creates a mask from a raster or vector, then creates a copy of the overlapping portion of the focal raster.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param template Any of:
#' \itemize{
#'		\item A \code{SpatRaster}, \code{SpatVector}, or object of class \code{sf}, used to define the extent of the crop region.
#'		\item The name of raster or vector already in the \code{GRASS} session.
#' }
#' @param rastOrVect Either \code{'raster'} or \code{'vector'}. If \code{NULL} (default), then the function will attempt to guess whether \code{template} refers to a raster or vector. However, in \code{GRASS}, it is possible to have a raster and a vector of the same name. If this is the case, then you can specify whether \code{template} is a raster or vector (partial matching is supported).
#'
#' @param trim If \code{TRUE} (default), then remove rows and columns that are entirely \code{NA}. This only has effect if the raster is imported back to \code{R} (\code{grassToR} is \code{TRUE}). It does not affect the raster in \code{GRASS}. It is conducted in \code{R} using \pkg{terra}'s \code{\link[terra]{trim}} function.
#' 
#' @return A \code{SpatRaster}. Also creates a raster in a new grass session named \code{outGrassName}.
#' 
#' @seealso \code{\link{fasterRes}}, \code{\link{fasterExtendRast}}, and \code{\link{fasterTrimRast}} in \pkg{fasterRaster}; \code{\link[terra]{crop}} and \code{\link[terra]{extend}} in \pkg{terra}; \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.clip.html}{\code{r.clip}}--but note that this function does \emph{not} use \code{r.clip}.
#' 
#' @examples man/examples/ex_fasterCropExtendRast.r
#'
#' @export

fasterCropRast <- function(
	rast,
	template,
	inRastName,
	inTemplateName,
	rastOrVect = NULL,
	outGrassName = 'croppedRast',
	trim = TRUE,

	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	fasterMask(
		rast = rast,
		mask = template,
		inRastName = inRastName,
		inMaskName = inTemplateName,
		rastOrVect = rastOrVect,
		
		removeMask = TRUE,
		outGrassName = outGrassName,
		
		replace = replace,
		grassToR = FALSE,
		autoRegion = autoRegion,
		grassDir = grassDir,
		...
	)
	
	# return
	if (grassToR) {

		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE)
		if (trim) out <- terra::trim(out)
		out <- terra::setMinMax(out)
		out

	}

}
