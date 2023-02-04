#' Export raster(s) to an existing GRASS session
#'
#' Export one or more rasters to an existing \code{GRASS} session. To import a raster from \code{GRASS} to \pkg{R}, use \code{\link[rgrass]{read_RAST}}. Note that rasters imported back from \code{GRASS}, when saved using \pkg{terra}'s \code{\link[terra]{writeRaster}} function, often obtain the integer data type, which can truncate values. To obviate this, see \code{\link{writeRaster4}} or save rasters directly from \code{R} using \code{\link{fasterWriteRaster}}.
#'
#' @inheritParams .sharedArgs_inRastName_multiple
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_autoRegion
#'
#' @param rast A \code{SpatRaster} with one or more layers, or a character vector providing the path and names of raster files to import into the active \code{GRASS} session.
#' @param ... Additional arguments (unused).
#'
#' @return \code{TRUE} (invisibly, if the raster was successfully exported). The function also exports a raster to a \code{GRASS} session so it can be used by other functions.
#'
#' @seealso \code{\link[terra]{rast}} in package \pkg{terra}; \code{\link[rgrass]{write_RAST}} and \code{\link[rgrass]{read_RAST}} in package \pkg{rgrass}; and \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.in.gdal.html}{r.in.gdal}
#'
#' @examples man/examples/ex_fasterRast_fasterVect.R
#'
#' @export
fasterRast <- function(
	rast,
	inRastName,
	replace = fasterGetOptions('replace', FALSE),
	autoRegion = fasterGetOptions('autoRegion', FALSE),
	...
) {

	### commons v1
	##############

		### arguments
		inRastName <- .getInRastName(inRastName, rast)
		.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=NULL)

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

	###############
	### end commons
	
	if (!inherits(rast, 'SpatRaster') & !inherits(rast, 'character')) rast <- terra::rast(rast)

	# export to GRASS
	if (inherits(rast, 'character')) {
	
		for (i in seq_along(rast)) {
			
			n <- terra::nlyr(rast(rast[i]))
			rgrass::execGRASS('r.in.gdal', input=rast[i], output=inRastName[i], flags=flags, intern=TRUE)
			
		}
	
	} else {

		# NB writing first raster in a stack actually writes all of them
		success <- rgrass::write_RAST(rast[[1L]], vname=inRastName[1L], flags=flags, verbose=FALSE)
		
	}
		
	# if multi-layer raster, rasters are imported using the first name plus a ".#" where # is a number, so they need renamed
	if (inherits(rast, 'SpatRaster')) {
		
		n <- terra::nlyr(rast)
		if (n > 1L) {
		
			baseName <- inRastName[1L]
		
			for (i in 1L:n) {
			
				from <- paste0(baseName, '.', i)
				to <- inRastName[i]
				fasterRename(from=from, to=to, type='raster')
			
			}
		
		}
	}
	
	invisible(TRUE)

}
