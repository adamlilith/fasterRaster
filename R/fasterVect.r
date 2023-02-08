#' Export a spatial vector to an active GRASS session
#'
#' Export a spatial vector to an active \code{GRASS} session. To import a vector from \code{GRASS} to \pkg{R}, use \code{\link[rgrass]{read_VECT}}. Vectors that are larger than the active \code{GRASS} region are not cropped to the region (unlike when exporting rasters).
#'
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_autoRegion
#' @param vect A single \code{SpatVector} (\pkg{terra} package) or \code{sf} object (\pkg{sf} package) spatial vector object, \emph{or} the file path(s) and name(s) of one or more spatial vectors. To see which formats are supported use \code{\link{fasterWriteVector()}} or see \href{https://grass.osgeo.org/grass82/manuals/v.in.ogr.html}{\code{v.in.ogr}}
#' @param inVectName Name of vector(s) in \code{GRASS}. If \code{NULL} (default), the name will be set to \code{'inputVect'}.
#' @param ... Additional arguments (unused).
#'
#' @return \code{TRUE} (invisibly, if the vector was successfully exported). The function also exports a vector to the active \code{GRASS} session.
#'
#' @seealso \code{\link[rgrass]{write_VECT}} and \code{\link[rgrass]{read_VECT}} in package \pkg{rgrass}
#'
#' @examples man/examples/ex_fasterRast_fasterVect.R
#'
#' @export
fasterVect <- function(
	vect,
	inVectName,
	replace = fasterGetOptions('replace', FALSE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	...
) {

	### commons v1
	##############

		### arguments
		if (!inherits(vect, 'character') & !inherits(vect, 'SpatVector')) vect <- terra::vect(vect)
		inVectName <- .getInVectName(inVectName, vect=vect)
		if (!replace) {
			if (any(fasterExists(inVectName))) stop('Vector(s) of the given name(s) already exist in GRASS.\nUse "replace=TRUE" or provide a different name with "inVectName".')
		}

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

	###############
	### end commons

	# export to GRASS
	if (inherits(vect, 'character')) {
	
		for (i in seq_along(vect)) {
			rgrass::execGRASS('v.in.ogr', input=vect[i], output=inVectName[i], flags=flags, intern=TRUE)
		}
	
	} else {
		success <- rgrass::write_VECT(vect, vname=inVectName, flags=flags)
	}

	invisible(TRUE)
	
}
