#' Export a spatial vector to an active GRASS session
#'
#' Export a spatial vector to an active \code{GRASS} session. To import a vector from \code{GRASS} to \pkg{R}, use \code{\link[rgrass]{read_VECT}}. Vectors that are larger than the active \code{GRASS} region are not cropped to the region (unlike when exporting rasters).
#'
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_autoRegion
#' @param vect A \code{SpatVector} (\pkg{terra} package) or \code{sf} (\pkg{sf} package) spatial vector object.
#' @param inVectName Name of vector in \code{GRASS}. If \code{NULL} (default), the name will be set to \code{'inputVect'}.
#' @param ... Additional arguments (unused).
#'
#' @return \code{TRUE} (invisibly, if the vector was successfully exported). The function also exports a vector to a \code{GRASS} session so it can be used by other functions.
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
		inVectName <- .getInVectName(inVectName, vect=vect)
		.checkVectExists(replace=replace, vect=vect, inVectName=inVectName, outGrassName=NULL)

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

	###############
	### end commons

	if (!inherits(vect, 'SpatVector')) vect <- terra::vect(vect)
	rgrass::write_VECT(vect, vname=inVectName, flags=flags)

	invisible(TRUE)
	
}
