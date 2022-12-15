#' Write vector to an existing GRASS session
#'
#' Write a raster to an existing GRASS session.
#'
#' @param vect A \code{SpatVector} (\pkg{terra} package) or \code{sf} (\pkg{sf} package) spatial vector object.
#' @param grassName Name of vector in GRASS.
#' @return Nothing (exports a vector to a GRASS session so it can be used by other functions).
#'
#' @examples
#'
#' \donttest{
#'
#' data(madCoast0)
#' data(madCoast4)
#'
#' # start a GRASS session
#' initGrass(madCoast0)
#' exportVectToGrass(madCoast4)
#'
#' }
#'
#' @export
exportVectToGrass <- function(
	vect,
	grassName = 'vect'
) {

	if (inherits(vect, 'sf')) vect <- terra::vect(vect)
	rgrass::write_VECT(vect, vname=grassName, v.in.ogr_flags='overwrite')
	
}
