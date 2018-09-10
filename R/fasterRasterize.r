#' Rasterize using a call to GRASS GIS.
#'
#' This function is a potentially faster version of the \code{\link[raster]{rasterize}} function in the \pkg{ratser} package which converts a spatial points, lines, or polygon into a raster based on a "template" raster. All cells covered by the spatial object can either have values taken from the spatial object or a user-defined.
#' @param x SpatialPoints, SpatialPointsDataFrame, SpatialLines, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame or the full path and name of such an object.
#' @param y Raster* object, serves as a template for the new raster or the full path and name of such an object.
#' @param use Character, indicates the types of values to be "burned" to the raster. Options include
#' \itemize{
#' \item \code{value} (default): a user-define value given by \code{value}
#' \item \code{field}: values directly from a field in \code{x} named by \code{field}
#' \item \code{category}: values according to which polygon is covered by a cell named in \code{field}
#' \item \code{z}: z-coordinate (points or contours only)
#' \item \code{direction}: flow direction (lines only)
#' }
#' @param field Name of column in \code{x} with values or category labbels to which to burn to the raster.
#' @param value Numeric, value to burn to each cell overlapped by the spatial object in \code{x}.
#' @param grassLoc Either \code{NULL} or a 3-element character vector. If the latter, the first element is the base path to the installation of GRASS, the second the version number, and the third the install type for GRASS.  For example, \code{c('C:/OSGeo4W64/', 'grass-7.4.1', 'osgeo4W')}. See \code{\link[link2GI]{linkGRASS7}} for further help. If \code{NULL} (default) the an installation of GRASS is searched for; this may take several minutes.
#' @param ... Arguments to pass to \code{\link[rgrass7]{execGRASS}} when used for rasterizing (i.e., function \code{v.to.rast} in GRASS).
#' @return A raster with the same extent, resolution, and coordinate reference system as \code{y}.
#' @seealso \code{\link[raster]{rasterize}}
#' @export

fasterRasterize <- function(
	x,
	y,
	use = 'value',
	value = 1,
	field = NULL,
	grassLoc = NULL,
	...
) {

	flags <- c('quiet', 'overwrite')

	# load spatial object and raster
	if (class(x) == 'character') x <- raster::shapefile(x)
	if (class(y) == 'character') y <- raster::raster(y)

	# ensure spatial data frame to avert error
	if (class(x) == 'SpatialPoints') x <- as(x, 'SpatialPointsDataFrame')
	if (class(x) == 'SpatialLines') x <- as(x, 'SpatialLinesDataFrame')
	if (class(x) == 'SpatialPolygons') x <- as(x, 'SpatialPolygonsDataFrame')

	# initialize GRASS
	link2GI::linkGRASS7(y, default_GRASS7=grassLoc, gisdbase=raster::tmpDir(), location='temp')

	rgrass7::writeVECT(x, vname='shape', v.in.ogr_flags=flags)

	# rasterize
	if (use == 'field') {
		rgrass7::execGRASS('v.to.rast', input='shape', output='out', use='attr', attribute_column=field, flags=flags)
	} else if (use == 'category') {
		rgrass7::execGRASS('v.to.rast', input='shape', output='out', use='cat', label_column=field, flags=flags)
	} else if (use == 'value') {
		rgrass7::execGRASS('v.to.rast', input='shape', output='out', use='val', value=value, flags=flags)
	} else {
		rgrass7::execGRASS('v.to.rast', input='shape', output='out', use=use, flags=flags)
	}

	# get raster back to R
	out <- rgrass7::readRAST('out')
	out <- raster::raster(out)
	out

}
