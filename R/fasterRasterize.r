#' Convert a spatial vector to a raster
#'
#' This function converts a spatial points, lines, or polygon vector into a raster based on a "template" raster. All cells covered by the vector can either have values taken from the vector or be set to a user-defined value.
#'
#' @inheritParams .sharedArgs_vect
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param use Character, indicates the types of values to be "burned" to the raster. Options include
#' \itemize{
#' \item \code{value} (default): a user-define value given by \code{value}
#' \item \code{field}: values directly from a field in \code{vect} named by \code{field}
#' \item \code{category}: values according to which polygon is covered by a cell named in \code{field}
#' \item \code{z}: z-coordinate (points or contours only)
#' \item \code{direction}: flow direction (lines only)
#' }
#' @param field Name of column in \code{vect} with values or category labbels to which to burn to the raster.
#' @param value Numeric, value to burn to each cell overlapped by the spatial object in \code{vect}.
#' @param burn \code{NULL} or any of \code{'point'}, \code{'line'}, \code{'area'}, \code{'boundary'}, or \code{'centroid'}. This determines the manner in which the vector data is "burned" to the raster. If \code{NULL} (default), then points objects will rasterized as points, lines objects as lines, and polygons objects as areas. See \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/v.to.rast.html}{v.to.rast} for more details.
#' @param ... Arguments to send to \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/v.to.rast.html}{\code{v.to.rast}}.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, a raster with the name given by \code{outRastName} is written into the \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{rasterize}} in \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/v.to.rast.html}{\code{v.to.rast}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterRasterize.r
#'
#' @export

fasterRasterize <- function(
	vect,
	rast,
	inVectName,
	inRastName,
	use = 'val',
	value = 1,
	field = NULL,
	burn = NULL,
	outGrassName = 'vectToRast',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### begin common
	flags <- 'quiet'
	flags <- .getFlags(replace=replace, flags=flags)
	inRastName <- .getInRastName(inRastName, rast)
	if (is.null(inVectName)) inVectName <- 'vect'
	
	# region settings
	success <- .rememberRegion()
	# on.exit(.restoreRegion(inits), add=TRUE)
	on.exit(.restoreRegion(), add=TRUE)
	on.exit(regionResize(), add=TRUE)
	
	if (is.null(inits)) inits <- list()
	### end common
	
	if (!inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
	if (!inherits(vect, 'SpatVector')) vect <- terra::vect(vect)

	# feature type
	gtype <- terra::geomtype(vect)
	if (is.null(burn)) {
		
		burn <- if (gtype == 'polygons') {
			'point'
		} else if (gtype == 'lines') {
			'line'
		} else if (gtype == 'points') {
			'area'
		}

	} else {
	
		if (!(burn %in% c('point', 'line', 'area', 'boundary', 'centroid'))) {
			stop('Argument "burn" must be NULL or one of "point", "line", "area", "boundary",\nor "centroid" and match the type of argument "vect".')
		}
		
		if (burn %in% c('point', 'centroid') & !(gtype != 'points')) {
			stop('Argument "burn" must be either "point" or "centroid" if\nargument "vect" is a "points" vector.')
		}
		
	}
		
	# initialize GRASS
	inits <- c(inits, list(rast=rast, vect=vect, inRastName=inRastName, inVectName=inVectName, replace=replace, grassDir=grassDir))
	input <- do.call('initGrass', inits)

	# resize region to encompass all objects
	success <- regionResize()

	# rasterize
	if (use == 'field') {
		rgrass::execGRASS('v.to.rast', input=input[['vector']], output=outGrassName, use='attr', attribute_column=field, type=burn, flags=flags)
	} else if (use == 'category') {
		rgrass::execGRASS('v.to.rast', input=input[['vector']], output=outGrassName, use='cat', label_column=field, type=burn, flags=flags)
	} else if (use == 'value') {
		rgrass::execGRASS('v.to.rast', input=input[['vector']], output=outGrassName, use='val', value=value, type=burn, flags=flags)
	} else {
		rgrass::execGRASS('v.to.rast', input=input[['vector']], output=outGrassName, use=use, flags=flags, type=burn)
	}
	
	# get raster back to R
	if (grassToR) {
	
		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE)
		out
		
	}

}
