#' Convert a raster to a vector (points, lines, or polygons)
#'
#' Convert a raster to a spatial polygons object (points, lines, or polygons).\cr
#'
#' Conversion to lines may be aided by "thinning" the raster so cells are better able to represent linear features. This can be done using \code{\link{fasterThinRast}}. Conversion to lines can also yield artifacts, which can be managed using \code{\link{fasterReshapeVect}}.\cr
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param trunc If \code{TRUE} (default), round values in the raster first so that like-valued cells become part of the same line or polygon.
#'
#' @param z If \code{TRUE}, then rasters converted to a points vector have their values interpreted as the "z" (up/down) coordinate. This is only valid if \code{vectTopo} is \code{'points'}.
#'
#' @param buildTopo If \code{TRUE}, construct the vector topology. If the vector to be created is very large, then setting this to \code{FALSE} can significantly reduce processing time. However, if the vector is to be used for further operations in \code{GRASS}, then this should probably be \code{TRUE}. When topology is built, cells that share a border will have that border represented just once in the vector data, but if it is not built, it will be represented twice.
#'
#' @param values If \code{TRUE} (default), then create an attribute table for the vector.
#'
#' @param smooth Logical. If \code{TRUE} then when creating polygons, cell corners will be "rounded" by connecting the midpoints of corner cells (which leaves out the corner-most triangle of that cell). Default is \code{FALSE}.
#'
#' @param ... Arguments to pass to \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.to.vect.html}{\code{r.to.vect}}.
#'
#' @return If \code{grassToR} if \code{TRUE}, then \code{SpatVector} or \code{sf} object is returned. A field named \code{value} will have the raster values. Regardless, vector object with the name given by \code{outGrassName} will be written into the \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{as.points}}, \code{\link[terra]{as.polygons}}, and \code{\link[terra]{as.lines}} in \pkg{terra}; \code{\link[fasterRaster]{fasterRasterize}} in \code{fasterRaster}; \href{https://grass.osgeo.org/grass82/manuals/r.to.vect.html}{\code{r.to.vect}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterVectorizeRast.r
#'
#' @export
fasterAsPoints <- function(
	rast,
	inRastName,
	values = TRUE,
	buildTopo = TRUE,
	z = FALSE,
	outGrassName = 'rastToPoint',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### commons v1
	##############

		### arguments
		.checkRastExists(replace=replace, rast=NULL, inRastName=NULL, outGrassName=outGrassName, ...)
		if (!inherits(rast, 'character') & !inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
		inRastName <- .getInRastName(inRastName, rast=rast)
		.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=NULL, ...)

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterAsPoints')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### errors?
	if (z) flags <- c(flags, 'z')
	if (!buildTopo) flags <- c(flags, 'b')
	if (!values) flags <- c(flags, 't')

	### function-specific
	args <- list(
		cmd = 'r.to.vect',
		input = inRastName,
		output = outGrassName,
		type = 'point',
		column = inRastName,
		flags = flags,
		intern = TRUE
	)
	args <- c(args, dots)

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### execute
	if (autoRegion) regionReshape(inRastName)
	do.call(rgrass::execGRASS, args=args)

	# remove "label" column
	execGRASS('v.db.dropcolumn', map=outGrassName, columns='label', flags='quiet')

	# get raster back to R
	if (grassToR) {
	
		out <- fasterWriteVector(outGrassName, paste0(tempfile(), '.gpkg'), flags='quiet')
		if (outVectClass == 'sf') out <- sf::st_as_sf(out)
		out
		
	} else { invisible(TRUE) }
	
}

#' @name fasterAsLines
#' @title Convert a raster to a vector (points, lines, or polygons)
#' @rdname fasterAsPoints
#' @export
fasterAsLines <- function(
	rast,
	inRastName,
	trunc = TRUE,
	values = TRUE,
	thin = TRUE,
	buildTopo = TRUE,
	outGrassName = 'rastToLine',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### commons v1
	##############

		### arguments
		.checkRastExists(replace=replace, rast=NULL, inRastName=NULL, outGrassName=outGrassName, ...)
		if (!inherits(rast, 'character') & !inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
		inRastName <- .getInRastName(inRastName, rast=rast)
		.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=NULL, ...)

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterAsLines')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### errors?
	if (!buildTopo) flags <- c(flags, 'b')
	if (!values) flags <- c(flags, 't')

	### function-specific
	args <- list(
		cmd = 'r.to.vect',
		input = inRastName,
		output = outGrassName,
		type = 'line',
		column = inRastName,
		flags = flags,
		intern = TRUE
	)
	args <- c(args, dots)

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### truncate?
	if (trunc) {

		truncRast <- .makeTempName('truncRast')
		ex <- paste0(truncRast, ' = int(', inRastName, ')')
		execGRASS('r.mapcalc', expression=ex, flags='quiet', intern=TRUE)
		
	}
	
	args$input <- if (trunc) {
		truncRast
	} else {
		inRastName
	}

	### execute
	if (autoRegion) regionReshape(inRastName)
	do.call(rgrass::execGRASS, args=args)

	# remove "label" column
	execGRASS('v.db.dropcolumn', map=outGrassName, columns='label', flags='quiet')

	### export
	if (grassToR) {
	
		out <- fasterWriteVector(outGrassName, paste0(tempfile(), '.gpkg'), flags='quiet')
		if (outVectClass == 'sf') out <- sf::st_as_sf(out)
		out
		
	} else { invisible(TRUE) }
	
}

#' @name fasterAsPolygons
#' @title Convert a raster to a vector (points, lines, or polygons)
#' @rdname fasterAsPoints
#' @export
fasterAsPolygons <- function(
	rast,
	inRastName,
	trunc = TRUE,
	values = TRUE,

	smooth = FALSE,
	buildTopo = TRUE,
	outGrassName = 'rastToPoly',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### commons v1
	##############

		### arguments
		.checkRastExists(replace=replace, rast=NULL, inRastName=NULL, outGrassName=outGrassName, ...)
		if (!inherits(rast, 'character') & !inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
		inRastName <- .getInRastName(inRastName, rast=rast)
		.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=NULL, ...)

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterAsPolygons')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### errors?

	### function-specific
	if (!buildTopo) flags <- c(flags, 'b')
	if (!values) flags <- c(flags, 't')
	if (smooth) flags <- c(flags, 's')

	args <- list(
		cmd = 'r.to.vect',
		input = NA,
		output = outGrassName,
		type = 'area',
		column = inRastName,
		flags = flags,
		intern = TRUE
	)
	args <- c(args, dots)

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### truncate?
	if (trunc) {

		truncRast <- .makeTempName('truncRast')
		ex <- paste0(truncRast, ' = int(', inRastName, ')')
		execGRASS('r.mapcalc', expression=ex, flags='quiet', intern=TRUE)
		
	}
	
	args$input <- if (trunc) {
		truncRast
	} else {
		inRastName
	}

	### execute
	if (autoRegion) regionReshape(inRastName)
	do.call(rgrass::execGRASS, args=args)
	
	# remove "label" column
	execGRASS('v.db.dropcolumn', map=outGrassName, columns='label', flags='quiet')

	### export
	if (grassToR) {
	
		out <- fasterWriteVector(outGrassName, paste0(tempfile(), '.gpkg'), flags='quiet')
		if (outVectClass == 'sf') out <- sf::st_as_sf(out)
		out
		
	} else { invisible(TRUE) }
	
}

