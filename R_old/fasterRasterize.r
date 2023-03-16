#' Convert a spatial vector to a raster
#'
#' This function converts a spatial points, lines, or polygon vector into a raster based on a "template" raster. All cells covered by the vector can either have values taken from the vector or be set to a user-defined value.
#'
#' @inheritParams .sharedArgs_vect
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_memory
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param use Character, indicates the types of values to be "burned" to the raster. Options include
#' \itemize{
#' \item \code{'value'} (default): A user-define value given by \code{value}
#' \item \code{'field'}: Values directly from a field in \code{vect} named by \code{field}
#' \item \code{'category'}: Each cell assigned an integer based on which polygon covers it.
#' \item \code{'z'}: Z-coordinate (points or contours only) UNTESTED!
#' \item \code{'direction'}: Flow direction (lines only) UNTESTED!
#' }
#' Partial matching is used.
#' @param field Name of column in \code{vect} with values or category labels to which to burn to the raster.
#' @param value Numeric, value to burn to each cell overlapped by the spatial object in \code{vect}.
#' @param thick If \code{FALSE} (default) and lines are being rasterized, then only cells on the actual path of the line will be rasterized. If \code{TRUE}, then any cell that touches the line will be included.
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
	use = 'value',
	value = 1,
	field = NULL,
	# burn = NULL,
	thick = FALSE,
	outGrassName = 'vectToRast',
	
	memory = fasterGetOptions('memory', 300),
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### commons v1
	##############

		### arguments
		.checkRastExists(replace=replace, rast=NULL, inRastName=NULL, outGrassName=outGrassName, ...)
		if (!missing(rast)) {
			if (!inherits(rast, 'character') & !inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
			inRastName <- .getInRastName(inRastName, rast=rast)
			.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=NULL, ...)
		} else {
			rast <- inRastName <- NULL
		}

		if (!missing(vect)) {
			if (!inherits(vect, 'character') & !inherits(vect, 'SpatVector')) vect <- terra::vect(vect)
			inVectName <- .getInVectName(inVectName, vect=vect)
			.checkVectExists(replace=replace, vect=vect, inVectName=inVectName, outGrassName=NULL, ...)
		} else {
			vect <- inVectName <- NULL
		}

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterRasterize')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons
	
	### function-specific

	# feature type
	if (inherits(vect, 'SpatVector')) {
		
		gtype <- terra::geomtype(vect)
		type <- if (gtype == 'polygons') {
			'area'
		} else if (gtype == 'lines') {
			'line'
		} else if (gtype == 'points') {
			'point'
		}
		
	} else {
		type <- vectTopo(vect)
	}

	# if (is.null(burn)) {
		
		# # burn <- if (gtype == 'polygons') {
			# # 'point'
		# # } else if (gtype == 'lines') {
			# # 'line'
		# # } else if (gtype == 'points') {
			# # 'area'
		# # }

	# } else {
	
		# if (!(burn %in% c('point', 'line', 'area', 'boundary', 'centroid'))) {
			# stop('Argument "burn" must be NULL or one of "point", "line", "area", "boundary",\nor "centroid" and match the type of argument "vect".')
		# }
		
		# if (burn %in% c('point', 'centroid') & !(gtype != 'points')) {
			# stop('Argument "burn" must be either "point" or "centroid" if\nargument "vect" is a "points" vector.')
		# }
		
	# }

	# construct arguments
	args <- list(
		cmd = 'v.to.rast',
		intern = TRUE,
		flags = flags,
		input = inVectName,
		output = outGrassName,
		type = type,
		memory = memory
	)
	args <- c(args, dots)
	
	if ('line' %in% type & thick) args$flags <- c(flags, 'd')

	# different kinds of rasterization
	opts <- c('field', 'category', 'value')
	use <- pmatch(use, opts)
	use <- if (is.na(use)) {
		use
	} else {
		opts[use]
	}
	
	if (use == 'field') {
		args$use <- 'attr'
		args$attribute_column <- field
	} else if (use == 'category') {
		args$use <- 'cat'
		args$label_column <- field
	} else if (use == 'value') {
		args$use <- 'val'
		args$value <- value
	} else {
		args$use <- 'val'
	}

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### execute
	if (autoRegion) regionReshape(inRastName)
	do.call(rgrass::execGRASS, args=args)
	
	# get raster back to R
	if (grassToR) {
	
		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		out
		
	} else { invisible(TRUE) }

}
