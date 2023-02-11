#' Distance between cells with 'NA's and closest non-'NA' cells on a raster
#'
#' By default, this function replaces values in all \code{NA} cells with the distance between them and their closest non-\code{NA} cell. The "target" cells that have their values replaced can be changed by the user, and a second set of cells can optionally be excluded from calculations. Also optionally, a "nearest-value" raster can be calculated which replaces target cells with the value of the nearest non-\code{NA} cell. The results are comparable to but not guaranteed to be the same as \pkg{terra}'s \code{\link[terra]{distance}} function.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param filNA If \code{TRUE} (default), then all cells with \code{NA} will have their values replaced by the distance to the nearest non-\code{NA} cells. If \code{FALSE}, then all non-\code{NA} cells will have their values replaced by distance to the nearest \code{NA} cells.
#'
#' @param distance If \code{TRUE} (default), calculate a distance raster. If \code{fillNA} is \code{TRUE} (default), then cells that are \code{NA} in the input raster will have their values replaced by the distance to the nearest non-\code{NA} cell, and cells that are not \code{NA} will have their values set to 0. If \code{fillNA} is \code{FALSE}, then non-\code{NA} cells will have their values replaced by the distance to the nearest \code{NA} cell, and the value of \code{NA} cells will be set to zero.
#'
#' @param nearestValue If \code{TRUE} and \code{fillNA} is \code{TRUE}, then create a raster where values of \code{NA} cells are replaced by the values of the nearest non-\code{NA} cells. If \code{fillNA} is \code{FALSE}, then a "nearest-value" raster cannot be creared because this would replace non-\code{NA} cells with the value of the nearest \code{NA} cell (whis is just \code{NA}). If \code{nearestValue} is \code{FALSE}
# 
#' @param metric Character, indicates type of distance to calculate:
#' \itemize{
#' \item \code{euclidean} (default): Euclidean distance
#' \item \code{geodesic}: Geographic distance (suggested to use with \code{meters = TRUE}).
#' \item \code{squared}: Squared Euclidean distance
#' \item \code{maximum}: Maximum Euclidean distance
#' \item \code{manhattan}: Manhattan distance (i.e., "taxicab" distance, distance along cells going only north-south and east-west and never along a diagonal).
#' }
#' Partial matching is used and capitalization is ignored.
#'
#' @param meters Logical, if \code{TRUE} then distance is in meters. If \code{FALSE} then distance is in map units.
#'
#' @param minDist,maxDist Either \code{NULL} or numeric values. Cells with distance values smaller/larger than these limits have their values set to \code{NA}.
#'
#' @param outDistName Name of the distance raster to be produced in \code{GRASS}.
#'
#' @param outValueName Name of the nearest-value raster to be produced in \code{GRASS}.
#'
#' @return If \code{grassToR} is \code{TRUE}, then a single- or two-later raster is returned to \code{R}. Regardless, one or two rasters with the name given by \code{outDistName} and/or \code{outValueName} is written into the \code{GRASS} session.
#'
#' @seealso \code{\link[fasterRaster]{fasterVectToRastDistance}} in \pkg{fasterRaster}; \code{\link[terra]{distance}} in \pkg{terra}; \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.grow.distance.html}{\code{r.grow.distance}}
#'
#' @example man/examples/ex_fasterRastDistance.r
#'
#' @export

fasterDistanceRast <- function(
	rast,
	inRastName,
	fillNA = TRUE,
	distance = TRUE,
	nearestValue = FALSE,
	metric = 'euclidean',
	meters = TRUE,
	minDist = NULL,
	maxDist = NULL,
	outDistName = 'distanceRast',
	outValueName = 'nearestValueRast',
	
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
		if (distance) .checkRastExists(replace=replace, rast=NULL, inRastName=NULL, outGrassName=outDistName, ...)
		if (nearestValue) .checkRastExists(replace=replace, rast=NULL, inRastName=NULL, outGrassName=outValueName, ...)
		if (!missing(rast)) {
			if (!inherits(rast, 'character') & !inherits(rast, 'SpatRaster')) rast <- terra::rast(rast)
			inRastName <- .getInRastName(inRastName, rast=rast)
			.checkRastExists(replace=replace, rast=rast, inRastName=inRastName, outGrassName=NULL, ...)
		} else {
			rast <- inRastName <- NULL
		}

		### flags
		flags <- .getFlags(replace=replace)
		
		### restore
		# on.exit(.restoreLocation(), add=TRUE) # return to starting location
		if (autoRegion) on.exit(regionExt('*'), add=TRUE) # resize extent to encompass all spatials

		### ellipses and initialization arguments
		initsDots <- .getInitsDots(..., callingFx = 'fasterDistanceRast')
		inits <- initsDots$inits
		dots <- initsDots$dots

	###############
	### end commons

	### errors?
	if (!distance & !nearestValue) stop('Either or both of "distance" and "nearestValue" must be TRUE.')
	
	if (!is.null(minDist) && minDist < 0) stop('Argument "minDist" must be positive or NULL.')
	if (!is.null(maxDist) && maxDist < 0) stop('Argument "maxDist" must be positive or NULL.')
	if ((!is.null(maxDist) & !is.null(maxDist)) && (minDist > maxDist)) stop('Argument "minDist" is greater than "maxDist".')
	
	if (!fillNA & nearestValue) stop('Cannot calculate "nearestValue" when "fillNA" is FALSE.\n  The value of the nearest NA-cell is uninteresting.')
	
	### function-specific 
	if (!fillNA) flags <- c(flags, 'n')
	if (meters) flags <- c(flags, 'm')

	# metric
	metrics <- c('euclidean', 'squared', 'maximum', 'manhattan', 'geodesic')
	metric <- pmatch(tolower(metric), metrics)
	if (is.na(metric)) stop('Argument "metric" is invalid.')
	metric <- metrics[metric]

	# arguments
	args <- list(
		cmd = 'r.grow.distance',
		input = inRastName,
		metric = metric,
		flags = flags,
		intern = TRUE
	)
	args <- c(args, dots)

	if (!is.null(minDist)) args$minimum_distance <- minDist
	if (!is.null(maxDist)) args$maximum_distance <- maxDist

	if (distance) args$distance <- outDistName
	if (nearestValue) args$value <- outValueName

	### initialize GRASS
	input <- do.call('startFaster', inits)

	### region management
	if (autoRegion) regionReshape(inRastName)

	### distance/value calculations
	do.call(rgrass::execGRASS, args=args)

	# return
	if (grassToR) {

		out <- fasterWriteRaster(outDistName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)

		if (nearestValue) {
			nearestVal <- fasterWriteRaster(outValueName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
			out <- c(out, nearestVal)
			}
		out

	} else { invisible(TRUE) }
	
}
