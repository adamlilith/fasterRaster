#' Distance between cells with NAs and closest non-NA cells on a raster
#'
#' This function replaces values in all \code{NA} cells with the distance between them and the closest non-\code{NA} cell. Alternatively, it fills in values of non-\code{NA} cells with the distance between them and the closest \code{NA} cell.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param metric Character, indicates type of distance to calculate:
#' \itemize{
#' \item \code{euclidean} (default): Euclidean distance
#' \item \code{geodesic}: geographic distance (suggested to use with \code{meters = TRUE}).
#' \item \code{squared}: Squared Euclidean distance
#' \item \code{maximum}: Maximum Euclidean distance
#' \item \code{manhattan}: Manhattan distance (i.e., "taxicab" distance, distance along cells going only north-south and east-west and never along a diagonal).
#' }
#' @param meters Logical, if \code{TRUE} then distance is in meters. If \code{FALSE} then distance is in map units.
#' @param fillNAs Logical, if \code{TRUE} (default) then fill code{NA} cells with distance between them and closest non-\code{NA}. If \code{TRUE} then replace value in non-{NA} cells with distance between them and nearest \code{NA} cell.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent and coordinate reference system as \code{vect}. Regardless, a raster with the name given by \code{outGrassName} is written into the \code{GRASS} session.
#'
#' @seealso \code{\link[fasterRaster]{fasterVectToRastDistance}} in \pkg{fasterRaster}; \code{\link[terra]{distance}} in \pkg{terra}; \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.grow.distance.html}{\code{r.grow.distance}}
#'
#' @example man/examples/ex_fasterRastDistance.r
#'
#' @export

fasterRastDistance <- function(
	rast,
	inRastName,
	metric = 'euclidean',
	meters = TRUE,
	fillNAs = TRUE,
	outGrassName = 'distanceRast',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### begin common
	flags <- 'quiet'
	flags <- .getFlags(replace=replace, flags=flags)
	inRastName <- .getInRastName(inRastName, rast)
	# if (is.null(inVectName)) inVectName <- 'vect'
	
	# region settings
	success <- .rememberRegion()
	# on.exit(.restoreRegion(inits), add=TRUE)
	# on.exit(.restoreRegion(), add=TRUE)
	# on.exit(regionResize(), add=TRUE)
	
	if (is.null(inits)) inits <- list()
	### end common

	if (meters) flags <- c(flags, 'm')
	if (!fillNAs) flags <- c(flags, 'n')
	
	# initialize GRASS
	if (is.null(inits)) inits <- list()
	inits <- c(inits, list(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, replace=replace, grassDir=grassDir))
	input <- do.call('startFaster', inits)
	
	# calculate distance
	rgrass::execGRASS('r.grow.distance', input='rast', distance=outGrassName, metric=metric, flags=flags)

	# return
	if (grassToR) {
	
		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		out
		
	} else { invisible(TRUE) }
	
}
