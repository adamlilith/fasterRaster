#' Distance between cells with NAs and closest non-NA cells on a raster
#'
#' This function is a potentially faster version of the \code{\link[terra]{distance}} function in the \pkg{terra} package which it replaces values in all \code{NA} cells with the distance between them and the closest non-\code{NA} cell. Alternatively, it fills in values of non-\code{NA} cells with the distance between them and the closest \code{NA} cell. Note that the \code{distance} function also calculates distances between a raster and a spatial vector object, but this functionality is reproduced in \code{\link[fasterRaster]{fasterVectToRastDistance}}.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
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
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when used for rasterizing (i.e., function \code{r.grow.distance} in \code{GRASS}).
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent and coordinate reference system as \code{vect}. Regardless, a raster with the name given by \code{outGrassName} is written into the \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{distance}} in \pkg{terra}; \code{\link[fasterRaster]{fasterVectToRastDistance}} in \pkg{fasterRaster}; \href{https://grass.osgeo.org/grass82/manuals/r.grow.distance.html}{\code{r.grow.distance}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterRastDistance.r
#'
#' @export

fasterRastDistance <- function(
	rast,
	metric = 'euclidean',
	meters = TRUE,
	fillNAs = TRUE,
	grassDir = options()$grassDir,
	grassToR = TRUE,
	inRastName = 'rast',
	outGrassName = 'distanceRast',
	...
) {

	flags <- c('quiet', 'overwrite')
	if (meters) flags <- c(flags, 'm')
	if (!fillNAs) flags <- c(flags, 'n')
	
	# initialize GRASS
	inRastName <- .getInRastName(inRastName, rast)
	input <- initGrass(rast=rast, vect=NULL, inRastName=inRastName, inVectName=NULL, grassDir=grassDir)
	
	# calculate distance
	rgrass::execGRASS('r.grow.distance', input='rast', distance=outGrassName, metric=metric, flags=flags)

	# return
	if (grassToR) {
	
		out <- rgrass::read_RAST(outGrassName)
		out <- terra::rast(out)
		names(out) <- outGrassName
		out
		
	}
	
}
