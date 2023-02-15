#' Distance between raster cells and nearest spatial vector feature
#'
#' Calculate the distance between each cell in a raster and the nearest feature in a spatial points, lines, or polygon object. Alternatively, calculate the distance from any cell covered by a vector object and the nearest cell \emph{not} covered by a vector object.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_vect
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_trimRast
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param metric Character, indicates type of distance to calculate:
#' \itemize{
#' \item \code{euclidean}: Euclidean distance
#' \item \code{geodesic} (default): geographic distance (suggested to use with \code{meters = TRUE}).
#' \item \code{squared}: Squared Euclidean distance
#' \item \code{maximum}: Maximum Euclidean distance
#' \item \code{manhattan}: Manhattan distance (i.e., "taxicab" distance, distance along cells going only north-south and east-west and never along a diagonal).
#' } 
#' @param meters Logical, if \code{TRUE} then distance is in meters. If \code{FALSE} then distance is in map units.
#' @param invert Logical, if \code{TRUE} then for cells covered by a vector object calculate distance to nearest cell \emph{not} covered by a vector object.
#' @param ... Arguments to pass to \code{GRASS} module \href{https://grass.osgeo.org/grass82/manuals/r.grow.distance.html}{\code{r.grow.distance}}.
#'
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the same extent, resolution, and coordinate reference system as \code{vect}. Regardless, a raster with the name of \code{distToVect} is written into the \code{GRASS} session.
#'
#' @seealso \code{\link[terra]{distance}} in \pkg{terra}; \code{\link{fasterRastDistance}} in \pkg{fasterRaster}; \href{https://grass.osgeo.org/grass82/manuals/r.grow.distance.html}{\code{r.grow.distance}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterVectToRastDistance.r
#'
#' @export

fasterVectToRastDistance <- function(
	rast,
	vect,
	inRastName,
	inVectName,
	metric = 'euclidean',
	meters = TRUE,
	invert = FALSE,	
	outGrassName = 'distToVectRast',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	trimRast = fasterGetOptions('trimRast', TRUE),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	flags <- .getFlags(replace=replace)
	flags_vToRast <- flags_rGrowDistance <- flags
	if (meters) flags_rGrowDistance <- c(flags_rGrowDistance, 'm')
	if (invert) flags_rGrowDistance <- c(flags_rGrowDistance)
	
	# rasterize vector: creates raster named "distToVect"
	fasterRasterize(vect=vect, rast=rast, use='value', value=1, grassDir=grassDir, grassToR=FALSE, inRastName=inRastName, inVectName=inVectName, replace=replace, outGrassName='vectToRast', inits=inits, ...)
	
	# calculate distance
	rgrass::execGRASS('r.grow.distance', input='vectToRast', distance=outGrassName, metric=metric, flags=flags_rGrowDistance, ...)

	# return
	if (grassToR) {
	
		out <- fasterWriteRaster(outGrassName, paste0(tempfile(), '.tif'), overwrite=TRUE, trimRast=trimRast)
		out
		
	} else { invisible(TRUE) }

}
