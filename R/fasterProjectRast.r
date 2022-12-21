#' Project and resample a raster
#'
#' Project and resample raster.
#'
#' @inheritParams .sharedArgs_rast
#' @inheritParams .sharedArgs_inRastName
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @param template Either a \code{SpatRaster} or the name of a raster in an existing \code{GRASS} session to serve as a template for projecting.
#' @param method Character, method for resampling cells:
#' \itemize{
#' 		\item \code{nearest}: Nearest neighbor (uses value from 1 cell).
#' 		\item \code{bilinear}: Bilinear interpolation (default; uses values from 4 cells).
#' 		\item \code{bilinear_f}: Bilinear interpolation with fallback.
#' 		\item \code{bicubic}: Bicubic interpolation (uses values from 16 cells).
#' 		\item \code{bicubic_f}: Bicubic interpolation with fallback.
#' 		\item \code{lanczos}: Lanczos interpolation (uses values from 25 cells).
#' 		\item \code{lanczos_f}: Lanczos interpolation with fallback.
#' }
#'
#' @param ... Arguments to pass to \code{\link[rgrass]{execGRASS}} when used for rasterizing (i.e., function \code{r.proj} in \code{GRASS}).
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, a raster with the name given by \code{outGrassName} is written into the \code{GRASS} session.
#' @details Note that it is not uncommon to get the warning "Projection of dataset does not appear to match the current mapset" (followed by more information). If the coordinate reference systems match, then the cause is likely due to extra information being stored in one of the spatial object's coordinate reference system slot (e.g., an EPSG code in addition to the other proj4string information), in which case it can probably be safely ignored.
#'
#'
#' @seealso \code{\link[terra]{project}} in \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/r.proj.html}{\code{r.proj}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterProjectRast.r
#'
#' @export

fasterProjectRast <- function(
	rast,
	template,
	method = 'bilinear',
	grassDir = options()$grassDir,
	grassToR = TRUE,
	inRastName = ifelse(is.null(names(rast)), 'rast', names(rast)),
	outGrassName = 'projectedRast',
	...
) {

	flags <- c('quiet', 'overwrite')
	
	# initialize GRASS
	fromRastGrass <- initGrass(rast=rast, vect=NULL, location='fromRast', inRastName=inRastName, inVectName=NULL, grassDir=grassDir)
	tempDir <- attr(fromRastGrass, 'tempDir')
	toRastGrass <- initGrass(rast=rast, vect=NULL, location='default', inRastName=inRastName, inVectName=NULL, grassDir=grassDir)

	
	# export raster to project to \code{GRASS} (projects it automatically)
	resol <- terra::res(template)[1L]
	rgrass::execGRASS('r.proj', location='fromRast', mapset='PERMANENT', input='rast', output=outGrassName, method=method, resolution=resol, flags=flags)

	# return
	if (grassToR) {
	
		out <- rgrass::read_RAST(outGrassName)
		names(out) <- outGrassName
		out
		
	}
	
}
