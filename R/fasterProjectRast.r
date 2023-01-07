#' Project and resample a raster
#'
#' Project and resample raster.
#'
#' @inheritParams .sharedArgs_rast_plural
#' @inheritParams .sharedArgs_inRastName_plural
#' @inheritParams .sharedArgs_grassDir_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @param template Either \code{NULL} (default) or a \code{SpatRaster} to serve as a template for projecting. If there is an existing \code{GRASS} session (started by another \pkg{fasterRaster} function or through the \code{\link{initGrass}} function), then this argument can be \code{NULL}, and the raster in \code{rast} will be projected to the coordinate reference system used by the current \code{GRASS} session.  However, if a \code{GRASS} session has not yet been started, or a different projection is desired, then this argument must be non-\code{NULL}.
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
#' @return If \code{grassToR} if \code{TRUE}, then a raster or raster stack with the same extent, resolution, and coordinate reference system as \code{rast}. Regardless, a raster with the name given by \code{outGrassName} is written into the \code{GRASS} session.
#'
#' @details Note that it is not uncommon to get the warning "Projection of dataset does not appear to match the current mapset" (followed by more information). If the coordinate reference systems match, then the cause is likely due to extra information being stored in one of the spatial object's coordinate reference system slot (e.g., an EPSG code in addition to the other proj4string information), in which case it can probably be safely ignored.
#'
#' @seealso \code{\link[terra]{project}} in \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/r.proj.html}{\code{r.proj}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterProject.r
#'
#' @export

fasterProjectRast <- function(
	rast,
	template = NULL,
	method = 'bilinear',
	grassDir = options()$grassDir,
	grassToR = TRUE,
	inRastName = NULL,
	outGrassName = NULL,
	...
) {

	flags <- c('quiet', 'overwrite')
	
	inRastName <- .getInRastName(inRastName, rast)
	if (is.null(outGrassName)) outGrassName <- inRastName

	# initialize two GRASS sessions
	fromGrass <- initGrass(rast=rast, vect=NULL, location='fromRast', inRastName=inRastName, inVectName=NULL, grassDir=grassDir)

	# create new GRASS location
	if (!is.null(template)) {
		tempDir <- attr(fromGrass, 'tempDir')
		initGrass(rast=template[[1L]], vect=NULL, location='default', inRastName='__TEMPTEMP_templateRast', inVectName=NULL, grassDir=grassDir)
	# use existing GRASS location
	} else {
		rgrass::execGRASS('g.mapset', flags=c('quiet'), mapset='PERMANENT', location = 'default')
	}

	# export raster to project to GRASS (projects it automatically)
	resol <- if (inherits(template, c('SpatRaster', 'Raster'))) {
		terra::res(template)[1L]
	} else {
		fasterRes()[1L]
	}
	
	for (i in 1L:nlyr(rast)) {
		rgrass::execGRASS('r.proj', location='fromRast', mapset='PERMANENT', input=inRastName[i], output=outGrassName[i], method=method, resolution=resol, flags=flags)
	}

	# return
	if (grassToR) {
	
		for (i in 1L:terra::nlyr(rast)) {
		
			this <- rgrass::read_RAST(outGrassName[i], flags='quiet')
			out <- if (exists('out', inherits=FALSE)) {
				c(out, this)
			} else {
				this
			}
			
		}
	
		names(out) <- outGrassName
		out
		
	}
	
}
