#' Project a spatial vector
#'
#' Project a spatial vector.
#'
#' @inheritParams .sharedArgs_vect
#' @inheritParams .sharedArgs_inVectName
#' @inheritParams .sharedArgs_outVectClass
#' @inheritParams .sharedArgs_replace
#' @inheritParams .sharedArgs_grassDir
#' @inheritParams .sharedArgs_grassToR
#' @inheritParams .sharedArgs_outGrassName
#' @inheritParams .sharedArgs_dots_forInitGrass_andGrassModule
#'
#' @param template Either \code{NULL} (default), \emph{or} a \code{SpatRaster}, \code{SpatVector}, or object of class \code{sf} to serve as a template for projecting. If there is an existing \code{GRASS} session (started by another \pkg{fasterRaster} function or through the \code{\link{initGrass}} function), then this can be \code{NULL}, and the vector in \code{vect} will be projected to the one used by the current \code{GRASS} session.  However, if a \code{GRASS} session has not yet been started, or a different projection is desired, then this argument must be non-\code{NULL}.
#' @param faster If \code{TRUE}, then do not build vector topology in \code{GRASS}. This can speed up projections for very large vectors, and is useful if the vector is not going to be used further in \code{GRASS}. If \code{FALSE} (default), then topology is built.
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
#' @seealso \code{\link{fasterProjectRast}} and \code{\link{fasterCRS}} in \pkg{fasterRaster}; \code{\link[terra]{project}} in \pkg{terra}; \href{https://grass.osgeo.org/grass82/manuals/v.proj.html}{\code{v.proj}} in \code{GRASS}
#'
#' @example man/examples/ex_fasterProject.r
#'
#' @export

fasterProjectVect <- function(
	vect,
	inVectName,
	template = NULL,
	faster = FALSE,
	outGrassName = 'projectedVect',
	
	replace = fasterGetOptions('replace', FALSE),
	grassToR = fasterGetOptions('grassToR', TRUE),
	outVectClass = fasterGetOptions('outVectClass', 'SpatVector'),
	autoRegion = fasterGetOptions('autoRegion', TRUE),
	grassDir = fasterGetOptions('grassDir', NULL),
	...
) {

	### begin common
	flags <- .getFlags(replace=replace)
	# inRastName <- .getInRastName(inRastName, rast)
	if (is.null(inVectName)) inVectName <- 'vect'
	
	# region settings
	success <- .rememberRegion()
	on.exit(.restoreRegion(inits), add=TRUE)
	on.exit(.revertRegion(), add=TRUE)
	on.exit(regionResize(), add=TRUE)
	
	if (is.null(inits)) inits <- list()
	### end common

	if (is.null(outGrassName)) outGrassName <- inVectName

	# initialize two GRASS sessions
	fromGrass <- initGrass(rast=NULL, vect=vect, location='fromVect', inRastName=NULL, inVectName=inVectName, replace=replace, grassDir=grassDir)

	fromVectLoc <- paste0('fromVect', round(1E9 * runif(1)))

	if (is.null(inits)) inits <- list()
	from <- c(inits, list(rast=NULL, vect=vect, inRastName=NULL, inVectName=inVectName, replace=replace, grassDir=grassDir, location=fromVectLoc))
	input <- do.call('initGrass', inits)

	# create new GRASS location
	if (!is.null(template)) {
		
		if (inherits(template, c('SpatRaster', 'Raster'))) {

			inits$rast <- template
			inits$vect <- NULL
			inits$inRastName <- 'TEMPTEMP_templateRast'
			inits$inVectName <- NULL
			inits$location <- 'default'

		} else if (inherits(template, c('Spatvector', 'sf', 'Spatial'))) {

			inits$rast <- NULL
			inits$vect <- template
			inits$inRastName <- NULL
			inits$inVectName <- 'TEMPTEMP_templateVect'
		}

		inits$location <- 'default'
		input <- do.call('initGrass', inits)
		
	# use existing GRASS location
	} else {
		rgrass::execGRASS('g.mapset', flags=c('quiet'), mapset='PERMANENT', location='default')
	}

	
	# export raster to project to GRASS (projects it automatically)
	resol <- if (inherits(template, c('SpatRaster', 'Raster'))) {
		terra::res(template)[1L]
	} else {
		fasterRes()[1L]
	}

	if (faster) flags <- c(thisFlags, 'b')
	rgrass::execGRASS('v.proj', location='fromVect', mapset='PERMANENT', input=inVectName, output=outGrassName, flags=flags)

	# return
	if (grassToR) {
	
		out <- rgrass::read_VECT(outGrassName, flags='quiet')
		if (.getOutVectClass(outVectClass) == 'sf') {
			out <- sf::st_as_sf(out)
		}
		out
		
	}
	
}
