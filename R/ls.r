#' Names of all rasters and/or spatial vectors in the active 'GRASS' location
#'
#' Displays the names of all rasters and/or vectors that have been exported to or created in the active 'GRASS' session's location.
#'
#' @param rastOrVect The type of spatial objects to display. This can include `'rasters'` (all rasters) and/or `'vectors'` (all spatial vectors). Partial matching is supported.
#' @param ... Other arguments (not used).
#' 
#' @return Nothing (displays names of rasters and/or vectors).
#'
#' @seealso [ls()]; **GRASS** module [https://grass.osgeo.org/grass82/manuals/g.list.html](`g.list`)
#'
#' @example man/examples/ex_fastLs.r
#'
#' @keywords internal

.ls <- function(
	rastOrVect = c('rasters', 'vectors', 'rasters3d')
) {

	rov <- c('rasters', 'vectors', 'rasters3d')
	if (is.null(rastOrVect)) rastOrVect <- rov

	# find rasters and vector
	rasts <- vects <- rasts3d <- vects3d <- character()
	match <- pmatch(rastOrVect, rov)
	
	# failure to match
	if (anyNA(match)) {

		warning('Cannot discern whether to display rasters or vectors.', immediate.=TRUE)
		out <- NULL
		
	# specific match
	} else {
		
		if (any(match == 1L)) {
			rasts <- rgrass::execGRASS('g.list', flags='quiet', type='raster', intern=TRUE, echoCmd=FALSE)
			if (length(rasts) > 0L) names(rasts) <- rep('raster', length(rasts))
			rasts <- sort(rasts)
		}
		
		if (any(match == 2L)) {
			vects <- rgrass::execGRASS('g.list', flags='quiet', type='vector', intern=TRUE, echoCmd=FALSE)
			if (length(vects) > 0L) names(vects) <- rep('vector', length(vects))
			vects <- sort(vects)
		}
		
		if (any(match == 3L)) {
			rasts3d <- rgrass::execGRASS('g.list', flags='quiet', type='raster_3d', intern=TRUE, echoCmd=FALSE)
			if (length(rasts3d) > 0L) names(rasts3d) <- rep('raster3d', length(rasts3d))
			rasts3d <- sort(rasts3d)
		}
		
		out <- c(rasts, rasts3d, vects)

	}
		
	out
	
}
