#' Change or report the extent and dimensions/resolution of a region
#'
#' This function simultaneously changes the extent and dimensions of a **GRASS** "region". This function is mostly used internally and rarely of direct use to most users.
#'
#' @param x Any of:
#'	* Missing (default): Reports the extent, resolution, and dimensions of the current region. All other arguments will be ignored.
#'	* A `GRaster`, `SpatRaster`, or `stars` raster: Sets the region's extent and dimensions to those of the raster.
#'
#' @param trimTo: A `GRaster` or `NULL` (default). If a `GRaster`, then the region will be trimmed to the non-`NA` cells in this raster. `trimTo` can only be non-`NULL` if `x` is a `GRaster`. Ignored if `NULL`.
#'
#' @return Either a list, or `TRUE` (invisibly) if resizing and resampling was successful. Also resizes and resamples the region in the active **GRASS** session.
#'
#' @seealso [regionExt()], [regionDim()], and [regionRes()] in **fasterRaster**; [terra::ext()], [terra::dim()], and [terra::res()]; [sf::st_bbox()]; **GRASS** module [g.region][https://grass.osgeo.org/grass82/manuals/g.region.html]
#'
#' @example man/examples/examples_regions.r
#'
#' @export

regionReshape <- function(x, trimTo = NULL) {

	# just print information about current region
	if (missing(x)) {
	
		rgrass::execGRASS('g.region', flags=c('p', '3', 'u'))
	
	} else if (inherits(x, c('SpatRaster', 'stars'))) {
	
		if (!is.null(trimTo)) stop('Cannot use <trimTo> if <x> is a SpatRaster.')
	
		extent <- terra::ext(x)@ptr$vector
		dims <- dim(x)
		
		w <- as.character(extent[1L])
		e <- as.character(extent[2L])
		s <- as.character(extent[3L])
		n <- as.character(extent[4L])
	
		rows <- dims[1L]
		cols <- dims[2L]
		
		rgrass::execGRASS('g.region', n=n, s=s, e=e, w=w, rows=rows, cols=cols, flags=c('o', 'quiet'))
	
	} else if (inherits(x, 'GRaster')) {
	
		topo <- topology(x)
		gname <- .gname(x)

		if (any(topo %in% '2D') & any(topo %in% '3D')) stop('Cannot mix 2D- and 3D-rasters when defining region.')

		if (!is.null(trimTo)) {
			trimToTopo <- topology(trimTo)
			if (any(!(topo %in% trimToTopo))) stop('Tropology of <trimTo> does not match topology of <x>.')
			
			trimTo <- .gname(trimTo)
			if (length(trimTo) != 1L) stop('Argument <trimTo> can have only one layer.')
		}

		if (all(topo == '2D') & is.null(trimTo)) {
			rgrass::execGRASS('g.region', raster=gname, flags=c('o', 'quiet'))
		} else if (all(topo == '2D') & !is.null(trimTo)) {
			rgrass::execGRASS('g.region', raster=gname, zoom=trimTo, flags=c('o', 'quiet'))
		} else if (all(topo == '3D') & is.null(trimTo)) {
			rgrass::execGRASS('g.region', raster_3d=gname, flags=c('o', 'quiet'))
		} else if (all(topo == '3D') & !is.null(trimTo)) {
			rgrass::execGRASS('g.region', raster_3d=gname, zoom=trimTo, flags=c('o', 'quiet'))
		} else {
			stop('Could not reshape region.')
		}
	
		invisible(TRUE)
	
	}

}
