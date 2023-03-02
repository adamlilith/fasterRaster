#' Get or set the spatial extent of the active 'GRASS' region
#'
#' This function either reports the spatial extent of a **GRASS** "region" or sets a new extent for the region.
#'
#' @param x Any of:
#'	* Missing (default) or `NULL`: Reports the extent of the current region. Also see argument \code{names}.
#'	* A `SpatRaster`, `SpatVector`, `SpatExtent`, or `sf` object: Sets the region's extent to the extent of the object. Note that this does not export the object to the **GRASS** session.
#'	* A vector of four numbers representing the longitude and latitude of the new extent: These must be listed in this order: western longitude, eastern longitude, southern latitude, northern latitude.
#'	* The name of one or more rasters or vectors in the active **GRASS** session: Resizes the extent to exactly encompass all objects.
#'
#' @param rastOrVect Either `'raster'` and/or `'vector'` (one value per value in `x`). If `NULL` (default), then the function will attempt to guess whether `x` refers to a raster or vector. However, in **GRASS**, it is possible to have a raster and a vector of the same name. If this is the case, then you can specify whether `x` is a raster or vector (partial matching is supported).
#' @param names If `TRUE` (default), then the returned vector will have names. Ignored if `x` is non-`NULL`.
#' @param terra If `TRUE`, then the returned object will be a \code{\link[terra]{SpatExtent}}. Ignored if `x` is non-`NULL`.
#'
#' @return Either a numeric vector with four values, or `TRUE` (invisibly) if resizing was successful. Also resizes the extent of the "region" in the active **GRASS** session.
#'
#' @seealso [regionDim()], \code{\link{regionRes}}, and [regionReshape()] in **fasterRaster**; [terra::ext()]; [sf::st_bbox()]; **GRASS** module [https://grass.osgeo.org/grass82/manuals/g.region.html](g.region)
#'
#' @example man/examples/example_regions.r
#'
#' @export

regionExt <- function(
	x,
	rastOrVect = NULL,
	names = TRUE,
	terra = FALSE,
	...
) {

	if (!.getSessionStarted()) {
	
		out <- invisible(FALSE)
		
	} else {

		spatials <- fasterLs(rastOrVect = c('raster', 'vector'), ...)

		# report region
		if (length(spatials) == 0L || missing(x) || is.null(x)) {

			if (length(spatials) == 0L) warning('No objects in GRASS session. Reporting region dimensions instead.', immediate.=TRUE)

			info <- rgrass::execGRASS('g.region', flags=c('g', 'u'), intern=TRUE)

			n <- info[substr(info, 1, 2) == 'n=']
			s <- info[substr(info, 1, 2) == 's=']
			e <- info[substr(info, 1, 2) == 'e=']
			w <- info[substr(info, 1, 2) == 'w=']

			n <- sub(n, pattern='n=', replacement='')
			s <- sub(s, pattern='s=', replacement='')
			e <- sub(e, pattern='e=', replacement='')
			w <- sub(w, pattern='w=', replacement='')

			n <- trimws(n)
			s <- trimws(s)
			e <- trimws(e)
			w <- trimws(w)

			n <- as.numeric(n)
			s <- as.numeric(s)
			e <- as.numeric(e)
			w <- as.numeric(w)

			out <- c(w, e, s, n)
			if (names ) names(out) <- c('xmin', 'xman', 'ymin', 'ymax')
			if (terra) out <- terra::ext(out)
			out

		# define region
		} else {
		
			if (inherits(x, c('SpatRaster', 'SpatVector', 'SpatExtent', 'sf'))) {
				extent <- terra::ext(x)@ptr$vector
			} else if (inherits(x, 'character')) {

				# reshape to everything
				if (length(x) == 1L) {

					info <- if (x == '*') {
						fasterInfo(rastOrVect=c('rasters', 'vectors'), ...)
					} else if (x == '*rasters*') {
						fasterInfo(rastOrVect='rasters', ...)
					} else if (x == '*vectors*') {
						fasterInfo(rastOrVect='vectors', ...)
					} else {
						fasterInfo(x, rastOrVect=rastOrVect, ...)
					}

				} else {
					info <- fasterInfo(x, rastOrVect=rastOrVect, ...)
				}
				
				extent <- c(
					min(info$west),
					max(info$east),
					min(info$south),
					max(info$north)
				)
				
			} else if (inherits(x, 'numeric')) {

				if (length(x) == 4L) {
					extent <- x
				} else {
					stop('Invalid value for argument "x".')
				}

			} else {
				stop('Invalid value for argument "x".')
			}
		
			w <- as.character(extent[1L])
			e <- as.character(extent[2L])
			s <- as.character(extent[3L])
			n <- as.character(extent[4L])

			rgrass::execGRASS('g.region', w=w, e=e, s=s, n=n, flags='quiet')
			out <- invisible(TRUE)
			
		} # if defining region
		
	} # if GRASS started

	out

}
