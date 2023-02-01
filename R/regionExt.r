#' Get or set the spatial extent of a GRASS region
#'
#' This function either reports the spatial extent of a \code{GRASS} \link{region} or sets a new extent for the region.
#'
#' @param x Any of:
#' \itemize{
#'	\item Missing (default) or \code{NULL}: Reports the extent of the current region. Also see argument \code{names}.
#'	\item A \code{SpatRaster}, \code{SpatVector}, \code{SpatExtent}, or \code{sf} object: Sets the region's extent to the extent of the object. Note that this does not export the object to the \code{GRASS} session.
#'	\item A vector of four numbers representing the longitude and latitude of the new extent: These must be listed in this order: western longitude, eastern longitude, southern latitude, northern latitude.
#'	\item The name of one or more rasters or vectors in the active \code{GRASS} session: Resizes the extent to exactly encompass all objects.
#'	\item \code{'*'}: Resizes the extent to \emph{all} objects in the active \code{GRASS} session. If you also set \code{rastOrVect} to \code{'rasters'} or \code{'vectors'}, it will resize the region to all rasters or vectors.
#' }
#' @param rastOrVect Either \code{'raster'} and/or \code{'vector'} (one value per value in \code{x}). If \code{NULL} (default), then the function will attempt to guess whether \code{x} refers to a raster or vector. However, in \code{GRASS}, it is possible to have a raster and a vector of the same name. If this is the case, then you can specify whether \code{x} is a raster or vector (partial matching is supported).
#' @param names If \code{TRUE} (default), then the returned vector will have names. Ignored if \code{x} is non-\code{NULL}.
#' @param terra If \code{TRUE}, then the returned object will be a \code{\link[terra]{SpatExtent}}. Ignored if \code{x} is non-\code{NULL}.
#'
#' @return Either a numeric vector with four values, or \code{TRUE} (invisibly) if resizing was successful. Also resizes the extent of the \link{region} in the active \code{GRASS} session.
#'
#' @seealso \code{\link{regionDim}}, \code{\link{regionRes}}, and \code{\link{regionReshape}} in \pkg{fasterRaster}; \code{\link[terra]{ext}} in the \pkg{terra} package; \code{\link[sf]{st_bbox}} in the \pkg{sf} package; \code{GRASS} module \code{\href{https://grass.osgeo.org/grass82/manuals/g.region.html}{g.region}}
#'
#' @example man/examples/ex_regions.r
#'
#' @export

regionExt <- function(
	x,
	rastOrVect = c('rasters', 'vectors'),
	names = TRUE,
	terra = FALSE
) {

	# report region
	if (missing(x) || is.null(x)) {

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
			
			rastOrVect <- .getRastOrVect(rastOrVect, n=Inf, nullOK=FALSE)

			info <- if (x != '*') {
				fasterInfo(x, rastOrVect=rastOrVect)
			} else if (x == '*') {
				fasterInfo()
			}

			if (is.null(info)) {
			
				meta <- rgrass::gmeta(ignore.stderr=TRUE)
				extent <- c(meta$w, meta$e, meta$s, meta$n)
				
				warning('No spatial objects are in the GRASS session. Using session defaults for the extent.')
				
			} else {

				extent <- c(
					min(info$west),
					max(info$east),
					min(info$south),
					max(info$north)
				)
					
			}
			
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
		invisible(TRUE)
		
	}

}
