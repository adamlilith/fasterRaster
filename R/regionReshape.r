#' Change the extent and dimensions of a region simultaneously
#'
#' This function simultaneously changes the extent and dimensions of a \code{GRASS} \link{region}.
#'
#' @param x Any of:
#' \itemize{
#'	\item Missing (default) or \code{NULL}: Reports the extent, resolution, and dimensions of the current region. All other arguments will be ignored.
#'	\item A \code{SpatRaster} object: Sets the region's extent and dimensions to those of the raster. Note that this does not export the raster to the \code{GRASS} session.
#'	\item The name of a raster in the active \code{GRASS} session: Resizes the extent and dimensions to those of this raster.
#'	\item Values for each of \code{extent} and \emph{either} \code{dim} or \code{res} ("manual" model):
#'	\itemize{
#'		\item \code{extent}: Longitude and latitude of the new extent with four values listed in this order: western longitude, eastern longitude, southern latitude, northern latitude.
#'		\item \code{dim}: Two integers representing number of rows and columns, respectively, in the region. The new resolution will be calculated by dividing the extent by the number of rows and columns.
#'		\item \code{res}: One or two values representing spatial resolution of the region. If one value, it will be used as both the east-west and north-south resolution. If two values, these represent east-west and north-south resolution, respectively. The new dimensions will be calculated by dividing the extent by the resolution. If this is not a whole number, the extent will be increased to accommodate an integer number of rows and columns.
#'	}
#' }
#' @param extent,dim,res Extent, dimensions, and resolution of the new region. These need only be specified if the region is to be resized and/or resampled "manually" (i.e., not using a pre-existing raster). In this case, \code{extent} must always be specified, and either \code{dim} or \code{res} must be specified. To change only extent, dimension, or resolution, use \code{\link{regionExt}}, \code{\link{regionDim}}, or \code{\link{regionRes}}.
#' @param warn If \code{TRUE} (default), then print a warning if redefining the region resolution also forced a change in extent.
#' @param ... Other arguments (not used).
#'
#' @return Either a list, or \code{TRUE} (invisibly) if resizing and resampling was successful. Also resizes and resamples the \link{region} in the active \code{GRASS} session.
#'
#' @seealso \code{\link{regionExt}}, \code{\link{regionDim}}, and \code{\link{regionRes}} in \pkg{fasterRaster}; \code{\link[terra]{ext}} in the \pkg{terra} package; \code{\link[sf]{st_bbox}} in the \pkg{sf} package; \code{GRASS} module \code{\href{https://grass.osgeo.org/grass82/manuals/g.region.html}{g.region}}
#'
#' @example man/examples/ex_regions.r
#'
#' @export

regionReshape <- function(
	x,
	extent = NULL,
	dim = NULL,
	res = NULL,
	warn = TRUE,
	...
) {

	if (missing(x)) x <- NULL

	if (inherits(x, 'SpatRaster')) {

		extent <- terra::ext(x)@ptr$vector
		dim <- dim(x)[1L:2L]
		
		dimOrRes <- 'dim'

	} else if (inherits(x, 'character'))  {
		
		extent <- fasterExt(x, rastOrVect = 'raster', ...)
		dim <- fasterDim(x, ...)
		
		dimOrRes <- 'dim'

	} else if (is.null(x) & inherits(extent, 'numeric') & (inherits(dim, 'numeric') | inherits(res, 'numeric'))) {
	
		if (length(extent) != 4L) stop('Invalid value for argument "extent".')
		if (!is.null(dim) & !is.null(res)) warning('Both "dim" and "res" have been supplied, but only "dim" will be used.')
		
		if (!is.null(dim)) {
			
			dimOrRes <- 'dim'

		} else if (!is.null(res)) {
		
			if (length(res) == 1L) res <- rep(res, 2L)
			dimOrRes <- 'res'

		}

	} else {
		stop('regionReshape: Invalid arguments.')
	}
	
	# reshape
	w <- as.character(extent[1L])
	e <- as.character(extent[2L])
	s <- as.character(extent[3L])
	n <- as.character(extent[4L])
	
	if (dimOrRes == 'dim') {

		rows <- dim[1L]
		cols <- dim[2L]

		rgrass::execGRASS('g.region', w=w, e=e, s=s, n=n, rows=rows, cols=cols, flags='quiet')

	} else if (dimOrRes == 'res') {

		ewres <- as.character(res[1L])
		nsres <- as.character(res[2L])

		rgrass::execGRASS('g.region', w=w, e=e, s=s, n=n, ewres=ewres, nsres=nsres, flags='quiet')

	}
	
	if (warn) {
		
		newExt <- regionExt()
		if (compareFloat(newExt[1L], extent[1L], '!=') |
			compareFloat(newExt[2L], extent[2L], '!=') |
			compareFloat(newExt[3L], extent[3L], '!=') |
			compareFloat(newExt[4L], extent[4L], '!=')
		) warning('Extent has been modified to accomodate the new dimensions/resolution.')
		
	}
	
	invisible(TRUE)

}
