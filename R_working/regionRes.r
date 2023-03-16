#' Get or set the spatial resolution of the active 'GRASS' region
#'
#' This function either reports the spatial resolution of a **GRASS** "region" or sets a new resolution for the region. *NOTE*: The extent of the region may be increased to accommodate an integer number of rows and columns.
#'
#' @param x Any of:
#'
#'	* `NULL` (default): Reports the spatial resolution of the current region. Also see argument \code{names}.
#'	* A `SpatRaster` object: Sets the region's resolution to the resolution of the object. Note that this does not export the raster to the **GRASS** session.
#'	* A vector of one or two numbers representing the resolution in the east-west and north-south directions (if two numbers) *or* resolution in both directions simultaneously (if one value--cells will be "square", though not necessarily actual squares owing to the curvature of the Earth).
#'	* The name of a raster in the active **GRASS** session: Resizes the resolution so it matched this raster.
#'
#' @param warn If `TRUE` (default), then print a warning if redefining the region resolution also forced a change in extent.
#' @param names If `TRUE` (default), then the returned vector will have names. Ignored if `x` is non-`NULL`.
#'
#' @return Either a numeric vector with two values, or `TRUE` (invisibly) if the resolution of the region was changed. Also resamples the resolution of the "region" in the active **GRASS** session.
#'
#' @seealso [regionDim()], [regionExt()], and [regionReshape()] in **fasterRaster**; **GRASS** module [https://grass.osgeo.org/grass82/manuals/g.region.html](g.region)
#'
#' @example man/examples/example_regions.r
#'
#' @export

regionRes <- function(
	x = NULL,
	warn = TRUE,
	names = TRUE
) {

	# report region
	if (is.null(x)) {

		info <- rgrass::execGRASS('g.region', flags=c('p', 'u'), intern=TRUE)

		ewres <- info[grepl('ewres:', info)]
		nsres <- info[grepl('nsres:', info)]

		ewres <- sub(ewres, pattern='ewres:', replacement='')
		nsres <- sub(nsres, pattern='nsres:', replacement='')

		ewres <- trimws(ewres)
		nsres <- trimws(nsres)

		ewres <- as.numeric(ewres)
		nsres <- as.numeric(nsres)

		out <- c(ewres, nsres)
		if (names ) names(out) <- c('ewres', 'nsres')
		out

	# define region
	} else if (inherits(x, c('SpatRaster', 'character', 'numeric'))) {
	
		if (warn) origExt <- regionExt()
	
		if (inherits(x, 'SpatRaster')) {
			res <- terra::res(x)
		} else if (inherits(x, 'character'))  {
			if (length(x) > 1L) stop('Argument "x" can only refer to a single raster.')
			res <- fasterRes(x)
		} else if (inherits(x, 'numeric')) {
			res <- x
			if (length(res) == 1L) res <- rep(res, 2L)
		}

		ewres <- as.character(res[1L])
		nsres <- as.character(res[2L])

		rgrass::execGRASS('g.region', ewres=ewres, nsres=nsres, flags='quiet')

		if (warn) {
			
			newExt <- regionExt()
			if (compareFloat(newExt[1L], origExt[1L], '!=') |
			    compareFloat(newExt[2L], origExt[2L], '!=') |
			    compareFloat(newExt[3L], origExt[3L], '!=') |
			    compareFloat(newExt[4L], origExt[4L], '!=')
			) warning('Region extent has been changed to accomodate the new resolution.')
		
		}
		
		invisible(TRUE)
	
	} else {
		stop('Region resolution was not changed.')
	}

}
