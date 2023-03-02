#' Change or report the number of rows and/or columns of a 'GRASS' region
#'
#' This function either reports the number of rows and columns of a **GRASS** "region", or sets the number of rows and columns. *NOTE*: The spatial resolution of the region may be changed to accommodate the desired number of rows and columns.
#'
#' @param x Any of:
#'
#'	* Missing (default): Reports the number of rows and columns of the current region.
#'	* A `GRaster`: Sets the region's dimensions to that of the raster.
#'	* A `SpatRaster`: Sets the region's dimensions to those of the raster. This does not convert the raster to a `GRaster`.
#'	* A numeric vector: Number of rows and columns, respectively.
#'	* Character: The name of a raster in the active **GRASS** session. Uses the number of rows and columns of this raster.
#'
#' @param names If `TRUE` (default), then the returned vector will have names. Ignored if `x` is non-`NULL`.
#'
#' @return Number of rows and columns, or `TRUE` (invisibly) if the dimensions of the region were changed. Also resamples the resolution of the "region" in the active **GRASS** session.
#'
#' @seealso [regionRes()], [regionExt()], and [regionReshape()] in **fasterRaster**; **GRASS** module [https://grass.osgeo.org/grass82/manuals/g.region.html](g.region)
#'
#' @example man/examples/example_regions.r
#'
#' @export

regionDim <- function(
	x,
	names = TRUE
) {

	# report region
	if (is.null(x)) {

		info <- rgrass::execGRASS('g.region', flags=c('p', 'u'), intern=TRUE)

		rows <- info[grepl('rows:', info)]
		cols <- info[grepl('cols:', info)]

		rows <- sub(rows, pattern='rows:', replacement='')
		cols <- sub(cols, pattern='cols:', replacement='')

		rows <- trimws(rows)
		cols <- trimws(cols)

		rows <- as.numeric(rows)
		cols <- as.numeric(cols)

		out <- c(rows, cols)
		if (names ) names(out) <- c('rows', 'cols')
		out

	# define region
	} else if (inherits(x, c('SpatRaster', 'character'))) {
	
		if (inherits(x, 'SpatRaster')) {
			dim <- terra::dim(x)
		} else if (inherits(x, 'character'))  {
			if (length(x) > 1L) stop('Argument "x" can only refer to a single raster.')
			dim <- fasterDim(x)
		}

		rows <- rows[1L]
		cols <- cols[2L]

		rgrass::execGRASS('g.region', rows=rows, cols=cols, flags='quiet')
		invisible(TRUE)
		
	
	} else {
		stop('Region dimensions were not changed.')
	}

}
