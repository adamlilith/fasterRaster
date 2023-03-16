#' "Stack" 'GRaster's
#'
#' `GRaster`s can be "stacked" using this function, effectively creating a mult-layered raster set.
#'
#' @param ... Two or more `GRaster`s in the same **GRASS** [location and mapset][location].
#'
#' @return A `GRaster`.
#'
#' @example man/examples/example_GRaster.r
#'
#' @export
setMethod(f = 'c',
	signature = 'GRaster',
	definition = function(x, ...) {
	
	out <- x
	dots <- list(...)
	
	for (i in seq_along(dots)) {
	
		if (!inherits(dots[[i]], 'GRaster')) stop('Can only combine GRasters.')
		
		comparable(x, dots[[i]], fail = TRUE)
		
		gname <- .gname(dots[[i]])
		names <- names(dots[[i]])
		info <- .rastInfo(gname)
		
		out <- GRaster(
			location = out@location,
			mapset = out@mapset,
			crs = crs(out),
			gname = c(out@gname, gname),
			names = c(out@names, names),
			topology = c(out@topology, info[['topology']][1L]),
			extent = out@extent,
			ztop = c(out@ztop, info[['ztop']]),
			zbottom = c(out@zbottom, info[['zbottom']]),
			datatypeGRASS = c(out@datatypeGRASS, info[['grassDataType']]),
			dimensions = c(out@dimensions[1L:3L], out@dimensions[4L] + nlyr(dots[[i]])),
			resolution = out@resolution,
			numCategories = c(out@numCategories, info[['numCategories']]),
			minVal = c(out@minVal, info[['minVal']]),
			maxVal = c(out@maxVal, info[['maxVal']])
		)
	
	} # next GRaster to combine
	
	out
	
	} # EOF
)
