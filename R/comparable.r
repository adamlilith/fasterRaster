#' Determine if two GRasters and/or GVectors are geographically comparable
#'
#' `comparable()` compares geographic metadata between two `GRaster`s, two `GVector`s, or a `GRaster` and a `GVector`. In many cases, spatial objects must be comparable for them to "interact" (e.g., conducting arithmetic operations, masking, etc.).
#'
#' @param x,y `GRaster`s or `GVector`s.
#' @param fail Logical: If `TRUE`, throw an error with an explanation if the objects are not comparable. If `FALSE` (default), return `TRUE` or `FALSE`.
#' @param compareTopo Logical: If `TRUE`, compare topology (only for `GRaster` vs. `GVector` comparison and `GVector` vs. `GVector` comparison).
#' @param compareZExt Logical: If `TRUE`, compare vertical extent. This will only be tested if `comparaTopo` is `TRUE`, and is only value for `GRaster` vs. `GVector` comparison and `GVector` vs. `GVector` comparison.
#'
#' @return Logical (invisibly), or side effect of throwing an error.
#'
#' @aliases comparable
#' @rdname comparable
#' @exportMethod comparable
methods::setMethod(
	'comparable',
	signature = c(x = 'GRaster', y = 'GRaster'),
	definition = function(x, y, fail = FALSE) {

	out <- TRUE
	if (location(x) != location(y)) {
		if (fail) stop('The rasters have different GRASS locations.')
		out <- FALSE
	}

	if (mapset(x) != mapset(y)) {
		if (fail) stop('The rasters have different GRASS mapsets.')
		out <- FALSE
	}

	if (topology(x) != topology(y)) {
		if (fail) stop('The rasters have different topologies.')
		out <- FALSE
	}

	if (st_crs(x) != st_crs(y)) {
		if (fail) stop('The rasters have different coordinate reference systems.')
		out <- FALSE
	}

	if (any(compareFloat(as.vector(ext(x)), as.vector(ext(y)), '!='))) {
		if (fail) stop('The rasters have different extents.')
		out <- FALSE
	}

	if (nrow(x) != nrow(y) | ncol(x) != ncol(y)) {
		if (fail) stop('The rasters have different rows and/or columns.')
		out <- FALSE
	}

	if (is.na(ndepth(x)) & !is.na(ndepth(y)) | !is.na(ndepth(x)) & is.na(ndepth(y))) {
		if (fail) stop('The rasters have different depths.')
		out <- FALSE
	}

	if (!is.na(ndepth(x)) & !is.na(ndepth(y)) && ndepth(x) != ndepth(y)) {
		if (fail) stop('The rasters have different depths.')
		out <- FALSE
	}

	if (is.na(zext(x)[1L]) & !is.na(ndepth(y)[1L]) | !is.na(zext(x)[1L]) & is.na(zext(y)[1L]) |
		is.na(zext(x)[2L]) & !is.na(ndepth(y)[2L]) | !is.na(zext(x)[2L]) & is.na(zext(y)[2L])) {
		if (fail) stop('The rasters have different vertical extents.')
		out <- FALSE
	}

	if ((!is.na(zext(x)[1L]) & !is.na(zext(y)[1L]) & !is.na(zext(x)[2L]) & !is.na(zext(y)[2L])) && zext(x) != zext(y)) {
		if (fail) stop('The rasters have different vertical extents.')
		out <- FALSE
	}

	invisible(out)

	} # EOF
)

#' @aliases comparable
#' @rdname comparable
#' @exportMethod comparable
methods::setMethod(f = 'comparable',
	signature = c(x = 'GRaster', y = 'GVector'),
	definition = function(x, y, fail = FALSE, compareTopo = FALSE, compareZExtent = FALSE) {

	out <- TRUE
	if (location(x) != location(y)) {
		if (fail) stop('The raster and vector have different GRASS locations.')
		out <- FALSE
	}

	if (mapset(x) != mapset(y)) {
		if (fail) stop('The raster and vector have different GRASS mapsets.')
		out <- FALSE
	}

	if (st_crs(x) != st_crs(y)) {
		if (fail) stop('The raster and vector have different coordinate reference systems.')
		out <- FALSE
	}

	if (compareTopo) {

		if (topology(x) != topology(y)) {
			if (fail) stop('The raster and vector have different topologies.')
			out <- FALSE
		}
		
		if ((is.na(zext(x)[1L]) & !is.na(zext(y)[1L]) | !is.na(zext(x)[1L]) & is.na(zext(y)[1L]) |
			is.na(zext(x)[2L]) & !is.na(zext(y)[2L]) | !is.na(zext(x)[2L]) & is.na(zext(y)[2L])) ||
			compareFloat(zext(x)[1L], zext(y)[1L], '!=') | compareFloat(zext(x)[2L], zext(y)[2L], '!=')) {

			if (fail) stop('The raster and vector have different vertical extents.')
			out <- FALSE
		
		}
		
	}

	invisible(out)

	} # EOF
)

#' @aliases comparable
#' @rdname comparable
#' @export
#' @exportMethod comparable
methods::setMethod(f = 'comparable',
	signature = c(x = 'GVector', y = 'GRaster'),
	definition = function(x, y, fail = FALSE, compareTopo = FALSE) {
		comparable(y, x, fail = fail, compareTopo)
	} # EOF
)

#' @aliases comparable
#' @rdname comparable
#' @export
#' @exportMethod comparable
methods::setMethod(f = 'comparable',
	signature = c(x = 'GVector', y = 'GVector'),
	definition = function(x, y, fail = FALSE, compareTopo = FALSE) {

	out <- TRUE
	if (location(x) != location(y)) {
		if (fail) stop('The vectors have different GRASS locations.')
		out <- FALSE
	}

	if (mapset(x) != mapset(y)) {
		if (fail) stop('The vectors have different GRASS mapsets.')
		out <- FALSE
	}

	if (st_crs(x) != st_crs(y)) {
		if (fail) stop('The vectors have different coordinate reference systems.')
		out <- FALSE
	}

	if (topology(x) != topology(y)) {
		if (fail) stop('The vectors have different topologies.')
		out <- FALSE
	}

	if (compareTopo) {

		if (topology(x) != topology(y)) {
			if (fail) stop('The vectors have different topologies.')
			out <- FALSE
		}
		
		if ((is.na(zext(x)[1L]) & !is.na(zext(y)[1L]) | !is.na(zext(x)[1L]) & is.na(zext(y)[1L]) |
			is.na(zext(x)[2L]) & !is.na(zext(y)[2L]) | !is.na(zext(x)[2L]) & is.na(zext(y)[2L])) ||
			compareFloat(zext(x)[1L], zext(y)[1L], '!=') | compareFloat(zext(x)[2L], zext(y)[2L], '!=')) {

			if (fail) stop('The vectors have different vertical extents.')
			out <- FALSE
			
		}
		
	}
	invisible(out)

	} # EOF
)
