#' Determine if two GRasters and/or GVectors are geographically comparable
#'
#' `comparable()` compares geographic metadata between two `GRaster`s, two `GVector`s, or a `GRaster` and a `GVector`. In many cases, spatial objects must be comparable for them to "interact" (e.g., conducting arithmetic operations, masking, etc.).
#'
#' @param x,y `GRaster`s or `GVector`s.
#' @param nlayers Logical: If `FALSE` (default), do not compare number of layers. If `TRUE`, then return `FALSE` or fail if the number of layers in each raster is different.
#' @param fail Logical: If `TRUE`, throw an error with an explanation if the objects are not comparable. If `FALSE` (default), return `TRUE` or `FALSE`.
#' @param compareTopo Logical: If `TRUE`, compare topology (only for `GRaster` vs. `GVector` comparison and `GVector` vs. `GVector` comparison).
#' @param compareGeo Logical: If `TRUE`, compare geometry (`GVector` vs. `GVector` comparison).
#' @param compareZ Logical: If `TRUE`, compare z extents (`GVector` vs. `GVector` and `GRaster` vs. `GVector` comparison--this is always done for `GRaster`s vs. `GRaster`s).
#'
#' @return Logical (invisibly), or side effect of throwing an error.
#'
#' @aliases comparable
#' @rdname comparable
#' @exportMethod comparable
methods::setMethod(
	'comparable',
	signature = c(x = 'GRaster', y = 'GRaster'),
	definition = function(x, y, nlayers = FALSE, fail = TRUE) {

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
	
	if (nlayers && nlyr(x) != nlyr(y)) {
		if (fail) stop('The rasters have a different number of layers.')
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
	definition = function(x, y, fail = TRUE, compareTopo = FALSE, compareZ = FALSE) {

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
		
		if (compareZ & topology(x) == '3D' & topology(y) == '3D') {
			
			xzext <- zext(x)
			xbottom <- xzext[1L]
			xtop <- xzext[2L]

			yzext <- zext(y)
			ybottom <- yzext[1L]
			ytop <- yzext[2L]
			
			if (anyNA(c(xzext, yzext))) stop('At least one z-extent value has a missing z-extent.')
			if (compareFloat(xbottom, ybottom, '!='))  stop('Vector and raster have different vertical extents.')
			if (compareFloat(xtop, ytop, '!=')) stop('Vector and raster have different vertical extents.')
			
		} # compare z extent?
		
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
	definition = function(x, y, fail = TRUE, compareTopo = FALSE) {
		comparable(y, x, fail = fail, compareTopo)
	} # EOF
)

#' @aliases comparable
#' @rdname comparable
#' @export
#' @exportMethod comparable
methods::setMethod(f = 'comparable',
	signature = c(x = 'GVector', y = 'GVector'),
	definition = function(x, y, fail = TRUE, compareTopo = FALSE, compareGeo = FALSE, compareZ = FALSE) {

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

	if (compareGeo) {
		if (geomtype(x) != geomtype(y)) {
			if (fail) stop('The vectors have different geometries.')
			out <- FALSE
		}
	}

	if (compareTopo) {

		if (topology(x) != topology(y)) {
			if (fail) stop('The vectors have different topologies.')
			out <- FALSE
		}
		
		if (compareZ & topology(x) == '3D' & topology(y) == '3D') {
			
			xzext <- zext(x)
			xbottom <- xzext[1L]
			xtop <- xzext[2L]

			yzext <- zext(y)
			ybottom <- yzext[1L]
			ytop <- yzext[2L]
			
			if (anyNA(c(xzext, yzext))) stop('At least one vector has a missing z-extent.')
			if (compareFloat(xbottom, ybottom, '!=')) stop('Vectors have different vertical extents.')
			if (compareFloat(xtop, ytop, '!=')) stop('Vectors have different vertical extents.')
			
		} # compare z extent?
		
	}
	invisible(out)

	} # EOF
)
