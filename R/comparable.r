#' Determine if two GRasters and/or GVectors are geographically comparable
#'
#' `comparable()` compares geographic metadata between two `GRaster`s, two `GVector`s, or a `GRaster` and a `GVector`. In many cases, spatial objects must be comparable for them to "interact" (e.g., conducting arithmetic operations, masking, etc.).
#'
#' @param x,y `GRaster`s or `GVector`s.
#'
#' @param fail Logical: If `TRUE` (default), throw an error with an explanation if the objects are not comparable. If `FALSE` (default), return `TRUE` or `FALSE`.
#'
#' @param warn Logical: If `TRUE (default), display a warning if a condition is not met. This only comes into effect if `fail` is `TRUE`.
#'
#' @param extent Logical: If `TRUE`, test for same extent. By default, is `TRUE` for raster-raster comparison and `FALSE` for all others.
#'
#' @param zextent Logical: Test for same vertical extents (3D only). By default, this is `TRUE` for raster-raster comparisons, and `FALSE` for all others.
#'
#' @param dim.dim3d Logical (raster-raster only): If `TRUE` (default), test for same dimensions in 2D and 3D.
#'
#' @param res,res3d Logical (raster-raster only): If `TRUE` (default), test for same resolution in 2D and 3D.
#'
#' @param topo Logical: Test for same topology (2D or 3D). By default, this is `TRUE` for raster-raster comparisons, and `FALSE` for all others.
#'
#' @param nlayers Logical (raster-raster comparison only): If `FALSE` (default), do not compare number of layers. If `TRUE`, then return `FALSE` or fail if the number of layers in each raster is different.
#'
#' @param geometry Logical (vector-vector comparison only): Compare geometry. Default is `FALSE`.
#'
#' @return Logical (invisibly), or side effect of throwing an error.
#'
#' @aliases comparable
#' @rdname comparable
#' @exportMethod comparable
methods::setMethod(
	'comparable',
	signature = c(x = 'GRaster', y = 'GRaster'),
	definition = function(
		x,
		y,
		fail = TRUE,
		warn = TRUE,
		extent = TRUE,
		zextent = TRUE,
		dim = TRUE,
		dim3d = TRUE,
		res = TRUE,
		res3d = TRUE,
		topo = TRUE,
		nlayers = FALSE
	) {

	out <- TRUE
	out <- .sessionCompare(out = out, x = x, y = y, fail = fail, warn = warn)
	if (topo) out <- .topoCompare(out = out, x = x, y = y, fail = fail, warn = warn)
	if (extent) out <- .extentCompare(out = out, x = x, y = y, fail = fail, warn = warn)

	if (dim) {
		if (nrow(x) != nrow(y) | ncol(x) != ncol(y)) {
			msg <- 'The rasters have different rows and/or columns.'
			if (fail) { stop(msg) } else if (warn) { warning(msg) }
			out <- FALSE
		}
	}

	if (dim3d) {
		if (is.na(ndepth(x)) & !is.na(ndepth(y)) | !is.na(ndepth(x)) & is.na(ndepth(y))) {
			msg <- 'The rasters have different depths.'
			if (fail) { stop(msg) } else if (warn) { warning(msg) }
			out <- FALSE
		}

		if (!is.na(ndepth(x)) & !is.na(ndepth(y)) && ndepth(x) != ndepth(y)) {
			msg <- 'The rasters have different depths.'
			if (fail) { stop(msg) } else if (warn) { warning(msg) }
			out <- FALSE
		}
	}

	if (zextent) out <- .zextentCompare(out, x, y, fail = fail, warn = warn)
	

	if (nlayers) {
		if (nlyr(x) != nlyr(y)) {
			msg <- 'The rasters have a different number of layers.'
			if (fail) { stop(msg) } else if (warn) { warning(msg) }
			out <- FALSE
		}
	}

	invisible(out)

	} # EOF
)

#' @aliases comparable
#' @rdname comparable
#' @exportMethod comparable
methods::setMethod(f = 'comparable',
	signature = c(x = 'GRaster', y = 'GVector'),
	definition = function(
		x,
		y,
		fail = TRUE,
		warn = TRUE,
		extent = FALSE,
		zextent = FALSE,
		topo = FALSE
	) {

	out <- TRUE
	out <- .sessionCompare(out = out, x = x, y = y, fail = fail, warn = warn)
	if (topo) out <- .topoCompare(out = out, x = x, y = y, fail = fail, warn = warn)		
	if (extent) out <- .extentCompare(out = out, x = x, y = y, fail = fail, warn = warn)
	if (zextent) out <- .zextentCompare(out, x, y, fail = fail, warn = warn)

	invisible(out)

	} # EOF
)

#' @aliases comparable
#' @rdname comparable
#' @export
#' @exportMethod comparable
methods::setMethod(f = 'comparable',
	signature = c(x = 'GVector', y = 'GRaster'),
	definition = function(x, y, fail = TRUE, warn = TRUE, extent = FALSE, zextent = FALSE, topo = FALSE) {
		comparable(y, x, fail = fail, warn = warn, extent = extent, zextent = zextent, topo = topo)
	} # EOF
)
#' @aliases comparable
#' @rdname comparable
#' @export
#' @exportMethod comparable
methods::setMethod(f = 'comparable',
	signature = c(x = 'GVector', y = 'GVector'),
	definition = function(
		x,
		y,
		fail = TRUE,
		warn = TRUE,
		extent = FALSE,
		zextent = FALSE,
		topo = TRUE,
		geometry = FALSE
	) {

	out <- TRUE
	out <- .sessionCompare(out = out, x = x, y = y, fail = fail, warn = warn)
	if (extent) out <- .extentCompare(out = out, x = x, y = y, fail = fail, warn = warn)
	if (zextent) out <- .zextentCompare(out = out, x = x, y = y, fail = fail, warn = warn)
	if (topo) out <- .topoCompare(out = out, x = x, y = y, fail = fail, warn = warn)
	
	if (geometry) {
		if (geomtype(x) != geomtype(y)) {
			msg <- 'The vectors have different geometries.'
			if (fail) { stop(msg) } else if (warn) { warning(msg) }
			out <- FALSE
		}
	}

	invisible(out)

	} # EOF
)

### compare location, mapset, and CRS
.sessionCompare <- function(out, x, y, fail, warn) {
	
	if (location(x) != location(y)) {
		msg <- 'The objects have different GRASS locations.'
		if (fail) { stop(msg) } else if (warn) { warning(msg) }
		out <- FALSE
	}

	if (mapset(x) != mapset(y)) {
		msg <- 'The objects have different GRASS mapsets.'
		if (fail) { stop(msg) } else if (warn) { warning(msg) }
		out <- FALSE
	}

	if (st_crs(x) != st_crs(y)) {
		msg <- 'The objects have different coordinate reference systems.'
		if (fail) { stop(msg) } else if (warn) { warning(msg) }
		out <- FALSE
	}
	
	out
	
}

### compare extent
.extentCompare <- function(out, x, y, fail, warn) {
	
	if (any(compareFloat(as.vector(ext(x)), as.vector(ext(y)), '!='))) {
		msg <- 'The raster and vector have different extents.'
		if (fail) { stop(msg) } else if (warn) { warning(msg) }
		out <- FALSE
	}
	out
	
}
		


### compare z-extents
.zextentCompare <- function(out, x, y, fail, warn) {

	if (zextent) {
		if (topology(x) == '3D' & topology(y) == '3D') {
		
			xzext <- zext(x)
			xbottom <- xzext[1L]
			xtop <- xzext[2L]

			yzext <- zext(y)
			ybottom <- yzext[1L]
			ytop <- yzext[2L]
			
			if (anyNA(c(xzext, yzext))) {
				msg <- 'At least one raster has a missing z-extent.'
				if (fail) { stop(msg) } else if (warn) { warning(msg) }
				out <- FALSE
			}
			if (compareFloat(xbottom, ybottom, '!=')) {
				msg <- 'Objects have different vertical extents.'
				if (fail) { stop(msg) } else if (warn) { warning(msg) }
				out <- FALSE
			}
			if (compareFloat(xtop, ytop, '!=')) {
				stop('Objects have different vertical extents.')
				if (fail) { stop(msg) } else if (warn) { warning(msg) }
				out <- FALSE
			}
		}
	}
	out
} # EOF

### compare topology
.topoCompare <- function(out, x, y, fail, warn) {

	if (topo) {
		if (topology(x) != topology(y)) {
			msg <- 'The rasters have different topologies.'
			if (fail) { stop(msg) } else if (warn) { warning(msg) }
			out <- FALSE
		}
	}
	out
	
}

