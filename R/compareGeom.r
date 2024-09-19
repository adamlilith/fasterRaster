#' Determine if GRasters and/or GVectors are geographically comparable
#'
#' `compareGeom()` compares geographic metadata between two or more `GRaster`s and/or `GVector`s. In many cases, spatial objects must be comparable for them to "interact" (e.g., conducting arithmetic operations, masking, etc.).
#'
#' @param x,y,... `GRaster`s or `GVector`s. If `y` is `GRaster`, then the `...` must also be `GRaster`s (or missing). If `y` is `GVector`, then the `...` must also be `GVector`s (or missing).
#'
#' @param location,mapset Logical: Compare **GRASS** "project/location" and "mapsets" (see `vignette("10_projects_locations_mapsets", package = "fasterRaster")`). Default is `TRUE`.
#' 
#' @param crs Logical: Compare coordinate reference systems. Default is `TRUE`.
#' 
#' @param lyrs Logical (rasters only): Compare number of layers of "stacked" rasters. Note this is different from number of vertical "depths" of a raster. Default is `FALSE`.
#' 
#' @param topo Logical: Test for same topology (2D or 3D). By default, this is `TRUE` for raster-raster comparisons, and `FALSE` for all others.
#'
#' @param ext Logical: If `TRUE`, test for same extent. By default, is `TRUE` for raster-raster comparison and `FALSE` for all others.
#'
#' @param zext Logical: Test for same vertical extents (3D only). By default, this is `TRUE` for raster-raster comparisons, and `FALSE` for all others.
#' 
#' @param rowcol Logical (rasters only): Test for same number of rows and columns. Default is `TRUE`.
#' 
#' @param depths Logical (rasters only): Test for same number of depths. Default is `TRUE`.
#' 
#' @param res Logical (rasters only): Test for same resolution in x- and y-dimensions. Default is `TRUE`.
#'
#' @param zres Logical (rasters only): Test for same resolution in z dimension. Default is `TRUE`.
#' 
#' @param geometry Logical (vector-vector comparison only): Compare geometry. Default is `FALSE`.
#'
#' @param stopOnError Logical: If `TRUE` (default), throw an error with an explanation if the objects are not comparable. If `FALSE` (default), return `TRUE` or `FALSE`.
#'
#' @param messages Logical: If `TRUE (default), display a warning if a condition is not met. This only comes into effect if `stopOnError` is `FALSE`.
#'
#' @returns Logical (invisibly): `TRUE` for no mismatches detected, `FALSE` for incompatibility), or side effect of throwing an error.
#'
#' @aliases compareGeom
#' @rdname compareGeom
#' @exportMethod compareGeom
methods::setMethod(
	"compareGeom",
	signature = c(x = "GRaster", y = "GRaster"),
	definition = function(
		x,
		y,
		...,
		location = TRUE,
		mapset = TRUE,
		topo = TRUE,
		lyrs = FALSE,
		crs = TRUE,
		ext = TRUE,
		zext = TRUE,
		rowcol = TRUE,
		depths = TRUE,
		res = TRUE,
		zres = TRUE,
		stopOnError = TRUE,
		messages = TRUE
	) {

	out <- TRUE

	y <- list(y, ...)
	for (i in seq_along(y)) {

		if (location) out <- .locationCompare(out=out, x=x, y=y[[i]], stopOnError = stopOnError, messages = messages)
		if (mapset) out <- .mapsetCompare(out = out, x=x, y=y[[i]], stopOnError = stopOnError, messages = messages)
		if (crs) out <- .crsCompare(out = out, x = x, y = y[[i]], stopOnError = stopOnError, messages = messages)

		if (lyrs) {
			if (nlyr(x) != nlyr(y[[i]])) {
				msg <- "The rasters have a different number of layers."
				if (stopOnError) stop(msg)
				if (messages & !stopOnError) warning(msg)
				out <- FALSE
			}
		}

		if (topo) out <- .topoCompare(out=out, x=x, y=y[[i]], stopOnError=stopOnError, messages=messages)
		if (ext) out <- .extentCompare(out=out, x=x, y=y[[i]], stopOnError=stopOnError, messages=messages)
		if (zext) out <- .zextentCompare(out=out, x=x, y=y[[i]], stopOnError=stopOnError, messages=messages)

		if (rowcol) {
			xdim <- dim(x)[1L:2L]
			ydim <- dim(y[[i]])[1L:2L]

			if (!all(xdim == ydim)) {
				msg <- "The rasters have different numbers of rows and/or columns."
				if (stopOnError) stop(msg)
				if (messages & !stopOnError) warning(msg)
				out <- FALSE
			}
		}

		if (depths) {

			msg <- "The rasters have a different numbers of depths."

			xd <- ndepth(x)
			yd <- ndepth(y[[i]])
			xna <- is.na(xd)
			yna <- is.na(yd)
			if (any(xna) | any(yna)) {
				if (!all(xna == yna)) {
					if (stopOnError) stop(msg)
					if (messages & !stopOnError) warning(msg)
					out <- FALSE
				}
			}

			if (any(!xna) | any(!yna)) {
				if (!all(xd[!xna] == yd[!yna])) {
					if (stopOnError) stop(msg)
					if (messages & !stopOnError) warning(msg)
					out <- FALSE
				}
			}

		}

		if (res) {
			xres <- res(x)[1L:2L]
			yres <- res(y[[i]])[1L:2L]
			
			if (!all(omnibus::compareFloat(xres, yres, "=="))) {
				msg <- "The rasters have a different horizontal resolutions."
				if (stopOnError) stop(msg)
				if (messages & !stopOnError) warning(msg)
				out <- FALSE
			}
		}

		if (zres) {
			
			xzres <- zres(x)
			yzres <- zres(y[[i]])

			if (!is.na(xzres) & !is.na(yzres)) {

				msg <- "The rasters have a different vertical resolutions."

				if (
					(is.na(xzres) & !is.na(yzres)) |
					(!is.na(xzres) & is.na(yzres))
				) {
					if (stopOnError) stop(msg)
					if (messages & !stopOnError) warning(msg)
					out <- FALSE
				} else if (omnibus::compareFloat(xzres, yzres, "!=")) {
					if (stopOnError) stop(msg)
					if (messages & !stopOnError) warning(msg)
					out <- FALSE
				}

			}

		}

	} # next object to compare to

	invisible(out)

	} # EOF
)

#' @aliases compareGeom
#' @rdname compareGeom
#' @exportMethod compareGeom
methods::setMethod(f = "compareGeom",
	signature = c(x = "GVector", y = "GVector"),
	definition = function(
		x,
		y,
		...,
		location = TRUE,
		mapset = TRUE,
		topo = FALSE,
		crs = TRUE,
		ext = FALSE,
		zext = FALSE,
		geometry = FALSE,
		stopOnError = TRUE,
		messages = TRUE
	) {

	.compareGeneric(
		x = x,
		y = y,
		...,
		location = location,
		mapset = mapset,
		crs = crs,
		topo = topo,
		ext = ext,
		zext = zext,
		geometry = geometry,
		stopOnError = stopOnError,
		messages = messages
	)

	} # EOF
)

#' @aliases compareGeom
#' @rdname compareGeom
#' @exportMethod compareGeom
methods::setMethod(f = "compareGeom",
	signature = c(x = "GRaster", y = "GVector"),
	definition = function(
		x,
		y,
		...,
		location = TRUE,
		mapset = TRUE,
		topo = FALSE,
		crs = TRUE,
		ext = FALSE,
		zext = FALSE,
		stopOnError = TRUE,
		messages = TRUE
	) {

	.compareGeneric(
		x = x,
		y = y,
		...,
		location = location,
		mapset = mapset,
		crs = crs,
		topo = topo,
		ext = ext,
		zext = zext,
		geometry = FALSE,
		stopOnError = stopOnError,
		messages = messages
	)

	} # EOF
)

#' @aliases compareGeom
#' @rdname compareGeom
#' @exportMethod compareGeom
methods::setMethod(f = "compareGeom",
	signature = c(x = "GVector", y = "GRaster"),
	definition = function(
		x,
		y,
		...,
		location = TRUE,
		mapset = TRUE,
		topo = FALSE,
		crs = TRUE,
		ext = FALSE,
		zext = FALSE,
		stopOnError = TRUE,
		messages = TRUE
	) {

	.compareGeneric(
		x = x,
		y = y,
		...,
		location = location,
		mapset = mapset,
		crs = crs,
		topo = topo,
		ext = ext,
		zext = zext,
		geometry = FALSE,
		stopOnError = stopOnError,
		messages = messages
	)

	} # EOF
)

# x is a GRaster and y and ... GVectors OR
# x is a GVector and y and ... GRasters
.compareGeneric <- function(
	x,
	y,
	...,
	location,
	mapset,
	crs,
	topo,
	ext,
	zext,
	geometry,
	stopOnError,
	messages
) {

	out <- TRUE
	y <- list(y, ...)
	
	allGVector <- sapply(y, inherits, "GVector")
	allGRaster <- sapply(y, inherits, "GRaster")
	allGVector <- all(allGVector)
	allGRaster <- all(allGRaster)
	allSameClass <- allGRaster | allGVector

	if (!allSameClass) stop("The objects supplied in ... must be of the same class as the y object.")

	for (i in seq_along(y)) {

		if (location) out <- .locationCompare(out=out, x=x, y=y[[i]], stopOnError=stopOnError, messages=messages)
		if (mapset) out <- .mapsetCompare(out=out, x=x, y=y[[i]], stopOnError=stopOnError, messages=messages)
		if (crs) out <- .crsCompare(out=out, x=x, y=y[[i]], stopOnError=stopOnError, messages=messages)
		if (topo) out <- .topoCompare(out=out, x=x, y=y[[i]], stopOnError=stopOnError, messages=messages)
		if (ext) out <- .extentCompare(out=out, x=x, y=y[[i]], stopOnError=stopOnError, messages=messages)
		if (zext) out <- .zextentCompare(out=out, x=x, y=y[[i]], stopOnError=stopOnError, messages=messages)

		if (geometry) {
			if (geomtype(x) != geomtype(y[[i]])) {
				msg <- "The vectors have a different geometries."
				if (stopOnError) stop(msg)
				if (messages & !stopOnError) warning(msg)
				out <- FALSE
			}
		}

	} # next object
	out

}

### compare locations
.locationCompare <- function(out, x, y, stopOnError, messages) {

	if (.location(x) != .location(y)) {
		msg <- paste0("The objects have different coordinate reference systems. You can use\n  project() to warp one of them to the reference system of the other.")
		if (stopOnError) stop(msg)
		if (messages & !stopOnError) warning(msg)
		out <- FALSE
	}
	out
}

### compare mapsets
.mapsetCompare <- function(out, x, y, stopOnError, messages) {

	if (.mapset(x) != .mapset(y)) {
		msg <- "The objects are in different GRASS mapsets."
		if (stopOnError) stop(msg)
		if (messages & !stopOnError) warning(msg)
		out <- FALSE
	}
	out
}

### compare CRS
.crsCompare <- function(out, x, y, stopOnError, messages) {

	if (st_crs(x) != st_crs(y)) {
		msg <- "The objects have different coordinate reference systems."
		if (stopOnError) stop(msg)
		if (messages & !stopOnError) warning(msg)
		out <- FALSE
	}
	out
}

### compare extents
.extentCompare <- function(out, x, y, stopOnError, messages) {
	
	xx <- ext(x, vector=TRUE)
	yy <- ext(y, vector=TRUE)
	if (any(omnibus::compareFloat(xx, yy, "!="))) {
		msg <- "The objects have different extents."
		if (stopOnError) stop(msg)
		if (messages & !stopOnError) warning(msg)
		out <- FALSE
	}
	out
	
}

### compare z-extents
.zextentCompare <- function(out, x, y, stopOnError, messages) {

	if (topology(x) == "3D" & topology(y) == "3D") {
	
		xzext <- zext(x)
		xbottom <- xzext[1L]
		xtop <- xzext[2L]

		yzext <- zext(y)
		ybottom <- yzext[1L]
		ytop <- yzext[2L]
		
		if (anyNA(c(xzext, yzext))) {
			msg <- "At least one raster has a missing z-ext."
			if (stopOnError) stop(msg)
			if (messages & !stopOnError) warning(msg)
			out <- FALSE
		}
		if (omnibus::compareFloat(xbottom, ybottom, "!=")) {
			msg <- "Objects have different vertical extents."
			if (stopOnError) stop(msg)
			if (messages & !stopOnError) warning(msg)
			out <- FALSE
		}
		if (omnibus::compareFloat(xtop, ytop, "!=")) {
			stop("Objects have different vertical extents.")
			if (stopOnError) stop(msg)
			if (messages & !stopOnError) warning(msg)
			out <- FALSE
		}
	}
	out
	
} # EOF

### compare topology
.topoCompare <- function(out, x, y, stopOnError, messages) {

	if (topology(x) != topology(y)) {
		msg <- "The rasters have different topologies."
		if (stopOnError) stop(msg)
		if (messages & !stopOnError) warning(msg)
		out <- FALSE
	}
	out
	
}
