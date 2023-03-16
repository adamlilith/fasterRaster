#' Determine if two GRasters and/or GVectors are geographically comparable
#'
#' `comparable()` compares geographic metadata between two `GRaster`s, two `GVector`s, or a `GRaster` and a `GVector`. In many cases, spatial objects must be comparable for them to "interact" (e.g., using addition, making, etc.).
#'
#' @param x,y `GRaster`s or `GVector`s.
#' @param fail If `TRUE`, throw an error with an explanation if the objects are not comparable. If `FALSE` (default), return `TRUE` or `FALSE`.
#'
#' @return Logical or an error.
#'
#' @noRd
if (!isGeneric('comparable')) setGeneric(name='comparable', def=function(x, y, ...) standardGeneric('comparable'))

setMethod(f = 'comparable',
	signature = c('GRaster', 'GRaster'),
	definition = function(x, y, fail = FALSE) {

	xloc <- location(x)
	xmapset <- mapset(x)
	xcrs <- st_crs(x)
	xext <- ext(x, vector = TRUE)
	xdim <- dim(x)
	xdepth <- ndepth(x)
	xtopology <- topology(x)
	xzext <- zExt(x)

	yloc <- location(y)
	ymapset <- mapset(y)
	ycrs <- st_crs(y)
	yext <- ext(y, vector = TRUE)
	ydim <- dim(y)
	ydepth <- ndepth(y)
	ytopology <- topology(y)
	yzext <- zExt(y)

	out <- TRUE
	if (xloc != yloc) {
		if (fail) stop('The rasters have different GRASS locations.')
		out <- FALSE
	}

	if (xmapset != ymapset) {
		if (fail) stop('The rasters have different GRASS mapsets.')
		out <- FALSE
	}

	if (xcrs != ycrs) {
		if (fail) stop('The rasters have different coordinate reference systems.')
		out <- FALSE
	}

	if (any(compareFloat(xext, yext, '!='))) {
		if (fail) stop('The rasters have different extents.')
		out <- FALSE
	}

	if (all(xdim != ydim)) {
		if (fail) stop('The rasters have different dimensions.')
		out <- FALSE
	}

	if (all(xtopology != ytopology)) {
		if (fail) stop('The rasters have different topologies.')
		out <- FALSE
	}

	xna <- is.na(xdepth)
	yna <- is.na(ydepth)
	
	if (!all(xna) | !all(yna)) {
		if (any(xdepth[!xna] != ydepth[!yna])) {
			if (fail) stop('The rasters have different depths.')
			out <- FALSE
		}
	}

	xna <- is.na(xzext)
	yna <- is.na(yzext)
	
	if (!all(xna) | !all(yna)) {
		if (any(compareFloat(xzext[!xna], yzext[!yna], '!='))) {
			if (fail) stop('The rasters have different vertical extents.')
			out <- FALSE
		}
	}
	
	out
	
	} # EOF
)

setMethod(f = 'comparable',
	signature = c('GRaster', 'GVector'),
	definition = function(x, y, fail = FALSE) {

	xloc <- location(x)
	xmapset <- mapset(x)
	xcrs <- st_crs(x)
	xtopology <- topology(x)
	xzext <- zExt(x)

	yloc <- location(y)
	ymapset <- mapset(y)
	ycrs <- st_crs(y)
	ytopology <- topology(y)
	yzext <- zExt(y)

	out <- TRUE
	if (xloc != yloc) {
		if (fail) stop('The raster and vector have different GRASS locations.')
		out <- FALSE
	}

	if (xmapset != ymapset) {
		if (fail) stop('The raster and vector have different GRASS mapsets.')
		out <- FALSE
	}

	if (xcrs != ycrs) {
		if (fail) stop('The raster and vector have different coordinate reference systems.')
		out <- FALSE
	}

	if (all(xtopology != ytopology)) {
		if (fail) stop('The raster and vector have different topologies.')
		out <- FALSE
	}

	xna <- is.na(xzext)
	yna <- is.na(yzext)
	
	if (!all(xna) | !all(yna)) {
		if (any(compareFloat(xzext[!xna], yzext[!yna], '!='))) {
			if (fail) stop('The raster and vector have different vertical extents.')
			out <- FALSE
		}
	}
	
	out
	
	} # EOF
)

setMethod(f = 'comparable',
	signature = c('GVector', 'GRaster'),
	definition = function(x, y, fail = FALSE) {
		comparable(y, x, fail = fail)
	} # EOF
)

setMethod(f = 'comparable',
	signature = c('GVector', 'GVector'),
	definition = function(x, y, fail = FALSE) {

	xloc <- location(x)
	xmapset <- mapset(x)
	xcrs <- st_crs(x)
	xtopology <- topology(x)
	xzext <- zExt(x)

	yloc <- location(y)
	ymapset <- mapset(y)
	ycrs <- st_crs(y)
	ytopology <- topology(y)
	yzext <- zExt(y)

	out <- TRUE
	if (xloc != yloc) {
		if (fail) stop('The vectors have different GRASS locations.')
		out <- FALSE
	}

	if (xmapset != ymapset) {
		if (fail) stop('The vectors have different GRASS mapsets.')
		out <- FALSE
	}

	if (xcrs != ycrs) {
		if (fail) stop('The vectors have different coordinate reference systems.')
		out <- FALSE
	}

	if (all(xtopology != ytopology)) {
		if (fail) stop('The vectors have different topologies.')
		out <- FALSE
	}

	xna <- is.na(xzext)
	yna <- is.na(yzext)
	
	if (!all(xna) | !all(yna)) {
		if (any(compareFloat(xzext[!xna], yzext[!yna], '!='))) {
			if (fail) stop('The vectors have different vertical extents.')
			out <- FALSE
		}
	}
	
	out
	
	} # EOF
)
