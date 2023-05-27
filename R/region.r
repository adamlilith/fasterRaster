#' Change or report the extent and dimensions/resolution of a region
#'
#' @description
#' This function either changes the extent and dimensions of a **GRASS** ["region"][tutorial_regions] or reports the current region's extent, resolution, and dimensions. This function is mostly used internally and rarely of direct use to most users.
#'
#' @param x Any of:
#'	* Missing (default): Reports the extent, resolution, and dimensions of the current region. All other arguments will be ignored.
#'	* A `GRaster`, `GVector`, `SpatRaster`, or `stars` raster: Sets the region's extent and dimensions to those of the raster or vector. If a raster is supplied, the region will be resized and resampled to match its extent, resolution, and dimensions. If a vector is supplied, the region will be resized and the number of cells will be increased/decreased to fully encompass this new extent, but resolution will not be changed.
#'	* A `numeric` vector. This will resize the region's extent, resample the region's resolution/dimensions, or both to ensure the desired dimensions or resolution are retained:
#'       * 4 values for `regionExt()`: Westernmost and easternmost easting (longitude), and southernmost and northernmost northing (latitude)
#'       * 2 values for `regionDim()`: Number of rows and columns
#'       * 2 values for `regionRes()`: Size of cells in the x- and y-dimensions
#'       * 2 values for `regionZExt()`: Topmost and bottom-most elevations
#'       * 3 values for `regionDim3d()`: Number of rows, columns, and depths
#'       * 3 values for `regionRes3d()`: Size of cells in the x-, y-, and z-dimensions
#'
#' @param trim A `GRaster` or `NULL` (default). If a `GRaster`, then the region will be trimmed to the non-`NA` cells in this raster. `trim` can only be non-`NULL` if `x` is a `GRaster`. Ignored if `NULL`.
#'
#' @return A list with values reflecting the region's horizontal extent, vertical extent, dimensions, and resolution. If the function is used to resize/resample the region, the values *before* resizing/resampling are returned invisibly. The function also resizes/resamples the [region][tutorial_regions].
#'
#' @example man/examples/ex_regions.r
#'
#' @aliases regionsReshape
#' @rdname regions
#' @export
#' @exportMethod region
methods::setMethod(
	f = 'region',
	signature = 'missing',
	definition = function(x) {
		
		info <- rgrass::execGRASS('g.region', flags=c('p', '3', 'u'), intern=TRUE)
		
		projection <- info[grepl(info, pattern='projection:')]
		longLatProj <- grepl(projection, pattern='(Latitude-Longitude)')
		
		# horizontal extent
		n <- info[grepl(info, pattern='north:')]
		s <- info[grepl(info, pattern='south:')]
		e <- info[grepl(info, pattern='east:')]
		w <- info[grepl(info, pattern='west:')]
		
		n <- gsub(n, pattern='north:', replacement='')
		s <- gsub(s, pattern='south:', replacement='')
		e <- gsub(e, pattern='east:', replacement='')
		w <- gsub(w, pattern='west:', replacement='')
		
		n <- trimws(n)
		s <- trimws(s)
		e <- trimws(e)
		w <- trimws(w)
		
		if (longLatProj) {
			
			n <- strsplit(n, ':')[[1L]]
			s <- strsplit(s, ':')[[1L]]
			e <- strsplit(e, ':')[[1L]]
			w <- strsplit(w, ':')[[1L]]
		
			nIsSouth <- any(grepl(n, pattern='S'))
			sIsSouth <- any(grepl(s, pattern='S'))
			eIsWest <- any(grepl(e, pattern='W'))
			wIsWest <- any(grepl(w, pattern='W'))

			n <- gsub(n, pattern='N', replacement='')
			n <- gsub(n, pattern='S', replacement='')
		
			s <- gsub(s, pattern='N', replacement='')
			s <- gsub(s, pattern='S', replacement='')
		
			e <- gsub(e, pattern='E', replacement='')
			e <- gsub(e, pattern='W', replacement='')

			w <- gsub(w, pattern='E', replacement='')
			w <- gsub(w, pattern='W', replacement='')
		
			n <- as.numeric(n)
			s <- as.numeric(s)
			e <- as.numeric(e)
			w <- as.numeric(w)
		
			if (length(n) == 3L) n <- n[1L] + n[2L] / 60 + n[3L] / 3600
			if (length(s) == 3L) s <- s[1L] + s[2L] / 60 + s[3L] / 3600
			if (length(e) == 3L) e <- e[1L] + e[2L] / 60 + e[3L] / 3600
			if (length(w) == 3L) w <- w[1L] + w[2L] / 60 + w[3L] / 3600
			
			if (nIsSouth) n <- -1 * n
			if (sIsSouth) s <- -1 * s
			if (eIsWest) e <- -1 * e
			if (wIsWest) w <- -1 * w
		
		} else {
			n <- as.numeric(n)
			s <- as.numeric(s)
			e <- as.numeric(e)
			w <- as.numeric(w)
		}
			
		extent <- c(xmin=w, xmax=e, ymin=s, ymax=n)
		
		# vertical extent
		top <- info[grepl(info, pattern='top:')]
		bottom <- info[grepl(info, pattern='bottom:')]
		
		top <- gsub(top, pattern='top:', replacement='')
		bottom <- gsub(bottom, pattern='bottom:', replacement='')
		
		top <- trimws(top)
		bottom <- trimws(bottom)
		
		top <- as.numeric(top)
		bottom <- as.numeric(bottom)
		
		zextent <- c(top=top, bottom=bottom)
		
		# dimensions
		rows <- info[grepl(info, pattern='rows:')]
		cols <- info[grepl(info, pattern='cols:')]
		depths <- info[grepl(info, pattern='depths:')]

		rows <- gsub(rows, pattern='rows:', replacement='')
		cols <- gsub(cols, pattern='cols:', replacement='')
		depths <- gsub(depths, pattern='depths:', replacement='')
		
		rows <- trimws(rows)
		cols <- trimws(cols)
		depths <- trimws(depths)
		
		rows <- as.numeric(rows)
		cols <- as.numeric(cols)
		depths <- as.numeric(depths)
		
		dims <- c(rows=rows, cols=cols, depths=depths)
		
		# resolution
		ewres <- info[grepl(info, pattern='ewres:')]
		nsres <- info[grepl(info, pattern='nsres:')]
		tbres <- info[grepl(info, pattern='tbres:')]

		ewres <- gsub(ewres, pattern='ewres:', replacement='')
		nsres <- gsub(nsres, pattern='nsres:', replacement='')
		tbres <- gsub(tbres, pattern='tbres:', replacement='')
		
		ewres <- trimws(ewres)
		nsres <- trimws(nsres)
		tbres <- trimws(tbres)
		
		tbres <- as.numeric(tbres)
		
		if (longLatProj) {
		
			ewres <- strsplit(ewres, ':')[[1L]]
			nsres <- strsplit(nsres, ':')[[1L]]
		
			ewres <- as.numeric(ewres)
			nsres <- as.numeric(nsres)
		
			if (length(ewres) == 3L) ewres <- ewres[1L] + ewres[2L] / 60 + ewres[3L] / 3600
			if (length(nsres) == 3L) nsres <- nsres[1L] + nsres[2L] / 60 + nsres[3L] / 3600
		
		} else {
			
			ewres <- as.numeric(ewres)
			nsres <- as.numeric(nsres)
			tbres <- as.numeric(tbres)
			
		}
			
		resol <- c(xres=ewres, yres=nsres, zres=tbres)
		
		list(extent = extent, zextent = zextent, dim = dims, res = resol)
		
	}
)

.regionReshapeFromRObject <- function(x) {

	initials <- region()

	extent <- as.vector(terra::ext(x))
	dims <- dim(x)
	
	w <- as.character(extent[1L])
	e <- as.character(extent[2L])
	s <- as.character(extent[3L])
	n <- as.character(extent[4L])

	rows <- dims[1L]
	cols <- dims[2L]
	
	rgrass::execGRASS('g.region', n=n, s=s, e=e, w=w, rows=rows, cols=cols, flags=c('o', 'quiet'), intern=TRUE)
	invisible(initials)

}

#' @rdname regions
#' @aliases region
#' @exportMethod region
methods::setMethod(f = 'region', signature = 'SpatRaster', definition = function(x) .regionReshapeFromRObject(x))

#' @rdname regions
#' @aliases region
#' @exportMethod region
methods::setMethod(f = 'region', signature = 'stars', definition = function(x) .regionReshapeFromRObject(x))

#' @rdname regions
#' @aliases region
#' @exportMethod region
methods::setMethod(
	f = 'region',
	signature = c(x = 'GRaster'),
	definition = function(x, trim = NULL) {
	
		.restore(x)
		initials <- region()
	
		topo <- topology(x)
		gn <- gnames(x)

		if (any(topo %in% '2D') & any(topo %in% '3D')) stop('Cannot mix 2D- and 3D-rasters when defining region.')

		# ensure validity of trim raster
		if (!is.null(trim)) {
		
			trimToTopo <- topology(trim)
			if (any(!(topo %in% trimToTopo))) stop('Topology of ', sQuote('trim'), ' does not match topology of ', sQuote('x'), '.')
			
			trim <- gnames(trim)
			if (length(trim) != 1L) stop('Argument ', sQuote('trim'), ' can have only one layer.')
		
		}

		if (all(topo == '2D') & is.null(trim)) {
			rgrass::execGRASS('g.region', raster=gn, flags=c('o', 'quiet'), intern=TRUE)
		} else if (all(topo == '2D') & !is.null(trim)) {
			rgrass::execGRASS('g.region', raster=gn, zoom=trim, flags=c('o', 'quiet'), intern=TRUE)
		} else if (all(topo == '3D') & is.null(trim)) {
			rgrass::execGRASS('g.region', raster_3d=gn, flags=c('o', 'quiet'), intern=TRUE)
		} else if (all(topo == '3D') & !is.null(trim)) {
			rgrass::execGRASS('g.region', raster_3d=gn, zoom=trim, flags=c('o', 'quiet'), intern=TRUE)
		} else {
			stop('Could not reshape region.')
		}
	
		invisible(initials)
		
	}
)

#' @rdname regions
#' @aliases region
#' @exportMethod region
methods::setMethod(
	f = 'region',
	signature = c(x = 'GVector'),
	definition = function(x) {
	
		.restore(x)
		initials <- region()
	
		topo <- topology(x)
		gn <- gnames(x)

		rgrass::execGRASS('g.region', vector=gn, flags=c('o', 'quiet'), intern=TRUE)
		invisible(initials)
		
	}
)

#' @rdname regions
#' @aliases regionExt
#' @exportMethod regionExt
methods::setMethod(f = 'regionExt',
	signature = c(x = 'missing'),
	definition = function(x) {
		region()$extent
	}
)

#' @rdname regions
#' @aliases regionExt
#' @exportMethod regionExt
methods::setMethod(f = 'regionExt', signature = 'numeric', definition = function(x) .regionExt(x))

#' @rdname regions
#' @aliases regionExt
#' @exportMethod regionExt
methods::setMethod(f = 'regionExt', signature = 'GRaster', definition = function(x) .regionExt(x))

#' @rdname regions
#' @aliases regionExt
#' @exportMethod regionExt
methods::setMethod(f = 'regionExt', signature = 'GVector', definition = function(x) .regionExt(x))

#' @rdname regions
#' @aliases regionExt
#' @exportMethod regionExt
methods::setMethod(f = 'regionExt', signature = 'SpatRaster', definition = function(x) .regionExt(x))

#' @rdname regions
#' @aliases regionExt
#' @exportMethod regionExt
methods::setMethod(f = 'regionExt', signature = 'SpatVector', definition = function(x) .regionExt(x))

#' @rdname regions
#' @aliases regionExt
#' @exportMethod regionExt
methods::setMethod(f = 'regionExt', signature = 'sf', definition = function(x) .regionExt(x))

#' @rdname regions
#' @aliases regionExt
#' @exportMethod regionExt
methods::setMethod(f = 'regionExt', signature = 'stars', definition = function(x) .regionExt(x))

.regionExt <- function(x) {

	if (inherits(x, 'GSpatial')) {
		.restore(x)
		x <- as.vector(ext(x))
	} else if (inherits(x, c('SpatRaster', 'SpatVector', 'sf', 'stars'))) {
		x <- as.vector(terra::ext(x))
	} else if (inherits(x, 'numeric')) {
		if (length(x) != 4L) stop('Please supply a numeric vector of four values.')
	}

	initials <- region()
	w <- as.character(x[1L])
	e <- as.character(x[2L])
	s <- as.character(x[3L])
	n <- as.character(x[4L])
	# rgrass::execGRASS('g.region', n=n, s=s, e=e, w=w, flags=c('o', 'a', 'quiet'), intern=TRUE)
	rgrass::execGRASS('g.region', n=n, s=s, e=e, w=w, flags=c('o', 'quiet'), intern=TRUE)
	
	invisible(initials)

}

#' @rdname regions
#' @aliases regionZExt
#' @exportMethod regionZExt
methods::setMethod(f = 'regionZExt',
	signature = c(x = 'missing'),
	definition = function(x) {
		region()$zextent
	}
)

#' @rdname regions
#' @aliases regionZExt
#' @exportMethod regionZExt
methods::setMethod(f = 'regionZExt', signature = 'numeric', definition = function(x) .regionZExt(x))

#' @rdname regions
#' @aliases regionZExt
#' @exportMethod regionZExt
methods::setMethod(f = 'regionZExt', signature = 'GRaster', definition = function(x) .regionZExt(x))

#' @rdname regions
#' @aliases regionZExt
#' @exportMethod regionZExt
methods::setMethod(f = 'regionZExt', signature = 'GVector', definition = function(x) .regionZExt(x))

.regionZExt <- function(x) {

	if (inherits(x, 'GSpatial')) {
		.restore(x)
		x <- as.vector(zext(x))
	} else if (inherits(x, 'numeric')) {
		if (length(x) != 2L) stop('Please supply a numeric vector of two values.')
	}

	initials <- region()
	top <- as.character(x[1L])
	bottom <- as.character(x[2L])
	rgrass::execGRASS('g.region', t=top, b=bottom, flags=c('o', 'quiet'), intern=TRUE)
	
	invisible(initials)

}

#' @rdname regions
#' @aliases regionDim
#' @exportMethod regionDim
methods::setMethod(f = 'regionDim',
	signature = c(x = 'missing'),
	definition = function(x) {
		region()$dim
	}
)

#' @rdname regions
#' @aliases regionDim
#' @exportMethod regionDim
methods::setMethod(f = 'regionDim', signature = 'numeric', definition = function(x) .regionDim(x))

#' @rdname regions
#' @aliases regionDim
#' @exportMethod regionDim
methods::setMethod(f = 'regionDim', signature = 'GRaster', definition = function(x) .regionDim(x))

#' @rdname regions
#' @aliases regionDim
#' @exportMethod regionDim
methods::setMethod(f = 'regionDim', signature = 'SpatRaster', definition = function(x) .regionDim(x))

#' @rdname regions
#' @aliases regionDim
#' @exportMethod regionDim
methods::setMethod(f = 'regionDim', signature = 'stars', definition = function(x) .regionDim(x))

.regionDim <- function(x) {

	if (inherits(x, 'GRaster')) {
		.restore(x)
		x <- as.vector(dim(x))[1L:2L]
	} else if (inherits(x, c('SpatRaster', 'stars'))) {
		x <- as.vector(dim(x))[1L:2L]
	} else if (inherits(x, 'numeric')) {
		if (length(x) != 2L) stop('Please supply a numeric vector of two integer values.')
	}

	initials <- region()
	rows <- x[1L]
	cols <- x[2L]
	rgrass::execGRASS('g.region', rows=rows, cols=cols, flags=c('o', 'quiet'), intern=TRUE)
	
	invisible(initials)

}

#' @rdname regions
#' @aliases regionRes
#' @exportMethod regionRes
methods::setMethod(f = 'regionRes',
	signature = c(x = 'missing'),
	definition = function(x) {
		region()$res
	}
)

#' @rdname regions
#' @aliases regionRes
#' @exportMethod regionRes
methods::setMethod(f = 'regionRes', signature = 'numeric', definition = function(x) .regionRes(x))

#' @rdname regions
#' @aliases regionRes
#' @exportMethod regionRes
methods::setMethod(f = 'regionRes', signature = 'GRaster', definition = function(x) .regionRes(x))

#' @rdname regions
#' @aliases regionRes
#' @exportMethod regionRes
methods::setMethod(f = 'regionRes', signature = 'SpatRaster', definition = function(x) .regionRes(x))

#' @rdname regions
#' @aliases regionRes
#' @exportMethod regionRes
methods::setMethod(f = 'regionRes', signature = 'stars', definition = function(x) .regionRes(x))

.regionRes <- function(x) {

	if (inherits(x, 'GRaster')) {
		.restore(x)
		x <- if (is.2d(x)) {
			res(x)
		} else if (is.3d(x)) {
			res3d(x)
		}
	} else if (inherits(x, c('SpatRaster', 'stars'))) {
		x <- terra::res(x)
	} else if (inherits(x, 'numeric')) {
		if (!(length(x) %in% c(2L, 3L))) stop('Please supply a numeric vector of two or three values.')
	}

	initials <- region()

	ewres <- as.character(x[1L])
	nsres <- as.character(x[2L])

	if (length(x) == 2L) {
		# rgrass::execGRASS('g.region', ewres=ewres, nsres=nsres, flags=c('o', 'a', 'quiet'), intern=TRUE)
		rgrass::execGRASS('g.region', ewres=ewres, nsres=nsres, flags=c('o', 'quiet'), intern=TRUE)
	} else if (length(x) == 3L) {
		tbres <- x[3L]
		rgrass::execGRASS('g.region', ewres=ewres, nsres=nsres, tbres=tbres, flags=c('o', 'quiet'), intern=TRUE)
	}
	
	invisible(initials)

}
