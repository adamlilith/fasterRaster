#' Report or change the extent, dimensions, and/or resolution of a region GRASS
#'
#' @description These functions either change the extent, dimensions, and/or resolution of a **GRASS** ["region"][tutorial_regions] or report the current region"s extent, dimensions, and/or resolution. These functions are mostly used internally and rarely of interest to most users.
#' * `region()`: All 2D and 3D aspects of a region.
#' * `regionDim()`: x- and y-dimensions.`
#' * `regionExt()`: x- and y-extent.
#' * `regionRes()`: x- and y-resolution.
#'
#' @param x Any of:
#'	* Missing (default): Reports the extent, resolution, and dimensions of the current region. All other arguments will be ignored. You can also use [ext()], [dim()], and [res()] and related functions with missing arguments.
#'	* A `GSpatial`, `GRegion`, `GRaster`, `GVector` object: Sets the region"s extent, dimensions, and/or resolution to those of the object.
#'	* A `numeric` vector. This will resize the region"s extent, resample the region"s resolution/dimensions, or both, to ensure the desired dimensions or resolution are retained:
#'       * 2 values for `regionDim()`: Number of rows and columns
#'       * 4 values for `regionExt()`: Westernmost and easternmost easting (longitude), and southernmost and northernmost northing (latitude)
#'       * 2 values for `regionRes()`: Size of cells in the x- and y-dimensions
#'
#' @param trim A `GRaster` or `NULL` (default). If a `GRaster`, then the region will be trimmed to the non-`NA` cells in this raster. `trim` can only be non-`NULL` if `x` is a `GRaster`. Ignored if `NULL`.
#' 
#' @param respect Character or `GRaster`: Indicates what aspect(s) of the current region to retain. Different functions allow for different aspect to be retained. Partial matching is used.
#' * `regionDim()`: `"extent"` or `"resolution"`.
#' * `regionExt()`: `"dimensions"` or `"resolution"`.
#' * `regionRes()`: `"extent"` or `"dimensions"`.
#' Alternatively, a `GRaster` can be supplied:
#' * `regionDim()`: New region will have same extent and resolution.
#' * `regionExt()`: New region will have same dimensions and resolution.
#' * `regionRes()`: New region will have same extent and dimensions.
#' 
#' In this case, the new region"s registration will be the same as this raster, and cell resolution will be the same
#' 
#' Note: In most cases extent cannot be retained exactly if the resolution is changed. When resolution is changed, the actual extent will be the user-supplied extent expanded by zero to one rows or zero to one columns to accommodate an integer number of cells of the desired size. The western and northern limits of the extent will be retained, while the eastern and southern limits of the extent will be moved to accommodate an integer number of columns and rows.
#'
#' @returns The value returned depends on how the function is used:
#' * If used with no arguments, `region()` returns a `GRegion` object.
#' * If used with no arguments, `regionDim()`, `regionExt()`, and `regionres()` return numeric or integer vectors.
#' * If the function is used to change reshape/resample the region, it returns a `GRegion` object reflecting the region *before* it was changed. This allows users to revert to the original region if desired.
#' 
#' @details When resizing extent, **terra** keeps the `xmin`` (west) and `ymax`` (north) the fixed and shifts `xmax` (east) and `ymin` (south) as needed. To retain as much fidelity between **fasterRaster** and **terra** as possible, these functions do the same to the region.
#'
#' @example man/examples/ex_regions.r
#'
#' @aliases region
#' @rdname region
#' @exportMethod region
methods::setMethod(
	f = "region",
	signature = "missing",
	definition = function(x) {
		
		info <- rgrass::execGRASS("g.region", flags=c("p", "3", "u"), intern=TRUE)
		
		projection <- info[grepl(info, pattern="projection:")]
		longLatProj <- grepl(projection, pattern="(Latitude-Longitude)")
		
		# horizontal extent
		n <- info[grepl(info, pattern="north:")]
		s <- info[grepl(info, pattern="south:")]
		e <- info[grepl(info, pattern="east:")]
		w <- info[grepl(info, pattern="west:")]
		
		n <- gsub(n, pattern="north:", replacement="")
		s <- gsub(s, pattern="south:", replacement="")
		e <- gsub(e, pattern="east:", replacement="")
		w <- gsub(w, pattern="west:", replacement="")
		
		n <- trimws(n)
		s <- trimws(s)
		e <- trimws(e)
		w <- trimws(w)
		
		if (longLatProj) {
			
			n <- strsplit(n, ":")[[1L]]
			s <- strsplit(s, ":")[[1L]]
			e <- strsplit(e, ":")[[1L]]
			w <- strsplit(w, ":")[[1L]]
		
			nIsSouth <- any(grepl(n, pattern="S"))
			sIsSouth <- any(grepl(s, pattern="S"))
			eIsWest <- any(grepl(e, pattern="W"))
			wIsWest <- any(grepl(w, pattern="W"))

			n <- gsub(n, pattern="N", replacement="")
			n <- gsub(n, pattern="S", replacement="")
		
			s <- gsub(s, pattern="N", replacement="")
			s <- gsub(s, pattern="S", replacement="")
		
			e <- gsub(e, pattern="E", replacement="")
			e <- gsub(e, pattern="W", replacement="")

			w <- gsub(w, pattern="E", replacement="")
			w <- gsub(w, pattern="W", replacement="")
		
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
			
		extent <- c(w, e, s, n)
		
		# vertical extent
		top <- info[grepl(info, pattern="top:")]
		bottom <- info[grepl(info, pattern="bottom:")]
		
		top <- gsub(top, pattern="top:", replacement="")
		bottom <- gsub(bottom, pattern="bottom:", replacement="")
		
		top <- trimws(top)
		bottom <- trimws(bottom)
		
		top <- as.numeric(top)
		bottom <- as.numeric(bottom)
		
		zextent <- c(top=top, bottom=bottom)
		
		# dimensions
		rows <- info[grepl(info, pattern="rows:")]
		cols <- info[grepl(info, pattern="cols:")]
		depths <- info[grepl(info, pattern="depths:")]

		rows <- gsub(rows, pattern="rows:", replacement="")
		cols <- gsub(cols, pattern="cols:", replacement="")
		depths <- gsub(depths, pattern="depths:", replacement="")
		
		rows <- trimws(rows)
		cols <- trimws(cols)
		depths <- trimws(depths)
		
		rows <- as.numeric(rows)
		cols <- as.numeric(cols)
		depths <- as.numeric(depths)
		
		dims <- c(rows=rows, cols=cols, depths=depths)
		dims <- as.integer(dims)
		
		# resolution
		ewres <- info[grepl(info, pattern="ewres:")]
		nsres <- info[grepl(info, pattern="nsres:")]
		tbres <- info[grepl(info, pattern="tbres:")]

		ewres <- gsub(ewres, pattern="ewres:", replacement="")
		nsres <- gsub(nsres, pattern="nsres:", replacement="")
		tbres <- gsub(tbres, pattern="tbres:", replacement="")
		
		ewres <- trimws(ewres)
		nsres <- trimws(nsres)
		tbres <- trimws(tbres)
		
		tbres <- as.numeric(tbres)
		
		if (longLatProj) {
		
			ewres <- strsplit(ewres, ":")[[1L]]
			nsres <- strsplit(nsres, ":")[[1L]]
		
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
		
		GRegion(
			location = location(),
			mapset = mapset(),
			crs = crs(),
			topology = "3D",
			extent = extent,
			zextent = zextent,
			sources = NA_character_,
			dimensions = dims,
			resolution = resol
		)

	} # EOF
)

#' @rdname region
#' @aliases region
#' @exportMethod region
methods::setMethod(
	f = "region",
	signature = "SpatRaster",
	definition = function(x) {
		
	initials <- region()

	dims <- dim(x)
	rows <- dims[1L]
	cols <- dims[2L]

	extent <- as.vector(terra::ext(x))
	w <- as.character(extent[1L])
	e <- as.character(extent[2L])
	s <- as.character(extent[3L])
	n <- as.character(extent[4L])

	args <- list(
		cmd = "g.region",
		n = n,
		s = s,
		e = e,
		w = w,
		rows = rows,
		cols = cols,
		flags=c("o", "quiet"),
		intern=TRUE
	)

	do.call(rgrass::execGRASS, args=args)
	invisible(initials)

	} # EOF
)

#' @rdname region
#' @aliases region
#' @exportMethod region
methods::setMethod(
	f = "region",
	signature = c(x = "GRegion"),
	definition = function(x) {
	
		.restore(x)
		initials <- region()

		w <- west(x, TRUE)
		e <- east(x, TRUE)
		s <- south(x, TRUE)
		n <- north(x, TRUE)
		t <- top(x, TRUE)
		b <- bottom(x, TRUE)

		rows <- nrow(x)
		cols <- ncol(x)

		tbres <- zres(x)
		tbres <- as.character(tbres)

		args <- list(
			cmd = "g.region",
			n = n, s = s, e = e, w = w,
			rows = rows, cols = cols,
			flags = c("o", "quiet"),
			intern = TRUE
		)

		if (!is.na(t)) args <- c(args, t = t)
		if (!is.na(b)) args <- c(args, b = b)
		if (!is.na(tbres)) args <- c(args, tbres = tbres)

		do.call(rgrass::execGRASS, args=args)
		invisible(initials)
		
	} # EOF
)

#' @rdname region
#' @aliases region
#' @exportMethod region
methods::setMethod(
	f = "region",
	signature = c(x = "GRaster"),
	definition = function(x, trim = NULL) {
	
	.restore(x)
	initials <- region()

	topo <- topology(x)

	if (any(topo %in% "2D") & any(topo %in% "3D")) stop("Cannot mix 2D- and 3D-rasters when defining region.")

	# ensure validity of trim raster
	if (!is.null(trim)) {
	
		trimToTopo <- topology(trim)
		if (any(!(topo %in% trimToTopo))) stop("Topology of ", sQuote("trim"), " does not match topology of ", sQuote("x"), ".")
		
		trim <- sources(trim)
		if (length(trim) != 1L) stop("Argument ", sQuote("trim"), " can have only one layer.")
	
	}

	gn <- sources(x)[1L]

	if (all(topo == "2D") & is.null(trim)) {
		rgrass::execGRASS("g.region", raster=gn, flags=c("o", "quiet"), intern=TRUE)
	} else if (all(topo == "2D") & !is.null(trim)) {
		rgrass::execGRASS("g.region", raster=gn, zoom=trim, flags=c("o", "quiet"), intern=TRUE)
	} else if (all(topo == "3D") & is.null(trim)) {
		rgrass::execGRASS("g.region", raster_3d=gn, flags=c("o", "quiet"), intern=TRUE)
	} else if (all(topo == "3D") & !is.null(trim)) {
		rgrass::execGRASS("g.region", raster_3d=gn, zoom=trim, flags=c("o", "quiet"), intern=TRUE)
	} else {
		stop("Could not reshape region.")
	}

	invisible(initials)
		
	} # EOF
)

#' @rdname region
#' @aliases region
#' @exportMethod region
methods::setMethod(
	f = "region",
	signature = c(x = "GVector"),
	definition = function(x) {
	
		.restore(x)
		initials <- region()
	
		rgrass::execGRASS("g.region", vector=sources(x), flags=c("o", "quiet"), intern=TRUE)
		invisible(initials)
		
	}
)

#################
### regionExt ###
#################

#' @rdname region
#' @aliases regionExt
#' @exportMethod regionExt
methods::setMethod(
	f = "regionExt",
	signature = "missing",
	definition = function(x) region()@extent
)

#' @rdname region
#' @aliases regionExt
#' @exportMethod regionExt
methods::setMethod(
	f = "regionExt",
	signature = "numeric",
	definition = function(x, respect) .regionExt(x, respect)
)

#' @rdname region
#' @aliases regionExt
#' @exportMethod regionExt
methods::setMethod(
	f = "regionExt",
	signature = "GSpatial",
	definition = function(x, respect) .regionExt(x, respect)
)

.regionExt <- function(x, respect) {

	respect <- pmatchSafe(respect, c("resolution", "dimensions"))

	if (inherits(x, "GSpatial")) {
		.restore(x)
		x <- ext(x, vector=TRUE)
	} else if (inherits(x, "numeric")) {
		if (length(x) != 4L) stop("Please supply a numeric vector of four values.")
		if (any(is.na(x))) stop("Extent cannot have an NA value.")
	}

	initials <- region()
	
	w <- x[1L]
	e <- x[2L]
	s <- x[3L]
	n <- x[4L]
	
	ewres <- res(initials)[1L]
	nsres <- res(initials)[2L]

	if (respect == "resolution") {

		cols <- ceiling((w - e) / ewres)
		rows <- ceiling((n - s) / nsres)

		e <- w + cols * ewres
		s <- n - rows * nsres

	} else if (respect == "dimensions") {

		cols <- ncol(initials)
		rows <- nrow(initials)

		ewres <- (e - w) / cols
		nsres <- (n - s) / rows

	}

	w <- as.character(x[1L])
	e <- as.character(x[2L])
	s <- as.character(x[3L])
	n <- as.character(x[4L])

	ewres <- as.character(ewres)
	nsres <- as.character(nsres)

	args <- list(
		cmd = "g.region",
		w = w, e = e, s = s, n = n,
		ewres = ewres, nsres = nsres,
		flags = c("o", "quiet"),
		intern = TRUE
	)

	do.call(rgrass::execGRASS, args=args)
	invisible(initials)

}

#################
### regionDim ###
#################

#' @rdname region
#' @aliases regionDim
#' @exportMethod regionDim
methods::setMethod(f = "regionDim",
	signature = c(x = "missing"),
	definition = function(x) {
		region()@dimensions
	}
)

#' @rdname region
#' @aliases regionDim
#' @exportMethod regionDim
methods::setMethod(
	f = "regionDim",
	signature = "numeric",
	definition = function(x, respect) .regionDim(x, respect)
)

#' @rdname region
#' @aliases regionDim
#' @exportMethod regionDim
methods::setMethod(
	f = "regionDim",
	signature = "GRegion",
	definition = function(x, respect) .regionDim(x, respect)
)

.regionDim <- function(x, respect) {

	respect <- pmatchSafe(respect, c("extent", "resolution"))

	if (inherits(x, "GRegion")) {
		x <- dim(x)[1L:2L]
	} else if (inherits(x, "SpatRaster")) {
		x <- dim(x)[1L:2L]
	} else if (inherits(x, "numeric")) {
		if (length(x) != 2L | any(x <= 0) |any(compareFloat(x %% 1, 0, "!="))) {
			stop("Please supply a numeric vector of two positive integer values.")
		}
	}

	initials <- region()

	rows <- x[1L]
	cols <- x[2L]

	extent <- ext(initials, vector=TRUE)
	w <- extent[["xmin"]]
	e <- extent[["xmax"]]
	s <- extent[["ymin"]]
	n <- extent[["ymax"]]

	resol <- initials@resolution
	ewres <- resol[1L]
	nsres <- resol[2L]

	if (respect == "extent") {

		ewres <- (e - w) / cols
		nsres <- (n - s) / rows

	} else if (respect == "resolution") {

		e <- w + cols * ewres
		s <- n - rows * nsres

	}

	w <- as.character(w)
	e <- as.character(e)
	s <- as.character(s)
	n <- as.character(n)

	ewres <- as.character(ewres)
	nsres <- as.character(nsres)

	args <- list(
		cmd = "g.region",
		w = w, e = e, s = s, n = n,
		rows = rows, cols = cols,
		ewres = ewres, nsres = nsres,
		flags = c("o", "quiet"),
		intern = TRUE
	)

	do.call(rgrass::execGRASS, args=args)
	invisible(initials)

}

#################
### regionRes ###
#################

#' @rdname region
#' @aliases regionRes
#' @exportMethod regionRes
methods::setMethod(f = "regionRes",
	signature = c(x = "missing"),
	definition = function(x) region()@resolution[1L:2L]
)

#' @rdname region
#' @aliases regionRes
#' @exportMethod regionRes
methods::setMethod(
	f = "regionRes",
	signature = "numeric",
	definition = function(x, respect) .regionRes(x, respect)
)

#' @rdname region
#' @aliases regionRes
#' @exportMethod regionRes
methods::setMethod(
	f = "regionRes",
	signature = "GRegion",
	definition = function(x, respect) .regionRes(x, respect)
)

# set resolution of region
.regionRes <- function(x, respect) {

	respect <- pmatchSafe(respect, c("extent", "dimensions"))

	if (inherits(x, "GRegion")) {
		x <- res(x)[1L:2L]
	} else if (inherits(x, "numeric")) {
		if (length(x) == 1L) x <- rep(x, 2L)
		if (length(x) != 2L | any(x <= 0)) {
			stop("Please supply a numeric vector of one or two positive values.")
		}
	}

	initials <- region()

	ewres <- x[1L]
	nsres <- x[2L]

	w <- west(initials)
	e <- east(initials)
	s <- south(initials)
	n <- north(initials)

	if (respect == "extent") {

		cols <- ceiling((e - w) / ewres)
		rows <- ceiling((n - s) / nsres)

		e <- w + cols * ewres
		s <- n - rows * nsres

	} else if (respect == "dimensions") {

		e <- w + ncol(initials) * ewres
		s <- n - nrow(initials) * nsres

	}

	w <- as.character(w)
	e <- as.character(e)
	s <- as.character(s)
	n <- as.character(n)

	ewres <- as.character(ewres)
	nsres <- as.character(nsres)

	args <- list(
		cmd = "g.region",
		w = w, e = e, s = s, n = n,
		ewres = ewres, nsres=nsres,
		flags = c("o", "quiet"),
		intern = TRUE
	)

	do.call(rgrass::execGRASS, args=args)
	invisible(initials)

}
