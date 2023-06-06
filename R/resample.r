#' Change the cell size of a GRaster
#'
#' @description `resample()` changes the cell size (resolution) of a `GRaster` using either another raster as a template or a user-defined resolution. Note that the extent of the output raster may be expanded to accommodate an integer number of cells.
#'
#' @param x The `GRaster` to resample.
#'
#' @param y Either a `GRaster` to serve as a template, or a numeric vector with two or three values. If a numeric vector, the values represent east-west and north-south resolution for 2D rasters, or east-west, north-south, and top-bottom resolution for 3D rasters.
#'
#' @param method Character or `NULL`: Method to use to assign values to cells. Partial matching is used.
#' * `NULL` (default): Automatically choose based on raster properties (`near` for categorical data, `bilinear` for continuous data)
#' * `'near'`: Nearest neighbor. Best for categorical data, and often a poor choice for continuous data.  If [ncat()] is >0 for all rasters, this method will be used by default.
#' * `'bilinear'`: Bilinear interpolation (default for non-categorical data; uses weighted values from 4 cells).
#' * `'bicubic'`: Bicubic interpolation (uses weighted values from 16 cells).
#' * `'lanczos'`: Lanczos interpolation (uses weighted values from 25 cells).
#'
#' @returns A `GRaster`.
#' 
#' @seealso [terra::resample()]
#'
#' @example man/examples/ex_resample.r
#'
#' @aliases resample 
#' @rdname resample 
#' @exportMethod resample
methods::setMethod(
	f = 'resample',
	signature = c(x = 'GRaster', y = 'GRaster'),
	definition = function(x, y, method = NULL) .resample(x=x, y=y, method=method)
)

#' @aliases resample 
#' @rdname resample 
#' @exportMethod resample
methods::setMethod(
	f = 'resample',
	signature = c(x = 'GRaster', y = 'numeric'),
	definition = function(x, y, method = NULL) .resample(x=x, y=y, method=method)
)

.resample <- function(x, y, method = NULL) {

	# method
	if (!is.null(method)) method <- pmatchSafe(method, c('nearest', 'bilinear', 'bicubic', 'lanczos'))
	if (is.null(method)) {
		method <- if (all(ncat(x) > 0)) {
			'bilinear'
		} else if (all(ncat(x) == 0)) {
			'nearest'
		}
	}
	
	# reshape region
	if (inherits(y, 'GRaster')) {

		ewres <- ewres(y)
		nsres <- nsres(y)
		zres <- zres(y)
		
		if (is.3d(x) & is.3d(y)) {
			t <- top(y)
			b <- bottom(y)
		}
		
	} else {
	
		ewres <- y[1L]
		nsres <- y[2L]
		tbres <- if (length(y) == 3L) {
			y[3L]
		} else {
			NA_real_
		}

		t <- top(x)
		b <- bottom(x)
	
	}
	
	w <- west(x)
	e <- east(x)
	s <- south(x)
	n <- north(x)

	cols <- ceiling((e - w) / ewres)
	rows <- ceiling((n - s) / nsres)

	e <- w + cols * ewres
	s <- n - rows * nsres

	w <- as.character(w)
	e <- as.character(e)
	s <- as.character(s)
	n <- as.character(n)
	t <- as.character(t)
	b <- as.character(b)

	ewres <- as.character(ewres)
	nsres <- as.character(nsres)
	tbres <- as.character(tbres)


	args <- list(
		cmd = 'g.region',
		n = n, s = s, e = e, w = w,
		ewres = ewres, nsres = nsres,
		flags = c('o', 'quiet'),
		intern = TRUE
	)
	if (!is.na(t)) args <- c(args, t=t)
	if (!is.na(b)) args <- c(args, t=b)
	if (!is.na(tbres)) args <- c(args, tbres=tbres)

	do.call(rgrass::execGRASS, args=args)
	
	# resample
	if (method == 'nearest') {
	
		args <- list(
			cmd = 'r.resample',
			input = NA,
			output = NA,
			flags = c('quiet', 'overwrite'),
			intern = TRUE
		)
		
	} else {
	
		args <- list(
			cmd = 'r.resamp.interp',
			input = NA,
			output = NA,
			method = method,
			flags = c('quiet', 'overwrite'),
			intern = TRUE
		)
	
	}

	nLayers <- nlyr(x)
	for (i in seq_len(nLayers)) {

		gn <- .makeGname(names(x)[i], 'raster')
		args$input <- gnames(x)[i]
		args$output <- gn

		do.call(rgrass::execGRASS, args=args)
		
		this <- makeGRaster(gn, names(x)[i])
		out <- if (i == 1L) {
			this
		} else {
			c(out, this)
		}
		
	}
	
	out

}
