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
	if (!is.null(method)) method <- .pmatch(method, c('nearest', 'bilinear', 'bicubic', 'lanczos'))
	if (is.null(method)) {
		method <- if (all(ncat(x) > 0)) {
			'bilinear'
		} else if (all(ncat(x) == 0)) {
			'nearest'
		}
	}
	
	# reshape region
	xExt <- as.vector(ext(x))
	n <- as.character(xExt[4L])
	s <- as.character(xExt[3L])
	e <- as.character(xExt[2L])
	w <- as.character(xExt[1L])
	
	if (inherits(y, 'GRaster')) {

		yRes <- res3d(y)
		ewres <- as.character(yRes[1L])
		nsres <- as.character(yRes[2L])
		tbres <- as.character(yRes[3L])
		
	} else {
	
		ewres <- as.character(y[1L])
		nsres <- as.character(y[2L])
		tbres <- if (length(y) == 3L) {
			as.character(y[3L])
		} else {
			NA_character_
		}
	
	}
	
	args <- list(
		cmd = 'g.region',
		n = n, s = s, e = e, w = w,
		ewres = ewres, nsres = nsres,
		flags = c('o', 'a', 'quiet'),
		intern = TRUE
	)
	
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
	gns <- .makeGname('resample', 'raster', nLayers)
	for (i in seq_len(nLayers)) {

		args$input <- gnames(x)[i]
		args$output <- gns[i]

		do.call(rgrass::execGRASS, args=args)
		
		out <- if (i == 1L) {
			makeGRaster(gns[i], names(x)[i])
		} else {
			c(out, makeGRaster(gns[i], names(x)[i]))
		}
		
	}
	
	out

}
