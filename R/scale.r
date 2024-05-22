#' Center and scale a GRaster, or the opposite
#'
#' @description The `scale()` function centers and scales layers in a `GRaster` by subtracting from each raster its mean value (centering), then dividing by its standard deviation (scaling). This is useful for using the raster in a linear model, for example, because unscaled predictors can lead to numerical instability.
#'
#' The `unscale()` function does the opposite of `scale()`: it multiples each layer by a value (presumably, its standard deviation), and adds another value (presumably, its mean).
#'
#' @param x A `GRaster`.
#'
#' @param center Value depends on the function:
#' * `scale()`: Logical: If `TRUE` (default), subtract from each raster layer its mean.
#' * `unscale()`: Numeric vector or `NULL` (default): This can be a single value, which will be recycled if there is more than one layer in the raster, or one value per raster layer. If a value is `NA`, then no un-centerinng will be performed on the relevant raster layer. If `NULL`, then no un-centering is done.
#'
#' @param scale Value depends on the function:
#' * `scale()`: Logical: If `TRUE` (default), divide each layer by its standard deviation.
#' * `unscale()`: Numeric vector or `NULL` (default): This can be a single value, which will be recycled if there is more than one layer in the raster, or one value per raster layer. If a value is `NA`, then no unscaling will be done on the relevant raster layer. If `NULL`, then no un-scaling is done.
#'
#' @returns Value depends on the function:
#' * `scale()`: The `GRaster` will have two attributes, "center" and "scale", which have the means and standard deviations of the original rasters (if `center` and `scale` are `TRUE`, otherwise, they will be `NA`). These can be obtained using `attributes(output_raster)$center` and `attributes(output_raster)$scale`.
#' * `unscale()`: A `GRaster`.
#'
#' @example man/examples/ex_scale_unscale.r
#'
#' @aliases scale
#' @rdname scale
#' @exportMethod scale
methods::setMethod(
	f = "scale",
	signature = c(x = "GRaster"),
	function(x, center = TRUE, scale = TRUE) {

	sample <- TRUE

	if (!center & !scale) {
	
		warning("No scaling performed because neither ", sQuote("center"), " nor ", sQuote("scale"), " are TRUE.")

		return(x)

	}
	
	.locationRestore(x)
	.region(x)

	if (center) {
		fx <- "mean"
	} else {
		fx <- NULL
	}

	if (scale) {
		if (sample) {
			sdfx <- "sd"
		} else{
			sdfx <- "sdpop"
		}
	} else {
		sdfx <- NULL
	}
	fx <- c(fx, sdfx)

	stats <- global(x, fx)

	nLayers <- nlyr(x)

	srcs <- .makeSourceName("scale_r_mapcalc", "raster", n = nLayers)
	for (i in seq_len(nLayers)) {
	
		if (center) mu <- stats[i, "mean"]
		if (scale) sigma <- stats[i, sdfx]

		if (center & scale) {
			ex <- paste0(srcs[i], " = (", sources(x)[i], " - ", mu, ") / ", sigma)
		} else if (center & !scale) {
			ex <- paste0(srcs[i], " = ", sources(x)[i], " - ", mu)
		} else if (!center & scale) {
			ex <- paste0(srcs[i], " = ", sources(x)[i], " / ", sigma)
		}

		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)
	
	} # next layer
	out <- .makeGRaster(srcs, names(x))

	if (center) {
		attr(out, "center") <- stats[ , "mean"]
	} else {
		attr(out, "center") <- NA_real_
	}

	if (scale) {
		attr(out, "scale") <- stats[ , sdfx]
	} else {
		attr(out, "scale") <- NA_real_
	}

	out

	} # EOF
)

#' @aliases unscale
#' @rdname scale
#' @exportMethod unscale
methods::setMethod(
	f = "unscale",
	signature = c(x = "GRaster"),
	function(x, center = NULL, scale = NULL) {

	if (is.null(center) & is.null(scale)) {
		warning("No unscaling performed because neither `center` `scale` are NULL.")
		return(x)

	}
	
	.locationRestore(x)
	.region(x)

	nLayers <- nlyr(x)

	if (!is.null(center)) {

		if (length(center) == 1L) center <- rep(center, nLayers)
		if (length(center) != nLayers) stop("The `center` argument must be a single value, one value per layer in the GRaster, or NULL.")

	}

	if (!is.null(scale)) {

		if (length(scale) == 1L) scale <- rep(scale, nLayers)
		if (length(scale) != nLayers) stop("The `scale` argument must be a single value, one value per layer in the GRaster, or NULL.")

	}

	srcs <- .makeSourceName("r_mapcalc", "raster", n = nLayers)
	for (i in seq_len(nLayers)) {
	
		if (!is.null(center)) mu <- center[i]
		if (!is.null(scale)) sigma <- scale[i]

		if (!is.null(center) & !is.null(scale)) {

			if (!is.na(mu) & !is.na(sigma)) {
				ex <- paste0(srcs[i], " = (", sources(x)[i], " * ", sigma, ") + ", mu)
			} else if (is.na(mu) & !is.na(sigma)) {
				ex <- paste0(srcs[i], " = ", sources(x)[i], " * ", sigma)
			} else if (!is.na(mu) & is.na(sigma)) {
				ex <- paste0(srcs[i], " = ", sources(x)[i], " + ", mu)
			} else if (is.na(mu) & is.na(sigma)) {
				ex <- NULL
			}

		} else if (is.null(center) & !is.null(scale)) {
		
			if (!is.na(sigma)) {
				ex <- paste0(srcs[i], " = ", sources(x)[i], " * ", sigma)
			} else {
				ex <- NULL
			}

		} else if (!is.null(center) & is.null(scale)) {

			if (!is.na(mu)) {
				ex <- paste0(srcs[i], " = ", sources(x)[i], " + ", mu)
			} else {
				ex <- NULL
			}

		}

		if (!is.null(ex)) {

			rgrass::execGRASS(
				cmd = "r.mapcalc",
				expression = ex,
				flags = c(.quiet(), "overwrite")
			)

		} else {
			srcs[i] <- sources(x)[i]
		}
	
	} # next layer
	.makeGRaster(srcs, names(x))

	} # EOF
)
