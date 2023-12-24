#' Center and scale a GRaster
#'
#' @description This function has three modes of operation:
#' * Arguments `center` or `scale` are either `TRUE` or `FALSE`: Subtracts the mean value from each layer of a `GRaster` and divides it by its standard deviation. This is useful for when rasters are used in linear models, which can become numerically unstable when predictors are on very different scales.
#' * Arguments `center` or `scale` are numeric vectors: Multiplies each layer by its corresponding `scale` value and adds the `center` value. For both `center` and `scale`, either a single value can be provided or one value per raster layer.
#' * Mixed cases of above (either `center` or `scale` is a vector and the other is either `TRUE` or `FALSE`): If `TRUE`, then the corresponding operation is conducted (subtraction of the mean or division by standard deviation). Note that the `center`(s) will always be subtracted before division by the `scale` value(s).
#'
#' @param x A `GRaster`.
#'
#' @param center Logical: If `TRUE` (default), subtract from each raster layer its mean.
#'
#' @param scale Logical: If `TRUE` (default), divide each layer by its standard deviation.
#'
#' @param sample Logical: If `TRUE` (default), use the sample standard deviation to scale (this is slower). If `FALSE`, use the population standard deviation. Note that these will be the same to several decimal places whenever the raster has more than a few thousand cells (i.e., most cases). The [base::scale()] and [terra::scale()] functions use the sample standard deviation.
#'
#' @returns A `GRaster`. The `GRaster` will have two attributes, "center" and "scale", which have the means and standard deviations of the original rasters (if `center` and `scale` are `TRUE`, otherwise, they will be `NA`). These can be obtained using `attributes(output_raster)$center` and `attributes(output_raster)$scale`.
#'
#' @example man/examples/ex_scale.r
#'
#' @aliases scale
#' @rdname scale
#' @exportMethod scale
methods::setMethod(
	f = "scale",
	signature = c(x = "GRaster"),
	function(x, center = TRUE, scale = TRUE, sample = TRUE) {

	if ((is.null(center) & is.null(scale)) || (!center & !scale)) {
	
		warning("No scaling performed. At least one of ", sQuote("center"), " and ", sQuote("scale"), " should be TRUE.")

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

	srcs <- .makeSourceName("r_mapcalc", "raster", n = nLayers)
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