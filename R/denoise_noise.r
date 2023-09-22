#' Remove or retain "noise" in a raster using PCA
#'
#' @description `denoise()` applies a principal component analysis (PCA) to layers of a `GRaster`, then uses the PCA to predict values back to a raster. This retains only coarse-scale trends, thereby removing "noise" (locally extreme values that fall far from a PC axis).
#' 
#' `noise()` does the opposite by first constructing the PCA, predicting values back to the raster, then subtracting these values from the original, removing coarse-scale trends and thereby leaving "noise".
#' 
#' @param x A `GRaster` with two or more layers.
#' 
#' @param scale Logical: If `TRUE`, input layers will be rescaled by dividing each layer by its overall population standard deviation. Note that rasters will always be centered (have their mean subtracted from values). Centering and scaling is recommended when rasters values are in different units. The default is `FALSE` (do not scale).
#' 
#' @param percent Numeric integer or integer in the range 50 to 99 (default is 80): Minimum total variation explained in the retained PC axes. Higher values will cause `denoise()` to remove less noise, and `noise()` to return less noise. If this value to too low to retain even one axis, the function will fail with a message to that effect.
#' 
#' @returns A multi-layer `GRaster` with one layer per input.
#' 
#' @seealso [pca()]; [stats::prcomp()]; module `i.pca` in **GRASS**
#' 
#' @example man/examples/ex_denoise_noise.r
#' 
#' @aliases denoise
#' @rdname denoise
#' @exportMethod denoise
methods::setMethod(
	f = "denoise",
	signature = c(x = "GRaster"),
	function(x, scale = FALSE, percent = 80) {

	if (nlyr(x) < 2L) stop("Input raster must have 2 or more layers.")

	if (percent %% 1 != 0) {
		warning("Argument ", sQuote("percent"), " must be an integer. Value will be rounded.")
		percent <- round(percent)
	}

	if (percent < 50 | percent > 99) stop("Argument ", sQuote("percent"), " must be an integer in the range [50, 99].")

	.restore(x)
	region(x)

	input <- paste(sources(x), collapse = ",")
	src <- .makeSourceName("i_pca", "raster")

	args <- list(
		cmd = "i.pca",
		input = input,
		output = src,
		rescale = c(0, 0),
		percent = percent,
		flags = c("quiet", "overwrite", "f"),
		intern = TRUE
	)

	if (scale) args$flags <- c(args$flags, "n")

	# execute PCA and get rasters
	info <- do.call(rgrass::execGRASS, args = args)
	layers <- 1L:nlyr(x)
	srcs <- paste0(src, ".", layers)
	.makeGRaster(srcs, names(x))

	} # EOF
)

#' @aliases noise
#' @rdname denoise
#' @exportMethod noise
methods::setMethod(
	f = "noise",
	signature = c(x = "GRaster"),
	function(x, scale = FALSE, percent = 80) {

	denoised <- denoise(x, scale = scale, percent = 80)
	out <- x - denoised
	names(out) <- names(x)
	out

	} # EOF
)
