#' Remove or retain "noise" in a raster using PCA
#'
#' @description `denoise()` applies a principal component analysis (PCA) to layers of a `GRaster`, then uses the PCA to predict values back to a raster. This retains only coarse-scale trends, thereby removing "noise" (locally extreme values that fall far from a PC axis).
#' 
#' `noise()` does the opposite by first constructing the PCA, predicting values back to the raster, then subtracting these values from the original, removing coarse-scale trends and thereby leaving "noise".
#' 
#' Note that by adding each raster layer from the output of `denoise()` to each raster layer from `noise()`, you will get the original input rasters back. Thus, these two functions together simply decompose signals in the input between coarse-scale and fine-scale trends.
#' 
#' @param x A `GRaster` with two or more layers.
#' 
#' @param scale Logical: If `TRUE`, input layers will be rescaled by dividing each layer by its overall population standard deviation. Note that rasters will always be centered (have their mean subtracted from values). Centering and scaling is recommended when rasters values are in different units. The default is `FALSE` (do not scale).
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
	function(x, scale = FALSE) {

	if (nlyr(x) < 2L) stop("Input raster must have 2 or more layers.")

	.restore(x)
	region(x)

	input <- paste(sources(x), collapse = ",")
	src <- .makeSourceName("i_pca", "raster")

	args <- list(
		cmd = "i.pca",
		input = input,
		output = src,
		rescale = c(0, 0),
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
	function(x, scale = FALSE) {

	denoised <- denoise(x, scale = scale)
	out <- x - denoised
	names(out) <- names(x)
	out

	} # EOF
)
