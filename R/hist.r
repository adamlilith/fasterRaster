#' Plot a histogram of raster values
#'
#' @description This function creates a histogram of values in `GRaster`. The function is modeled after [graphics::hist()], but actually uses [graphics::barplot()].
#'
#' @param x A `GRaster`.
#'
#' @param layer Character, numeric, or integer: Indicates which layer of a multi-layer `GRaster` for which to plot a histogram. The layer can be identified using its [name()] (character) or index (numeric or integer). If this is missing, then up to `maxnl` layers are plotted.
#'
#' @param maxnl Maximum number of layers for which to create histograms. This is 16 by default, but ignored if `layer` is defined.
#'
#' @param bins Positive numeric integer: Number of bins in which to divide values of a raster with continuous values. For `integer` and categorical rasters, each value is tallied.
#'
#' @param freq Logical: If `TRUE` (default), plot the frequency of values. If `FALSE`, plot the density of values (i.e., the number in each bin divided by the total number of cells with non-`NA` values).
#'
#' @param ... Arguments to pass to [graphics::barplot()].
#'
#' @returns A named list of `data.frame`s (invisibly), one per layer plotted, and creates a graph.
#'
#' @example man/examples/ex_plot.r
#'
#' @importFrom graphics par
#'
#' @aliases hist
#' @rdname hist
#' @exportMethod hist
methods::setMethod(
	f = "hist",
	signature = c(x = "GRaster"),
	function(x, layer, maxnl = 16, bins = 30, freq = TRUE, ...) {

	if (!missing(layer)) {
		layer <- .layerIndex(layer = layer, x = x)
	} else {
		layer <- seq(1L, min(maxnl, nlyr(x)))
	}
	numLayers <- length(layer)
	
	x <- x[[layer]]

	freqs <- freq(x, bins = bins)
	if (length(layer) == 1L) freqs <- list(freqs)

	# plot
	ncols <- ceiling(sqrt(numLayers))
	nrows <- ceiling(numLayers / ncols)
	pars <- graphics::par()
	on.exit(graphics::par(pars), add = TRUE)

	graphics::par(mfrow = c(nrows, ncols))
	for (i in seq_along(layer)) {
	
		if (!inherits(freqs[[i]], "data.table")) freqs[[i]] <- as.data.table(freqs[[i]])

		if (is.factor(x[[i]])) {
			xs <- levels(x[[i]])[[2L]]
			las <- 3
			xlab <- ""
		} else if (is.cell(x[[i]])) {
   			xs <- freqs[[i]][["value"]]
			las <- 0
			xlab <- "Value"
		} else {

			TEMPTEMP_mean_ <- NULL
   			data.table::setDT(freqs[[i]])[ , TEMPTEMP_mean_ := rowMeans(.SD, na.rm = TRUE), .SDcols = c("from", "to")]

			xs <- freqs[[i]][["TEMPTEMP_mean_"]]
			las <- 0
			xlab <- "Mean bin value"
		
		}
		
		ys <- freqs[[i]][["count"]]
		if (!freq) ys <- ys / sum(ys)
		
		ylab <- if (freq) { "Frequency"} else { "Density" }
		
		graphics::barplot(ys, main = names(x)[i], xlab = xlab, ylab = ylab, names.arg = xs, las = las, ...)
	
	}
	
	invisible(freqs)
	
	} # EOF
)
