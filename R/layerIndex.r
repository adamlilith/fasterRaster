#' Get index of raster layers
#'
#' @param layer Integer, numeric, logical, or character: Refers to one or more layers.
#'
#' @param x A `GRaster`.
#'
#' @param recycle Logical: If `TRUE` (default), and `layer` is logical and smaller in number than the number of layers, then recycle the vector of `layer`.
#'
#' @returns An integer vector.
#'
#' @noRd
.layerIndex <- function(layer, x, recycle = TRUE) {

	if (length(layer) > nlyr(x)) stop("Raster only contains ", nlyr(x), " layer(s).")

	if (is.character(layer)) {
		layer <- match(layer, names(x))
		if (anyNA(layer)) stop("Layer not found.")
	} else if (is.logical(layer)) {
		if (recycle && length(layer) < nlyr(x)) layer <- rep(layer, length.out = nlyr(x))
		layer <- which(layer)
	}
	if (any(!(layer %in% seq_len(nlyr(x))))) stop("Raster only contains ", nlyr(x), " layer(s).")
	layer

}
