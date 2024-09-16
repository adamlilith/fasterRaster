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
#' @keywords internal
.layerIndex <- function(layer, x, recycle = TRUE) {

	nx <- nlyr(x)

	if (is.character(layer)) {
		layer <- match(layer, names(x))
		if (anyNA(layer)) stop("Layer not found.")
	} else if (is.logical(layer)) {
		if (recycle && length(layer) < nx) layer <- rep(layer, length.out = nx)
		layer <- which(layer)
	} else if (inherits(layer, c("numeric", "integer"))) {
		if (any(layer > nx) | any(layer < 1L)) stop("Index out of bounds.")
	}
	if (any(!(layer %in% seq_len(nx)))) stop("Raster only contains ", nlyr(x), " layer(s).")
	layer

}
