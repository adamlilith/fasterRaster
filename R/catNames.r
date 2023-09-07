#' Names of levels tables of a categorical raster
#'
#' @description This function returns the column names of each "levels" table of a [categorical raster][tutorial_raster_data_types].
#'
#' @param x A `GRaster`.
#'
#' @param layer `NULL`, numeric integer, or character: The index (indices) or name(s) of one or more raster layers. The default is `NULL`, in which case all names for all layers are returned.
#'
#' @returns A list of character vectors.
#'
#' @example man/examples/ex_GRaster_categorical.r
#'
#' @seealso [cats()], [raster data types][tutorial_raster_data_types]
#'
#' @aliases catNames
#' @rdname catNames
#' @exportMethod catNames
methods::setMethod(
	f = "catNames",
	signature = c(x = "GRaster"),
	function (x, layer = NULL) {
	
	if (is.null(layer)) layer <- seq_len(nlyr(x))
	if (is.character(layer)) layer <- match(layer, names(x))
	if (is.logical(layer)) {
		if (length(layer) != nlyr(x)) {
			layer <- rep(layer, length.out = nlyr(x))
		}
		layer <- which(layer)
	}

	categs <- x@levels[layer]
	lapply(categs, names)
	
	} # EOF
)

#' @aliases catNames
#' @rdname catNames
#' @exportMethod catNames
methods::setMethod(
	f = "catNames",
	signature = c(x = "SpatRaster"),
	function (x, layer = NULL) {
	
	if (is.null(layer)) layer <- seq_len(terra::nlyr(x))
	if (is.character(layer)) layer <- match(layer, names(x))
	if (is.logical(layer)) {
		if (length(layer) != nlyr(x)) {
			layer <- rep(layer, length.out = terra::nlyr(x))
		}
		layer <- which(layer)
	}

	categs <- terra::cats(x[[layer]])
	lapply(categs, names)
	
	} # EOF
)
