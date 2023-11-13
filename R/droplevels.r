#' Remove rows from the "levels" table of a categorical raster
#'
#' @description `droplevels()` removes levels (category values) from the "levels" table of a [categorical raster][tutorial_raster_data_types].
#'
#' @param x A `GRaster`.
#'
#' @param level `NULL`, character, numeric, integer, or logical: Level(s) to drop. If `NULL` (default), then all levels without values in the raster are dropped (this may remove the "levels" table entirely if all levels are dropped, converting the raster to an `integer`-type raster).  If a character, this is the category label(s) to drop. If numeric or integer, this is the category value(s) to drop. If logical, values that are `TRUE` are dropped.
#'
#' @param layer Numeric integers, logical vector, or character: Layer(s) to which to add or from which to drop levels.
#'
#' @returns A `GRaster`. The "levels" table of the raster is modified.
#'
#' @seealso [terra::droplevels()], [categorical rasters][tutorial_raster_data_types] in **fasterRaster**
#'
#' @example man/examples/ex_GRaster_categorical.r
#'
#' @aliases droplevels
#' @rdname droplevels
#' @exportMethod droplevels
methods::setMethod(
	f = "droplevels",
	signature = c(x = "GRaster"),
	function(x, level = NULL, layer = 1) {
	
	layer <- .layerIndex(layer, x, recycle = TRUE)
	
	levs <- cats(x)
	isFact <- is.factor(x)
	for (i in layer) {

		if (isFact[i]) {

			# remove all non-extant levels
			if (is.null(level)) {

			    freqs <- freq(x[[i]])

				data.table::setkeyv(freqs, names(freqs)[1L])
				data.table::setkeyv(levs[[i]], names(levs[[i]])[1L])

				cols <- names(levs[[i]])

    			# x@levels[[i]] <- levs[[i]][unique(levs[[i]][freqs$value != 0L, which = TRUE]), ]
    			x@levels[[i]] <- levs[[i]][freqs$count != 0L]

			} else if (is.character(level)) {
			
				ac <- activeCat(x, layer = i, names = TRUE)

				x@levels[[i]] <- levs[[i]][levs[[i]][ , !(get(ac) %in% levels)]]
			
			} else if (is.logical(level)) {
			
				if (length(level) < nrow(levs[[i]])) {

					level <- rep(level, length.out = nrow(levs[[i]]))
				}

    			x@levels[[i]] <- levs[[i]][!level]
			
			} else if (is.numeric(levels) | is.integer(levels)) {
			
				drops <- which(x@levels[[i]][[1L]] %in% levels)
				x@levels[[i]] <- dropRows(x@levels[[i]], drops = drops)
			
			}

			if (nrow(x@levels[[i]]) == 0L) x@activeCat[i] <- NA_integer_

		} # if this layer has levels

	} # next raster
	methods::validObject(x)
	x
	
	} # EOF
)

droplevels <- function(x) UseMethod("droplevels", x)
