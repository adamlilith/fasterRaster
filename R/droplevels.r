#' Add rows or columns to the "levels" table of a categorical raster
#'
#' @description `addCats()` and `addCats()<-` add information to a categorical raster's "levels" table. The `addCats()` function uses [data.table::merge()] or [cbind()] to do this--it does not add new rows, but rather new columns. The `addCats()<-` function uses [rbind()] to add new categories (rows) to the "levels" table.
#'
#' GRaster`s can represent [categorical data][tutorial_raster_data_types]. Cell values are actually integers, each corresponding to a category, such as "desert" or "wetland." A categorical raster is associated with a "levels" table that matches each value to a category name. The table must be `NULL` (i.e., no categories--so not a categorical raster), or have at least two columns. The first column must have integers and represent raster values. One or more subsequent columns must have category labels. The column with these labels is the "active category".
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

				data.table::setkeyv(freqs, names(freqs))
				data.table::setkeyv(levs[[i]], names(levs[[i]])[1L])

				cols <- names(levs[[i]])

    			x@levels[[i]] <- levs[[i]][unique(levs[[i]][freqs, which = TRUE]), ]

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
