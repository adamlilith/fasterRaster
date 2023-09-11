#' Add rows or columns to the "levels" table of a categorical raster
#'
#' @description `addCats()` and `addCats()<-` add information to a categorical raster's "levels" table. The `addCats()` function uses [data.table::merge()] or [cbind()] to do this--it does not add new rows, but rather new columns. The `addCats()<-` function uses [rbind()] to add new categories (rows) to the "levels" table.
#'
#' GRaster`s can represent [categorical data][tutorial_raster_data_types]. Cell values are actually integers, each corresponding to a category, such as "desert" or "wetland." A categorical raster is associated with a "levels" table that matches each value to a category name. The table must be `NULL` (i.e., no categories--so not a categorical raster), or have at least two columns. The first column must have integers and represent raster values. One or more subsequent columns must have category labels. The column with these labels is the "active category".
#'
#' @param x A `GRaster`.
#'
#' @param value A `data.frame`, `data.table`, a list of `data.frames` or `data.tables` with one per raster layer, or a categorical `SpatRaster`. The table's first column is the "value" column and must contain numeric values (of class `numeric` or `character`). If a `SpatRaster` is supplied, then its categories will be transferred to the `GRaster`.
#'
#' @param merge Logical (function `addCats()`): If `FALSE` (default), columns will be combined with the existing "levels" table using [cbind()]. If `TRUE`, they will be combined using [data.table::merge()].
#'
#' @param layer Numeric integers, logical vector, or character: Layer(s) to which to add or from which to drop levels.
#'
#' @param level Character, numeric, integer, or logical vector: Level(s) to drop. If a character, then the labels are used to identify levels to drop. If numeric or integer, then the values are used. If logical, then levels that correspond to `TRUE` are dropped. For logical vectors, if the vector is shorter than the number of rows in the raster's "levels" table, the vector is recycled.
#'
#' @returns A `GRaster`. The "levels" table of the raster is modified.
#'
#' @seealso [terra::addCats()], [droplevels()], [categorical rasters][tutorial_raster_data_types] in **fasterRaster**
#'
#' @example man/examples/ex_GRaster_categorical.r
#'
#' @aliases addCats
#' @rdname addCats
#' @exportMethod addCats
methods::setMethod(
	f = "addCats",
	signature = c(x = "GRaster"),
	function(x, value, merge = FALSE, layer = 1) {

	layer <- .layerIndex(layer, x, recycle = TRUE)

	if (!inherits(value, "data.table")) {
		value <- data.table::as.data.table(value)
	}

	for (i in layer) {

		if (merge) {

			levelValCol <- names(x@levels[[i]])[1L]
			valueValCol <- names(value)[1L]
			data.table::setkeyv(value, valueValCol)

			x@levels[[i]] <- merge(x@levels[[i]], value, by.x = levelValCol, by.y = valueValCol, all.x = TRUE, allow.cartesian = FALSE)

			data.table::setkeyv(x@levels[[i]], levelValCol)

		} else {
		
			x@levels[[i]] <- cbind(x@levels[[i]], value)
		
		}
	}

 	methods::validObject(x)
	x

	} # EOF
)

#' @aliases addCats<-
#' @rdname addCats
#' @exportMethod addCats<-
methods::setMethod(
	f = "addCats<-",
	signature = c(x = "GRaster", value = "data.frame"),
	function(x, layer = 1, value) {
		.addCatsAssign(x = x, layer = 1, value = value)
	}
)

#' @aliases addCats<-
#' @rdname addCats
#' @exportMethod addCats<-
methods::setMethod(
	f = "addCats<-",
	signature = c(x = "GRaster", value = "data.table"),
	function(x, layer = 1, value) {
		.addCatsAssign(x = x, layer = 1, value = value)
	}
)

#' Worker function for addCats<-
#'
#' @param x A `GRaster`.
#' @param value A `data.frame` or `data.table`.
#'
#' @returns A `GRaster.
#'
#' @noRd
.addCatsAssign <- function(x, layer, value) {

	layer <- .layerIndex(layer, x, recycle = TRUE)

	if (!inherits(value, "data.table")) {
		value <- data.table::as.data.table(value)
	}

	for (i in layer) {

  		x@levels[[i]] <- rbind(x@levels[[i]], value)
		val <- names(x@levels[[i]])[1L]
		data.table::setkeyv(x@levels[[i]], val)
	
	}
 	methods::validObject(x)
	x

}

#' @aliases droplevels
#' @rdname addCats
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

    			x@levels[[i]] <- levs[[i]][level]
			
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
