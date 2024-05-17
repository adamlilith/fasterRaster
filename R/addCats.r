#' Add rows or columns to the "levels" table of a categorical raster
#'
#' @description `addCats()` and `addCats()<-` add information to a categorical`GRaster's "levels" table.
#' * addCats()` uses [data.table::merge()] or [cbind()] to do this--it does not add new rows, but rather new columns.
#' * `addCats()<-` uses [rbind()] to add new categories (rows) to the "levels" table.
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
#' @returns A `GRaster`. The "levels" table of the raster is modified.
#'
#' @seealso [terra::addCats()], [combineCats()], [combineLevels()], [droplevels()], [categorical rasters][tutorial_raster_data_types] in **fasterRaster**
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
	signature = c(x = "GRaster"),
	function(x, layer = 1, value) {

	if (missing(value)) value <- 1L

	layer <- .layerIndex(layer, x, recycle = TRUE)

	if (!inherits(value, "data.table")) value <- data.table::as.data.table(value)

	for (i in layer) {

		val <- names(value)[1L]
		value[ , (val) := lapply(.SD, as.integer), .SDcols = val]
  		x@levels[[i]] <- rbind(x@levels[[i]], value)
		data.table::setkeyv(x@levels[[i]], val)

		if (length(unique(x@levels[[i]][[val]])) < nrow(x@levels[[i]])) stop("More than one label assigned to the same value in the levels table.")
	
	}

 	methods::validObject(x)
	x

	} # EOF
)
