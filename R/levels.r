#' Set and get categories for categorical rasters
#'
#' @description `GRaster`s can represent [categorical data][tutorial_raster_data_types]. Cell values are actually integers, each corresponding to a category, such as "desert" or "wetland." A categorical raster is associated with a table that matches each value to a category name. The table must be `NULL` (i.e., no categories--so not a categorical raster), or have at least two columns. The first column must have integers and represent raster values. One or more subsequent columns must have category labels. The column with these labels is the "active category".
#'
#' Several functions help manage categorical rasters. **The functions described here return information about the "levels" table(s) of a categorical raster.**
#'
#' * `levels()`: Reports category values and their labels. Only the values and active column is reported.
#' * `cats()`: Reports category values and their labels. The entire table is returned.
#' * [levels<-]: Assign category values and their labels to all layers.
#' * [categories()]: Assign category values and their labels to specific layers.
#' * [addCats<-]: Add values and categories (rows) to the levels table of a categorical raster.
#' * [addCats()]: Add new columns to a levels table using [data.table::merge()] or [cbind()]. This does not add new rows to the table.
#' * `catNames()`: Column names of each levels table.
#' * `missingCats()`: Values in the raster that have no assigned category.
#' * [nlevels()]: Number of levels in each raster.
#' * [activeCat()]: Column index or name of the category labels.
#' * [activeCat<-]: Set the column of the category labels.
#' * [droplevels()]: Removes levels that are not represented in the raster.
#' * [freq()]: Frequency of each category across cells of a raster\cr
#'
#' @param x A `GRaster`.
#'
#' @param value A `data.frame`, `data.table`, a list of `data.frames` or `data.tables` with one per raster layer, or a categorical `SpatRaster`. The table's first column is the "value" column and must contain numeric values (of class `numeric` or `character`). If a `SpatRaster` is supplied, then its categories will be transferred to the `GRaster`.
#'
#' @param layer Numeric integers, logical vector, or character: Layer(s) for which to obtain levels.
#'
#' @returns Values returned are:
#' * `levels()`: A list of `data.frame`s or `data.table`s, one per raster.
#' * `cats()`: A list of `data.frame`s or `data.table`s, one per raster.
#' * `activeCat()`: An integer vector or character vector.
#'
#' @seealso [levels<-], [catNames()], [missingCats()], [activeCat<-], [freq()], [nlevels()], [terra::cats()], [terra::levels()], [terra::addCats()], [terra::droplevels()], [categorical rasters][tutorial_raster_data_types] in **fasterRaster**
#'
#' @example man/examples/ex_GRaster_categorical.r
#'
#' @aliases levels
#' @rdname levels
#' @exportMethod levels
methods::setMethod(
    f = "levels",
    signature = c(x = "GRaster"),
    function(x) {

		out <- cats(x)
		numLevels <- nlevels(x)

		for (i in seq_along(out)) {

			if (numLevels[i] > 0L) {
			
				names <- catNames(x, i)
				active <- activeCat(x, name = TRUE)[i]
				value <- names[1L]

				cols <- c(value, active)

				out[[i]] <- out[[i]][ , ..cols]

			}
		}

		names(out) <- names(x)
        out

    } # EOF
)

#' @aliases cats
#' @rdname levels
#' @exportMethod cats
methods::setMethod(
    f = "cats",
    signature = c(x = "GRaster"),
    function(x, layer = 1:nlyr(x)) {
	
	layer <- .layerIndex(layer, x, recycle = TRUE)
	
	out <- x@levels[layer]
	numLevels <- nlevels(x)[layer]
	for (i in seq_along(out)) {
		if (numLevels[i] == 0L) out[[i]] <- NULL
	}

	if (!getFastOptions("useDataTable")) {
		for (i in seq_along(out)) {
			if (numLevels[i] > 0L) out[[i]] <- as.data.frame(out[[i]])
		}
	}
	names(out) <- names(x)[layer]
	out

    } # EOF
)

#' @aliases missingCats
#' @rdname levels
#' @exportMethod missingCats
methods::setMethod(
    f = "missingCats",
    signature = c(x = "GRaster"),
    function(x, layer = 1:nlyr(x)) {

	layer <- .layerIndex(layer, x, recycle = TRUE)

	levs <- levels(x)
	isFact <- is.factor(x)

	out <- list()
	for (i in layer) {
	
		if (!isFact[i]) {
			out[[i]] <- numeric()
		} else {
		
			freqs <- freq(x[[i]])

			ac <- activeCat(x, names = TRUE)[i]
			val <- names(freqs)[1L]

			out <- freqs[(is.na(get(ac))), get(val)]

		} # if this layer has levels
	} # next raster
	names(out) <- names(x)[layer]
	out

    } # EOF
)
