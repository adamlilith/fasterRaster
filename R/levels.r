#' Set and get categories for categorical rasters
#'
#' @description `GRaster`s can represent [categorical data][tutorial_raster_data_types]. Cell values are actually integers, each corresponding to a category, such as "desert" or "wetland." A categorical raster is associated with a table that matches each value to a category name. The table must be `NULL` (i.e., no categories--so not a categorical raster), or have at least two columns. The first column must have integers and represent raster values. One or more subsequent columns must have category labels. The column with these labels is the "active category".
#'
#' * `levels()`: Displays the "levels" table of a raster (just the value and active category columns).
#' * `cats()`: Displays the entire "levels" table of a raster.
#' * `levels()<-`: (Re)assigns the "levels" table to each layer of a raster. Assigning a "levels" table to an integer raster makes it a categorical raster.
#' * `categories()`: (Re)assigns the "levels" table to specific layer(s) of a raster.
#' * For a complete list of functions relevant to categorical rasters, see [raster data types][tutorial_raster_data_types].
#'
#' @param x A `GRaster`.
#'
#' @param value A `data.frame`, `data.table`, a list of `data.frames` or `data.tables` with one per raster layer, or a categorical `SpatRaster`. The table's first column is the "value" column and must contain numeric values (of class `numeric` or `character`). If a `SpatRaster` is supplied, then its categories will be transferred to the `GRaster`.
#'
#' @param layer Numeric integers, logical vector, or character: For `cats()` and `categories()`, this specifies the layer(s)for which to obtain level(s).
#'
#' @param active An integer or a character: The index or column name of the column used for category labels (the "active column"). Following [terra::activeCat()], the first column of the "levels" table is ignored, so a value of 1 means to use the second column of the table for labels. A value of 2 means to use the third column, and so on.
#'
#' @returns Values returned are:
#' * `levels()` and `cats()`: A list of `data.frame`s or `data.table`s, one per raster layer.
#' * `levels()<-` and `categories()`: A `GRaster`.
#'
#' @seealso [terra::levels()], \code{\link[fasterRaster]{levels<-}}, [terra::cats()], [terra::categories()], [categorical rasters][tutorial_raster_data_types] in **fasterRaster**
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
		
			names <- catNames(x, i)[[1L]]
			active <- activeCat(x, layer = i, name = TRUE)[i]
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
	numLevels <- .nlevels(out)
	for (i in seq_along(out)) {
		if (numLevels[i] == 0L) out[[i]] <- data.table::data.table(NULL)
	}
	
	# out <- lapply(out, .replace0LevelsWithNull)

	if (!faster("useDataTable")) {
		for (i in seq_along(out)) {
			if (numLevels[i] > 0L) out[[i]] <- as.data.frame(out[[i]])
		}
	}
	names(out) <- names(x)[layer]
	out

    } # EOF
)

# #' Replaces data.table/frames in a list with 0 rows with NULL
# #'
# #' Have to do this because defining a list element as 'NULL' removes that element entirely. Fix from https://stackoverflow.com/questions/7944809/assigning-null-to-a-list-element-in-r.
# #'
# #' @param y A data.table/frame
# #'
# #' @returns A data.table/frame or 'NULL'.
# #'
# #' @noRd
# .replace0LevelsWithNull <- function(y) {

	# if (nrow(y) == 0L) {
		# NULL
	# } else {
		# y
	# }

# }

#' @aliases categories
#' @rdname levels
#' @exportMethod categories
methods::setMethod(
	f = "categories",
	signature = c(x = "GRaster"),
	function(x, layer = 1, value, active = 1) {

	layer <- .layerIndex(layer, x, recycle = TRUE)

	if (!inherits(value, "list")) value <- list(value)

	if (length(list) != length(layer)) stop("The number of level tables is not the same as the number of raster layers.")

	for (i in layer) {

		if (is.character(value[[i]]) && value[[i]] == "") {
			value[[i]] <- data.table::data.table(NULL)
		} else if (!inherits(value[[i]], "data.table")) {
   			value[[i]] <- data.table::as.data.table(value[[i]])
		}

		# set value to integer and sort by value
		if (!is.character(value[[i]])) {

			names <- names(value[[i]])
			valueCol <- names[1L]

			# convert first column to integer
			value[[i]][ , (valueCol) := lapply(.SD, as.integer), .SDcols = valueCol]

			# detect non-unique values
			unis <- unique(value[[i]][, 1L])
			numUnis <- nrow(unis)
			if (numUnis < nrow(value[[i]])) stop("The value column (the first column) must have unique values.")

			# sort by first column
			data.table::setorderv(value[[i]], col = valueCol)
		
		}

	}

	for (i in layer) {
		x@levels[[i]] <- value[[i]]
		x@activeCat[i] <- as.integer(active)
	}
	methods::validObject(x)
	x

	} # EOF
)

#' @aliases levels<-
#' @rdname levels
#' @exportMethod levels<-
methods::setMethod(
	f = "levels<-",
	signature = c(x = "GRaster", value = "data.frame"),
	function(x, value) {
	
	value <- list(value)
	levels(x) <- value
	x
	
	} # EOF
)

#' @aliases levels<-
#' @rdname levels
#' @exportMethod levels<-
methods::setMethod(
	f = "levels<-",
	signature = c(x = "GRaster", value = "data.table"),
	function(x, value) {
	
	value <- list(value)
	levels(x) <- value
	x
	
	} # EOF
)

#' @aliases levels<-
#' @rdname levels
#' @exportMethod levels<-
methods::setMethod(
	f = "levels<-",
	signature = c(x = "GRaster", value = "SpatRaster"),
	function(x, value) {

	value <- cats(value)
	levels(x) <- value
	x@activeCat <- terra::activeCat(value, layer = layer)
	x

	} # EOF
)

#' @aliases levels<-
#' @rdname levels
#' @exportMethod levels<-
methods::setMethod(
	f = "levels<-",
	signature = c(x = "GRaster", value = "GRaster"),
	function(x, value) {

	value <- cats(value)
	levels(x) <- value
	x@activeCat <- activeCat(value, layer = layer)
	x

	} # EOF
)

#' @aliases levels<-
#' @rdname levels
#' @exportMethod levels<-
methods::setMethod(
	f = "levels<-",
	signature = c(x = "GRaster", value = "list"),
	function(x, value) {
	
	if (length(value) != nlyr(x)) stop("The number of level tables is not the same as the number of raster layers.\n  If you want to change the level table of specifc layers, use categories().")

	for (i in seq_along(value)) {

		# convert empty strings to NULL data.tables
		if (is.character(value[[i]]) && value[[i]] == "") {

			value[[i]] <- data.table::data.table(NULL)

		# convert to data.table
		} else if (!inherits(value[[i]], "data.table")) {
			
			value[[i]] <- data.table::as.data.table(value[[i]])

		}

		names <- names(value[[i]])
		valueCol <- names[1L]

		# convert first column to integer
		value[[i]][ , (valueCol) := lapply(.SD, as.integer), .SDcols = valueCol]

		# detect non-unique values
		unis <- unique(value[[i]][ , 1L])
		numUnis <- nrow(unis)
		if (numUnis < nrow(value[[i]])) stop("The value column (the first column) must have unique values.")

		# sort by first column
		data.table::setorderv(value[[i]], col = valueCol)

	}

	x@activeCat <- rep(2L, nlyr(x))
	x@levels <- value
 	a <- methods::validObject(x)
	x

	} # EOF
)
