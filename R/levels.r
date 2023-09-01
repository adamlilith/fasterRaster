#' Set and get categories for categorical rasters
#'
#' @description `GRaster`s can represent [categorical data][tutorial_raster_data_types]. Cell values are actually integers, each corresponding to a category, such as "desert" or "wetland." A categorical raster is associated with a table that matches each value to a category name. The table must be `NULL` (i.e., no categories--so not a categorical raster), or have at least two columns. The first column must have integers and represent raster values. One or more subsequent columns must have category labels.
#'
#' Several functions help manage categorical rasters:
#'
#' * `levels()`: Reports category values and their labels. Only the values and active column is reported.
#' * `cats()`: Reports category values and their labels. The entire table is returned.
#' * `levels() <-`: Assign category values and their labels to all layers.
#' * `categories()`: Assign category values and their labels to specific layers.
#' * [nlevels()]: Number of levels in each raster.
#' * [activeCat()]: Retrieve the column index or name of the category labels.
#' * [activeCat()<-]: Set the column index or name of the category labels.
#'
#' @param x A `GRaster`.
#'
#' @param value A `data.frame`, `data.table`, a list of `data.frames` or `data.tables` with one per raster layer, or a categorical `SpatRaster`. The table's first column is the "value" column and must contain numeric values (of class `numeric` or `character`). If a `SpatRaster` is supplied, then its categories will be transferred to the `GRaster`.
#'
#' @returns Values returned are:
#' `levels`: A list of `data.frame`s or `data.table`s, one per raster.
#' `cats`: A list of `data.frame`s or `data.table`s, one per raster.
#' `levels <-`: A categorical `GRaster`.
#' `categories`: A categorical `GRaster`.
#'
#' @seealso [nlevels()], [terra::cats()], [terra::levels()], [terra::addCats()], [terra::droplevels()], [categorical rasters][tutorial_raster_data_types]
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

		for (i in seq_along(out)) {

			if (numLevels[i] > 0L) {
			
				activeCat <- x@activeCat[[i]]
				out[[i]] <- out[[i]][ , c(1L, ..activeCat)]

			}
		}

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
	
	out <- x@levels[layer]
	numLevels <- nlevels(x)
	for (i in seq_along(out)) {
		if (numLevels[i] == 0L) out[[i]] <- ""
	}

	if (!getFastOptions("useDataTable")) {
		for (i in seq_along(out)) {
			if (numLevels[i] > 0L) out[[i]] <- as.data.frame(out[[i]])
		}
	}
	out

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
	
	} # EOF
)

#' @aliases levels<-
#' @rdname levels
#' @exportMethod levels<-
methods::setMethod(
	f = "levels<-",
	signature = c(x = "GRaster", value = "SpatRaster"),
	function(x, value) {

	value <- levels(value)
	levels(x) <- value
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

	if (length(value) != nlyr(x)) stop("The number of level tables is not the same as the number of raster layers. \n  If you want to change the level table of select layers, use categories().")

	for (i in seq_along(value)) {

		# convert empty strings to NULL data.tables
		if (is.character(value[[i]]) && value[[i]] == "") {

			value[[i]] <- data.table::data.table(NULL)

		# convert to data.table
		} else if (!inherits(value[[i]], "data.table")) {
			
			value[[i]] <- data.table::as.data.table(value[[i]])

		}

		# set value to integer and sort by value
		if (!is.character(value[[i]])) {
			
			names <- names(value[[i]])
			valueCol <- names[1L]

			# convert first column to integer
			value[[i]][, (valueCol) := lapply(.SD, as.integer), .SDcols = valueCol]

			# sort by first column
   			data.table::setorderv(value[[i]], col = valueCol)

		}

	}

	x@activeCat <- rep(2L, nlyr(x))
	x@levels <- value
 	validObject(x)
	x

	} # EOF
)

#' @aliases categories
#' @rdname levels
#' @exportMethod categories
methods::setMethod(
	f = "categories",
	signature = c(x = "GRaster"),
	function(x, layer = 1, value, active = 1) {

	if (!is.list(value)) value <- list(value)

	if (length(list) != length(layer)) stop("The number of level tables is not the same as the number of raster layers.")

	for (i in seq_along(value)) {

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
			value[[i]][, (valueCol) := lapply(.SD, as.integer), .SDcols = valueCol]

			# sort by first column
			data.table::setorderv(value[[i]], col = valueCol)
		}

	}

	for (i in layer) {
		x@levels[[i]] <- value[[i]]
		x@activeCat[i] <- 2L
	}
	validObject(x)
	x

	} # EOF
)
