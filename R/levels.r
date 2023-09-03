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
#' * `addCats()`: Add categories (rows) or columns to the levels table of a categorical raster.
#' * `missingCats()`: Identifies values that have no assigned category.
#' * [nlevels()]: Number of levels in each raster.
#' * [activeCat()]: Retrieve the column index or name of the category labels.
#' * [activeCat() <-]: Set the column index or name of the category labels.
#' * droplevels(): Removes levels that are not represented in the raster.
#'
#' @param x A `GRaster`.
#'
#' @param value A `data.frame`, `data.table`, a list of `data.frames` or `data.tables` with one per raster layer, or a categorical `SpatRaster`. The table's first column is the "value" column and must contain numeric values (of class `numeric` or `character`). If a `SpatRaster` is supplied, then its categories will be transferred to the `GRaster`.
#'
#' @param level Numeric, integer, character, or `NULL` (default): Level(s) to remove. Levels cane be specified by their integer (numeric) value or category label. `NULL` removes all levels not represented in the raster.
#'
#' @param layer Numeric integers: Layer(s) from which to drop levels.
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
		numLevels <- nlevels(x)

		for (i in seq_along(out)) {

			if (numLevels[i] > 0L) {
			
				names <- names(x@levels[[i]])
				active <- names[x@activeCat[[i]]]
				active <- names[x@activeCat[[i]]]
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
	names(out) <- names(x)[layer]
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

			# detect non-unique values
			unis <- unique(value[[i]][ , 1L])
			numUnis <- nrow(unis)
			if (numUnis < nrow(value[[i]])) stop("The value column (the first column) must have unique values.")

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
		x@activeCat[i] <- 2L
	}
	validObject(x)
	x

	} # EOF
)

#' @aliases addCats
#' @rdname levels
#' @exportMethod addCats
methods::setMethod(
	f = "addCats",
	signature = c(x = "GRaster"),
	function(x, value, merge = FALSE, layer = 1) {

	if (inherits(value, "data.frame", which = TRUE) != 1L) {
		value <- data.table::as.data.table(value)
	}

	if (merge) {
	
  		# levelValCol <- names(x@levels[[layer]])[1L]
  		valValCol <- names(value)[1L]

  		x@levels[[layer]] <- x@levels[[layer]][value, on = c(Code = valValCol)]

		# data.table::setkey(x@levels[[layer]], levelValCol)
		# data.table::setkey(value, valValCol)
		# x@levels[[layer]] <- value[x@levels[[layer]]]
	
	} else {
	
		x@levels[[layer]] <- cbind(x@levels[[layer]], value)
	
	}
 	validObject(x)
	x

	} # EOF
)

#' @aliases droplevels
#' @rdname levels
#' @exportMethod droplevels
methods::setMethod(
	f = "droplevels",
	signature = c(x = "GRaster"),
	function(x, level = NULL, layer = 1) {
	
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
			
			}

			if (nrow(x@levels[[i]]) == 0L) x@activeCat[i] <- NA_integer_

		} # if this layer has levels

	} # next raster
	validObject(x)
	x
	
	} # EOF
)

#' @aliases missingCats
#' @rdname levels
#' @exportMethod missingCats
methods::setMethod(
    f = "missingCats",
    signature = c(x = "GRaster"),
    function(x) {

	levs <- levels(x)
	isFact <- is.factor(x)

	out <- list()
	for (i in seq_len(nlyr(x))) {
	
		if (!isFact[i]) {
			out[[i]] <- numeric()
		} else {
		
			freqs <- freq(x[[i]])
			if (!inherits(freqs, "data.table")) freqs <- data.table::as.data.table(freqs)

			freqs <- freqs[ , 1L]
			
			data.table::setkeyv(freqs, names(freqs))
   			data.table::setkeyv(levs[[i]], names(levs[[i]])[1L])

   			missing <- freqs[!levs[[i]]]
			if (nrow(missing) == 0L) {
   				out[[i]] <- integer()
			} else {
				out[[i]] <- missing[[1L]]
			}

		} # if this layer has levels
	} # next raster
	names(out) <- names(x)
	out

    } # EOF
)

