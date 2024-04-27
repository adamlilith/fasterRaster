#' Classify GRaster cell values
#'
#' @description This function classifies a raster so that cells that have values within a given range are assigned a new value. Cells that are not re-assigned a value retain their original value.
#'
#' @param x A `GRaster`.
#' @param rcl Reclassification system:
#' * A single integer: Number of "bins" into which to divide values. Arguments `include.lowest` and `right` apply.
#' * A vector of numeric values: Breakpoints of bins into which to divide values. These will be sorted from lowest to highest before classification. Arguments `include.lowest` and `right` apply.
#' * A 2-column `matrix`, `data.frame`, or `data.table`: The first column provides specific values in `x` to be replaced, and the second provides the values they are replaced with. This method is only useful for classifying [integer][tutorial_raster_data_types] `GRaster`s. Arguments `include.lowest` and `right` are ignored. Cells will be classified in the order in which values are listed in the first column.
#' * A 3-column `matrix`, `data.frame`, or `data.table`: The first column provides the lower value of a bin, the second the upper value, and the third the value to assign to the cells in the bin. Arguments `include.lowest` and `right` apply. Cells will be classified in the order of how intervals are listed (intervals will not be sorted).
#'
#' @param include.lowest,right Logical: These arguments determine how cells that have values exactly equal to the lower or upper ends of an interval are classified.
# * `include.lowest = FALSE` (default) and `right = TRUE` (also default): Intervals will be "left-open, right-closed". Cells with values equal to the lowest lower boundary will not be reclassified.
#' * `include.lowest = TRUE` and `right = TRUE`: All intervals will be "left-open, right-closed" except for the lowest interval, which will be "left-closed/right-closed".
#' * `include.lowest = FALSE` and `right = FALSE`: Intervals will be "left-closed/right-open". Cells with values equal to the highest higher boundary will not be reclassified.
#' * `include.lowest = TRUE` and `right = FALSE`: All intervals will be "left-closed/right-open", except for the highest interval, which will be "right-closed/left-closed".
#' * `right = NA`: Only useful for classifying [integer][tutorial_raster_data_types] `GRaster`s. All intervals are "left-closed/right-closed". This is easier than accounting for "open" intervals when dealing with integers. Argument `include.lowest` is ignored.
#'
#' @param others Integer or `NULL` (default), or `NA`: Value to assign to cells that do not fall into the set intervals. Cells with `NA` values are not reclassified. Setting `others` equal to `NULL` or `NA` replaces all other values with `NA`. The value will be coerced to an integer value.
#' 
#' @returns A `GRaster`. The raster will be of type "factor" if the original values were continuous (i.e., a single- or double-precision raster), or of type "integer" if the input was an integer.
#'
#' @seealso [terra::classify()]; `if ()` statements in [app()]
#'
#' @example man/examples/ex_classify.r
#'
#' @aliases classify
#' @rdname classify
#' @exportMethod classify
methods::setMethod(
	f = "classify",
	signature = c(x = "GRaster"),
	function(x, rcl, include.lowest = FALSE, right = TRUE, others = NULL) {

	if (is.na(right) & !all(is.int(x))) stop("You can only use `right = NA` for GRasters of type `integer`.")

	.locationRestore(x)
	.region(x)
	
	nLayers <- nlyr(x)

	if (!is.null(others) && !is.na(others) && !is.integer(others) && omnibus::compareFloat(others, as.integer(others), "!=")) {
		warning("Converting value of `others` to an integer.")
		others <- as.integer(others)
	}

	if (inherits(rcl, c("matrix", "data.frame", "data.table")) && ncol(rcl) == 2L) {
		if (!all(is.int(x))) stop("You can only use a 2-column matrix for `rcl` if all layers of a GRaster are of type `integer`.")
		right <- NA
	}

	### classify
	srcs <- .makeSourceName("reclass_r_mapcalc", "raster", nLayers)
	levs <- list()
	for (i in seq_len(nLayers)) {

		makeLevs <- !is.na(right) # make levels table?

		# rcl is a scalar or vector
		if (!inherits(rcl, c("matrix", "data.frame", "data.table"))) {

			# rcl is a scalar			
			if (length(rcl) == 1L) {

				if (rcl <= 1) stop("Argument `rcl` must be a value >= 2, or a vector, matrix, or data.frame.")
				mm <- minmax(x[[i]])
				fromTos <- seq(mm["min", 1L], mm["max", 1L], length.out = rcl + 1L)
				
				fromTos <- data.table::data.table(
					from = fromTos[1L:(length(fromTos) - 1L)],
					to = fromTos[2L:length(fromTos)],
					new = 1L:(length(fromTos) - 1L)
				)

			# rcl is a vector
			} else {

				rcl <- sort(rcl)
				len <- length(rcl)
				fromTos <- data.table::data.table(from = rcl[1L:(len - 1L)], to = rcl[2L:len], value = 1L:(len - 1L))
			
			}

		# rcl is a 2-column matrix
		} else if (ncol(rcl) == 2L) {
			
			if (!inherits(rcl, "data.table")) rcl <- data.table::as.data.table(rcl)
			fromTos <- data.table::data.table(from = rcl[[1L]], to = rcl[[1L]], value = rcl[[2L]])

			right <- NA
			makeLevs <- FALSE
		
		# rcl is a 2-column matrix
		} else if (ncol(rcl) == 3L) {

			if (!inherits(rcl, "data.table")) rcl <- data.table::as.data.table(rcl)
			fromTos <- rcl
			if (!is.integer(fromTos[[3L]]) && !all(omnibus::compareFloat(fromTos[[3L]], as.integer(fromTos[[3L]]), "=="))) makeLevs <- FALSE

		} else {
			stop("Argument `rcl` must be a single integer, a numeric vector, or a 2- or 3-column matrix or data.frame.")
		}

		### create r.mapcalc formula
		ex <- paste0(srcs[i], " = ")

		labels <- rep(NA_character_, nrow(fromTos))
		for (j in 1L:nrow(fromTos)) {

			if (is.na(right)) {
				lower <- ">="
				upper <- "<="
			} else if (right & include.lowest) {
				if (j == which.min(fromTos[[1L]])) {
					lower <- ">="
				} else {
					lower <- ">"
				}
				upper <- "<="
			} else if (right & !include.lowest) {
				lower <- ">"
				upper <- "<="
			} else if (!right & include.lowest) {
				lower <- ">="
				if (j == which.max(fromTos[[2L]])) {
					upper <- "<="
				} else {
					upper <- "<"
				}
			} else if (!right & !include.lowest) {
				lower <- ">="
				upper <- "<"
			}

			ex <- paste0(ex, "if(", sources(x)[i], lower, fromTos[j, 1L], " & ", sources(x)[i], upper, fromTos[j, 2L], ", ", fromTos[j, 3L], ", ")

			# labels for levels table
			if (!is.na(right)) {

				leftBrack <- if (lower == ">") { "(" } else if (lower == ">=") { "["}
				rightBrack <- if (upper == "<") { ")" } else if (upper == "<=") { "]"}
				labels[j] <- paste0(leftBrack, fromTos[j, 1L], "-", fromTos[j, 2L], rightBrack)

			}

		}

		if (is.null(others) || is.na(others)) {
			ex <- paste0(ex, "null()")
		} else {
			ex <- paste0(ex, others)
		}
		ex <- paste0(ex, paste(rep(")", j), collapse = ""))
		
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
		
		# create levels table
		if (!is.na(right)) {

			levs[[i]] <- data.table::data.table(ID = as.integer(fromTos[[3L]]), DUMMY = labels)

			if (!is.null(others) && !is.na(others)) levs[[i]] <- rbind(levs[[i]], data.table::data.table(ID = as.integer(others), DUMMY = 'others'))

			names(levs[[i]])[2L] <- names(x)[i]

		} else {
			levs <- NULL
		}

	}

	out <- .makeGRaster(srcs, names = names(x), levels = levs, ac = 1L)
	out
	
	} # EOF
)

