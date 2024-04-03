#' Combine levels table from multiple categorical GRasters
#'
#' @description This function creates a single "levels" table from the levels tables of one or more [categorical][tutorial_raster_data_types] `GRaster`s.
#'
#' The difference between this function and [combineCats()] is that `combineCats()` creates a "combined" `GRaster` with a combined levels table, whereas this one just merges the levels tables.
#'
#' @param x A `GRaster` or a `list` of `GRaster`s.
#' @param ... Arguments to pass to [data.table::merge()].
#'
#' @returns A `list` with a "levels" table (a `data.frame` or `data.table`), and the active category number for the new table. Following [terra::activeCat()], the number is offset by 1, so a value of 1 indicates that the second column in the table should be used for the category labels, a value of 2 indicates the third column should be used, and so on.
#'
#' @example man/examples/ex_GRaster_categorical.r
#'
#' @seealso [combineCats()], `GRaster` [data types][tutorial_raster_data_types]
#'
#' @aliases combineLevels
#' @rdname combineLevels
#' @exportMethod combineLevels
methods::setMethod(
	f = "combineLevels",
	signature = c(x = "GRaster"),
	function(x) {

	if (!any(is.factor(x))) {
	
		out <- data.table::data.table(NULL)
		if (!faster("useDataTable")) out <- as.data.frame(out)
	
	} else {
		x <- cats(x)
		out <- .combineLevels(x = x)
	}
	out
	
	}
)

#' @aliases combineLevels
#' @rdname combineLevels
#' @exportMethod combineLevels
methods::setMethod(
	f = "combineLevels",
	signature = c(x = "list"),
	function(x) {
	
	if (!all(sapply(x, inherits, what = 'GRaster'))) stop("`x` must be a `GRaster` with one or more layers, or a `list` of `GRaster`s.")
	xx <- x
	x <- list()
	for (i in seq_along(xx)) x <- c(x, cats(xx[[i]]))
	.combineLevels(x = x)
	
	}
)

#' Combine "levels" tables
#'
#' @param x A list of `data.frame`s or `data.table`s.
#'
#' @noRd
.combineLevels <- function(x) {

	for (i in seq_along(x)) {
		if (!inherits(x[[i]], "data.table")) x[[i]] <- data.table::as.data.table(x[[i]])
	}

	i <- 1L
	out <- data.table::data.table(NULL)
	while (nrow(out) == 0L & i <= length(x)) {
		out <- x[[i]]
		i <- i + 1L
	}

	if (i <= length(x)) {
		
		by.x <- names(out)[1L]
		for (j in i:length(x)) {
			cats2 <- x[[j]]
			if (nrow(cats2) > 0L) {
				by.y <- names(cats2)[1L]
				out <- merge(out, cats2, by.x = by.x, by.y = by.y)
			}
		}
		
	}
	if (!faster("useDataTable")) out <- as.data.frame(out)
	out
	
}
