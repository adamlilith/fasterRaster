#' Rows of a GRaster or GVector's table that have no NAs or that have NAs
#'
#' @description When applied to a [categorical][tutorial_raster_data_types] `GRaster`, `compete.cases()` returns `TRUE` for each row of the "levels" table that has no `NA`s in it. In contrast, `missing.cases()` returns `TRUE` for each row that has at least one `NA` in it. If the raster is not categorical, then `NA` is returned.
#'
#' When applied to a `GVector` with a data table, `complete.cases()` returns `TRUE` for each row where there are no `NA`s. if the `GVector` has no data table, then a vector of integers from 1 to the total number of geometries will be returned. In contrast, `missing.cases()` returns `TRUE` for every row that has at least one `NA` in it. If the `GVector` has no data table, then `NA` is returned.
#'
#' @param ... A `GRaster` or `GVector`.
#'
#' @param levels Logical (`GRaster`s only): If `TRUE` (default), then assess only the "value" and [activeCat()] columns of the levels table (see [levels()]). If `FALSE`, then assess all columns (see [cats()]).
#'
#' @returns Both `complete.cases()` and `missing.cases()` return the same type of object. The output depends on the input:
#' * A categorical `GRaster` with just one layer: A logical vector.
#' * An integer, float, or double `GRaster` with just one layer: `NA`.
#' * A `GRaster` with multiple layers: A list with one element per layer with either logical vectors or `NA`s, as per above.
#' * A `GVector` with a data table: A logical vector.
#' * A `GVector` without a data table: `NA`.
#'
#' @example man/examples/ex_complete.cases.r
#'
#' @aliases complete.cases
#' @rdname complete.cases
#' @exportMethod complete.cases
methods::setMethod(
	f = "complete.cases",
	signature = "GRaster",
	function(..., levels = TRUE) {
	
	dots <- list(...)
	if (length(dots) != 1L) stop("Can only assess complete cases for a single\n  GRaster at a time (though it can be multi-layered).")
	x <- dots[[1L]]
	
	if (levels) {
		categs <- levels(x)
	} else {
		categs <- cats(x)
	}

	out <- list()
	for (i in seq_len(nlyr(x))) {
	
		if (is.factor(x)[i]) {
			out[[i]] <- stats::complete.cases(categs[[i]])
		} else {
			out[[i]] <- NA
		}
	
	}
	
	if (nlyr(x) == 1L) out <- out[[1L]]
	out
	
	} # EOF
)

#' @aliases complete.cases
#' @rdname complete.cases
#' @exportMethod complete.cases
methods::setMethod(
	f = "complete.cases",
	signature = "GVector",
	function(...) {
	
	dots <- list(...)
	if (length(dots) != 1L) stop("Can only assess complete cases for a single GVector at a time.")
	x <- dots[[1L]]
	
	table <- x@table
	if (nrow(table) == 0L) {
		out <- seq_len(ngeom(x))
	} else {
		out <- stats::complete.cases(table)
	}
	out
	
	} # EOF
)

#' @aliases missing.cases
#' @rdname complete.cases
#' @exportMethod missing.cases
methods::setMethod(
	f = "missing.cases",
	signature = "GRaster",
	function(..., levels = TRUE) {

	dots <- list(...)
	if (length(dots) != 1L) stop("Can only assess complete cases for a single\n  GRaster at a time (though it can be multi-layered).")
	x <- dots[[1L]]
	
	if (levels) {
		categs <- levels(x)
	} else {
		categs <- cats(x)
	}
	
	out <- list()
	for (i in seq_len(nlyr(x))) {

		if (is.factor(x)[i]) {
			out[[i]] <- !stats::complete.cases(categs[[i]])
		} else {
			out[[i]] <- NA
		}
	
	}
	
	if (nlyr(x) == 1L) out <- out[[1L]]
	out
	
	} # EOF
)

#' @aliases missing.cases
#' @rdname complete.cases
#' @exportMethod missing.cases
methods::setMethod(
	f = "missing.cases",
	signature = "GVector",
	function(...) {

	dots <- list(...)
	if (length(dots) != 1L) stop("Can only assess missing cases for a single GVector at a time.")
	x <- dots[[1L]]
	
	table <- x@table
	if (nrow(table) == 0L) {
		out <- NA
	} else {
		out <- !stats::complete.cases(table)
	}
	out
	
	
	} # EOF
)




