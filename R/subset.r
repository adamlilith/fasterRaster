#' Subset layers from a GRaster, or specific rows from a GVector
#'
#' @description `subset()` can be used to subset or remove one or more layers from a `GRaster`. It can also be used to subset or remove rows from a `GVector` with a data table.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param subset Numeric integer, integer, logical, or character: Indicates the layer(s) of a `GRaster` to subset, or the rows(s) of a `GVector` to return.
#'
#' @param negate Logical: If `TRUE`, all layers or rows in `subset` will be *removed* from the output. Default is `FALSE`.
#'
#' @returns A `GRaster` or `GVector`.
#'
#' @seealso \code{\link[fasterRaster]{[[}}, \code{\link[fasterRaster]{[}}
#'
#' @example man/examples/ex_GRaster_GVector_subset_assign.r
#'
#' @aliases subset
#' @docType methods
#' @rdname subset
#' @exportMethod subset
methods::setMethod(
	"subset",
	signature = c(x = "GRaster"),
	function(x, subset, negate = FALSE) {

	# test indices
	subset <- .layerIndex(subset, x, recycle = TRUE, negate = negate)
	x[[subset]]

	} # EOF
)

#' @aliases subset
#' @rdname subset
#' @exportMethod subset
methods::setMethod(
	"subset",
	signature = c(x = "GVector"),
	function(x, subset, negate = FALSE) {

	# turn subset into integer
	if (is.logical(subset)) {
		if (length(subset) < nr) subset <- rep(subset, length.out = nr)
		subset <- which(subset)
	}
	if (negate) subset <- seq_len(nr)[-subset]
	x[subset]	
	
	} # EOF
)
