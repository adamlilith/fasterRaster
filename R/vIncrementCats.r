#' Increment category values of a GVector
#'
#' @description Adds a constant to all category values of a `GVector`.
#'
#' @param x A `GVector` or the [sources()] name of one.
#'
#' @param add Integer: Value to add to each category value.
#'
#' @param return Character: What to return. Either `"GVector"` or "`sources`" ([sources()] name of the new vector).
#'
#' @returns The same `GVector` as in `x`, but with incremented category values.
#'
#' @aliases .vIncrementCats
#' @noRd
.vIncrementCats <- function(x, add, return = "GVector") {

	if (inherits(x, "GVector")) {
		.restore(x)
		srcIn <- sources(x)
	} else {
		srcIn <- x
	}

	src <- .makeSourceName("v_category", "vector")
	rgrass::execGRASS(
		cmd = "v.category",
		input = srcIn,
		output = src,
		option = "sum",
		cat = add,
		flags = c(.quiet(), "overwrite")
	)

	return <- omnibus::pmatchSafe(return, c("GVector", "sources"))
	if (return == "GVector") {
		.makeGVector(src, table = as.data.table(x))
	} else {
		src
	}

}
