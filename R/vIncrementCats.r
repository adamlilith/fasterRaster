#' Increment category values of a "GRASS" vector
#'
#' @description Adds a constant to all category values of a **GRASS** vector. **This function is mostly of use to developers.**
#'
#' @param x A `GVector` or the [sources()] name of one.
#'
#' @param add Integer: Value to add to each category value.
#'
#' @returns The [sources()] name of a **GRASS** vector with category values incremented.
#'
#' @aliases .vIncrementCats
#' @rdname vIncrementCats
#' @keywords internal
.vIncrementCats <- function(x, add) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
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
		flags = c(.quiet(), "overwrite", "t")
	)
	src

}
