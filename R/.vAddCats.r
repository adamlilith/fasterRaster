#' Add categories to vector features lacking them
#'
#' @description This function adds category identifiers to geometries of a **GRASS** vector that lack them.
#' 
#' @param x A `GRaster` or the [sources()] name of one.
#' 
#' @returns The [sources()] name of the `GVector`.
#' 
#' @aliases .vAddCats
#' @rdname .vAddCats
#' @noRd
.vAddCats <- function(x) {

	if (inherits(x, "GVector")) {
		.restore(x)
		srcIn <- sources(x)
	} else {
		srcIn <- x
	}

	src <- .makeSourceName("v_category", "vector")
	args <- list(
		cmd = "v.category",
		input = srcIn,
		output = src,
		option = "add"
	)
	do.call(rgrass::execGRASS, args = args)
	src

}