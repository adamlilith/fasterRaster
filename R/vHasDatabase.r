#' Tests if a GRASS vector is linked to an attribute table
#'
#' @description **GRASS** vectors can be lined to one or more attribute tables, or "databases.". This function tests to see if the vector does indeed have a database. This function is typically used by developers.
#'
#' @param x A `GVector` or the [sources()] name of a vector in **GRASS**.
#'
#' @returns Logical.
#'
#' @example man/examples/ex_vFunctions.r
#'
#' @aliases .vHasDatabase
#' @rdname vHasDatabase
#' @noRd
.vHasDatabase <- function(x) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	info <- rgrass::execGRASS(
		"v.db.connect",
		map = src,
		flags = c(.quiet(), "p"),
		intern = TRUE
	)

	length(info) > 0L

}
