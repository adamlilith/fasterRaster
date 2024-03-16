#' Add a database table to a GRASS attribute table
#'
#' @description `.vDetachDatabase()` detaches the database from a **GRASS** vector and deletes it. This table is meant to be "invisible" to most users--they should use interact with attribute tables using the `GVector` slot `@table``. Some functions do require tables (e.g., [extract()] and [spatSample()]). **This function is mostly of use to developers.**
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#' 
#' @returns Invisibly returns the [sources()] name of a vector in **GRASS**.
#' 
#' @aliases .vDetachDatabase
#' @rdname vDetachDatabase
#' @export
.vDetachDatabase <- function(x) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	if (.vHasDatabase(src)) {

		rgrass::execGRASS(
			cmd = "v.db.droptable",
			map = src,
			flags = c(.quiet(), "f")
		)

	}
	invisible(src)

}
