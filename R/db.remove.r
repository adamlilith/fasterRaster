#' Remove an attribute table from a vector in GRASS
#'
#' @desciption This function removes an attribute table from a vector in **GRASS*.  It does *not* remove the attribute table associated with a `GVector`, which is a an **R** object that points to a vector in **GRASS**.
#'
#' This function is typically used by developers. Note that it removes the entire table, even the `cat` column, which is necessary for identifying individual features.
#'
#' @param x A `GVector`.
#'
#' @returns A `GVector`.
#'
#' @aliases db.remove
#' @rdname db.remove
#' @noRd
methods::setMethod(
	f = "db.remove",
	signature = c(x = "GVector"),
	function(x) {
	
	.restore(x)
	args <- list(
		cmd = "v.db.droptable",
		map = sources(x),
		flags = c("quiet", "f"),
		intern = TRUE
	)
	do.call(rgrass::execGRASS, args = args)

	x
	
	} # EOF
)
