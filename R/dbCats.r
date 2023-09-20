#' Category column of a vector in GRASS
#'
#' @description Imports the `cat` column of a vector in **GRASS**. This is *not* the same as the `cat` index attached to a `GVector`, which is an **R** object that points to a vector in **GRASS**.
#'
#' @param x A `GVector`.
#'
#' @returns A `data.table` with a single column named `cat`.
#'
#' @aliases .dbCats
#' @rdname dbCats
#' @noRd
methods::setMethod(
	f = ".dbCats",
	signature = c(x = "GVector"),
	definition = function(x) {

	out <- .dbToDataTable(x)
	out[ , "cat"]

	} # EOF
)
