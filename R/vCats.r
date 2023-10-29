#' Category column values of a vector in GRASS
#'
#' @description Returns values in the `cat` column of a vector in **GRASS**.
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#'
#' @param table Logical: If `TRUE`, return category numbers from the data table associated with the vector. If `FALSE` (default), return category numbers from the actual vector.
#'
#' @param integer Logical: If `TRUE` (default), return category values as integers. In some cases, a geometry can have multiple categories, in which case `NA` is returned. If `FALSE`, return category values as strings (and thus, if a geometry has more than one category, does not convert to `NA`).
#'
#' @returns An integer vector.
#'
#' @aliases .vCats
#' @rdname vCats
#' @noRd
.vCats <- function(x, table = FALSE, integer = TRUE) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	if (table) {
		out <- .vAsDataTable(src)$cat
	} else {
		out <- rgrass::execGRASS("v.category", input = src, option = "print", flags = "quiet", intern = TRUE)
	}
	if (integer) out <- as.integer(out)
	out

}
