#' Category column values of a vector in GRASS
#'
#' @description Returns values in the `cat` column of a vector in **GRASS**.
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#'
#' @param long Logical: If `TRUE`, return category numbers for all sub-geometries. If `FALSE` (default), return category numbers for all geometries.
#'
#' @returns An integer vector.
#'
#' @aliases .vCats
#' @rdname vCats
#' @noRd
.vCats <- function(x, long = FALSE) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	if (long) {

		out <- rgrass::execGRASS("v.category", input = src, option = "print", flags = "quiet", intern = TRUE)

		as.integer(out)

	} else {

		as.integer(.vAsDataTable(src)$cat)
	
	}

}
