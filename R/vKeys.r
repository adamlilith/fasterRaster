#' Key column values of a vector in GRASS
#'
#' @description Returns values in the `key` column of a vector in **GRASS**.
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#'
#' @returns An character vector with the "key" of each geometry in the vector
#'
#' @aliases .vKeys
#' @rdname vKeys
#' @noRd
.vKeys <- function(x) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	out <- .vAsDataTable(src)
	out <- out[["key"]]
	out

}
