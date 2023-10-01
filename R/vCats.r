#' Category column values of a vector in GRASS
#'
#' @description Returns values in the `cat` column of a vector in **GRASS**.
#'
#' @param x A `GVector` or the name of a vector in **GRASS**.
#'
#' @returns An integer vector with the category of each geometry in the vector
#'
#' @aliases .vCats
#' @rdname vCats
#' @noRd
.vCats <- function(x) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	out <- .vAsDataTable(y)
	out <- out[["cat"]]

	# args <- list(
	# 	cmd = "v.category",
	# 	input = src,
	# 	option = "print",
	# 	flags = c("quiet", "overwrite"),
	# 	intern = TRUE
	# )

	# out <- do.call(rgrass::execGRASS, args = args)
	out <- as.integer(out)
	out

}
