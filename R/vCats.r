#' Category column of a vector in GRASS
#'
#' @description Imports the `cat` column of a vector in **GRASS**. This is *not* the same as the `cat` index attached to a `GVector`, which is an **R** object that points to a vector in **GRASS**.
#'
#' @param x A `GVector`.
#'
#' @returns An integer vector with the category of each geometry in the vector
#'
#' @aliases .vCats
#' @rdname vCats
#' @noRd
.vCats <- function(x) {

	args <- list(
		cmd = "v.category",
		input = sources(x),
		option = "print",
		flags = c("quiet", "overwrite"),
		intern = TRUE
	)

	out <- do.call(rgrass::execGRASS, args = args)
	out <- as.integer(out)
	out

}
