#' Remake the attribute table of a GRASS vector with only the "cat" column
#' 
#' @description Attaches an attribute table to a **GRASS** raster with only the `cat` column. Use [.dbRemove()] to remove the attribute table first.
#' 
#' @param x A `GVector`.
#' @returns A `GVector`.
#' 
#' @aliases .dbRecat
#' @rdname dbRecat
#' @noRd
.dbRecat <- function(x) {

	.restore(x)

	src <- .makeSourceName("v_category", "vector")
	args <- list(
		cmd = "v.category",
		input = sources(x),
		output = src,
		option = "add",
		flags = c("quiet", "overwrite"),
		intern = TRUE
	)

	do.call(rgrass::execGRASS, args = args)
	.makeGVector(src)

}
