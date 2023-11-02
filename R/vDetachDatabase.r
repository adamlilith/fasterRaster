#' Detach database from GVector
#'
#' @param x A `GVector` or the [sources()] name of one.
#'
#' @returns The [sources()] name of a `GVector` (invisibly). Also disconnects its database.
#'
#' @noRd
.vDetachDatabase <- function(x) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	rgrass::execGRASS(
		cmd = "v.db.connect",
		map = src,
		flags = c("quiet", "d")
	)

	invisible(src)

}