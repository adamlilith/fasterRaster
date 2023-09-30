#' Report GRASS vector geometries
#'
#' @description Report **GRASS** vector geometries.
#' 
#' @param x A `GVector` or the name of a vector in **GRASS**.
#' 
#' @returns
#' 
#' @aliases .vGeometries
#' @rdname vGeometries
#' @noRd
.vGeometries <- function(x) {

	if (inherits(x, "GVector")) {
		.restore(x)
		src <- sources(x)
	} else {
		src <- x
	}

	args <- list(
		cmd = "v.category",
		input = src,
		option = "report",
		flags = c("quiet", "overwrite"),
		intern = TRUE
	)

	info <- do.call(rgrass::execGRASS, args = args)
	info <- info[-1L:-2L]
	out <- rep(NA_integer_, length(info))
	for (i in seq_along(info)) {

		this <- info[i]
		endType <- regexpr(this, pattern = " ")[1L]
		type <- substr(this, 1L,  endType - 1L)
		n <- substr(this, endType, endType + 10L)
		n <- as.integer(n)
		if (is.na(n)) n <- 0L

		out[i] <- n
		names(out)[i] <- type

	}
	out

}