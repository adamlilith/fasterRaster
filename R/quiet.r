#' Returns .quiet() or NULL for "flags" argument to GRASS modules
#'
#' @description A function for developers used for setting the "quiet' argument in `flags` arguments passed to [rgrass::execGRASS()]. If `faster("debug")` is `TRUE`, the string "quiet" is returned. If `FALSE`, then `NULL` is returned.
#'
#' @returns A string (.quiet()) or `NULL`.
#'
#' @aliases .quiet
#' @rdname quiet
#' @keywords internal
.quiet <- function() {

	if (faster("debug")) {
		out <- 'verbose'
	} else {
		out <- "quiet"
	}
	out

}
