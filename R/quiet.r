#' Returns .quiet() or NULL for "flags" argument to GRASS modules
#'
#' @description A function for developers used for setting the "quiet' argument in `flags` arguments passed to [rgrass::execGRASS()]. If `faster("verbose")` is `TRUE`, the string "quiet" is returned. If `FALSE`, then `NULL` is returned.
#'
#' @returns A string (.quiet()) or `NULL`.
#'
#' @examples
#'
#' if (grassStarted()) {
#'
#' faster("verbose")
#' .quiet()
#'
#' }
#'
#' @aliases .quiet
#' @rdname quiet
#' @keywords internal
.quiet <- function() {

	if (faster("verbose")) {
		out <- NULL
	} else {
		out <- "quiet"
	}
	out

}
