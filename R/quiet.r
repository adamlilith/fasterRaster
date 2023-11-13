#' Returns .quiet() or NULL for "flags" argument to GRASS modules
#'
#' @description An internal function used to return .quiet() or `NULL` for use in the `flags` argument to [rgrass::execGRASS()]. If `getFastOptions("verbose")` is `TRUE`, the string "quiet" is returned. If `FALSE`, then `NULL` is returned.
#'
#' @returns A string (.quiet()) or `NULL`.
#'
#' @aliases .quiet
#' @rdname quiet
#' @noRd
.quiet <- function() {

	if (getFastOptions("verbose")) {
		"quiet"
	} else {
		NULL
	}

}
