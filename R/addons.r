#' Test if addons directory exists and if an addon is installed
#'
#' @description This function tests to see if the "addons" directory specified using [faster()] actually exists, and if a particular **GRASS** `addons tool is available. The `addons` folder and tool must exists for methods that rely on particular **GRASS** `addons` to work. See `vignette("addons", package = "fasterRaster")`.
#'
#' @param x Either `NULL` or a character specifying the name of a **GRASS** addons tool. If `NULL`, the existence of the `addonsDir` (see [faster()]) will be tested. If the tool name is provided, the existence of the folder and tool will be tested. The "`/bin`" subfolder should not be included.
#'
#' @param fail Logical: If `TRUE` (default), and the addons folder is not correctly specified, the exit the function with an error. If `FALSE`, then `NULL` will be returned with a warning.
#'
#' @param verbose Logical: If `TRUE` (default), display a message on success or warning (the `fail` option always displays a message).
#'
#' @returns Logical.
#'
#' @seealso `vignette("addons", package = "fasterRaster")`
#'
#' @example man/examples/ex_addons.r
#'
#' @aliases addons
#' @rdname addons
#' @export
addons <- function(x = NULL, fail = TRUE, verbose = TRUE) {

	out <- TRUE

	ao <- faster("addonsDir")
	if (is.null(ao) || !file.exists(ao)) {
		
		msg <- paste0("The `addons` folder is incorrect. See `faster()` and `vignette(", dQuote("addons", q = FALSE), ", package = ", dQuote("fasterRaster", q = FALSE), ").")

		if (fail) {
			stop(msg)
		} else if (!fail & verbose) {
			warning(msg)
		}

		out <- FALSE

	}
	
	if (!is.null(x)) {

		extensions <- list.files(paste0(faster("addonsDir"), "/bin"))
		exts <- .fileExt(extensions)
		extensions <- substr(extensions, 1L, nchar(extensions) - nchar(exts) - 1L)

		if (!(x %in% extensions)) {

			msg <- paste0("The addon extension `", x, "` cannot be found. See `vignette(", dQuote("addons", q = FALSE), ", package = ", dQuote("fasterRaster", q = FALSE), ").")
			if (fail) {
				stop(msg)
			} else if (!fail & verbose) {
				warning(msg)
			}
		
			out <- FALSE

		}
	}
	
	if (verbose) {
		if (is.null(x)) {
			omnibus::say("Addons directory exists.")
		} else {
			if (out) {
				omnibus::say("Addon `", x, "` is installed.")
			} else {
				omnibus::say("Addon `", x, "` cannot be found.")
			}
		}
	}
	invisible(out)

}
