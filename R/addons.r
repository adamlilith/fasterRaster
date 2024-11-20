#' Test if addons directory exists and if an addon is installed
#'
#' @description This function tests to see if the "addons" directory specified using [faster()] actually exists, and if a particular **GRASS** `addons module is available. The `addons` folder and module must exists for methods that rely on particular **GRASS** `addons` to work.
#'
#' @param x Either `NULL` or a character specifying the name of a **GRASS** addons module. If `NULL`, the existence of the `addonsDir` (see [faster()]) will be tested. If the module name is provided, the existence of the folder will be tested, and if this test passes, the existence of the module will be tested.
#'
#' @param onFail Character: What to do if the addons folder is not found:
#' * `"fail"` (default): Fail with a message.
#' * `"warning"`: Warn with a message.
#'
#' @param verbose Logical: If `TRUE` (default), display a message on success or warning (the `fail` option always displays a message).
#'
#' @returns Logical.
#'
#' @examples man/examples/ex_addons.r
#'
#' @aliases addons
#' @rdname addons
#' @export
addons <- function(x = NULL, onFail = "fail", verbose = TRUE) {

	onFail <- omnibus::pmatchSafe(onFail, c("fail", "warning"), nmax = 1L)

	out <- TRUE

	ao <- faster("addonsDir")
	if (is.null(ao) || !file.exists(ao)) {
		
		msg <- paste0("The `addons` folder is incorrect. See `faster()` and `vignette(", dQuote("addons", q = FALSE), ", package = ", dQuote("fasterRaster", q = FALSE), ").")

		if (onFail == "fail") {
			stop(msg)
		} else if (onFail == "warning" & verbose) {
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
			if (onFail == "fail") {
				stop(msg)
			} else if (onFail == "warning" & verbose) {
				warning(msg)
			}
		
			out <- FALSE

		}
	}
	
	if (verbose) {
		if (is.null(x)) {
			omnibus::say("Addons directory exists.")
		} else {
			omnibus::say("Addon `", x, "` is installed.")
		}
		invisible(out)
	} else {
		invisible(out)
	}


}
