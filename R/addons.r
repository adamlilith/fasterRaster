#' Test if addons directory exists and if an addon is installed
#'
#' @description These functions handle **GRASS** addons, which are optional tools that can be installed. Most functions in **fasterRaster** rely on "base" **GRASS** tools (not addons), but a few do.
#'
#' * `addons()`: Either lists all installed addons or verifies if one or more specific addons are installed.
#' * `installAddon()`: Installs a **GRASS** addon. An addon typically only needs installed once. You can install an addon, quit and restart **R**, attach **fasterRaster**, and any installed addons can be used without using this function again. 
#' * `removeAddon()`: Delete an installed addon from your system.
#'
#' @param x Either `NULL` or a character specifying the name of a **GRASS** addons tool. If `NULL`, a vector of installed addons is returned. If a character vector is provided, a logical vector is returned, one per value in `x` that indicates if the respective addon is installed.
#'
#' @param check Logical: If `TRUE`, check to see if the addon is available (`installAddon()`)or if it is installed (`removeAddon()`).
#'
#' @returns `addons()`: Logical. The other functions invisibly return a logical value indicating if the operation succeeded or not.
#'
#' @seealso `vignette("addons", package = "fasterRaster")`
#'
#' @example man/examples/ex_addons.r
#'
#' @aliases addons
#' @rdname addons
#' @export
addons <- function(x = NULL) {

	if (!grassStarted()) {
		warning("GRASS has not been started. Use fast() to create a GRaster or GVector to start GRASS.")
		out <- NULL
	} else {

		out <- rgrass::execGRASS("g.extension", flags = c("a", .quiet()), intern = TRUE)
		if (is.null(x)) {
			out <- sort(out)
		} else if (!is.null(x)) {
			out <- x %in% out
			names(out) <- x
		}
	}
	out
	
}

#' @aliases installAddon
#' @rdname addons
#' @export
installAddon <- function(x, check = TRUE) {

	if (check) {
		avails <- rgrass::execGRASS("g.extension", flags = c("l", .quiet()))
		if (!(x %in% avails)) {
			warning("The addon is not available on the official GRASS addon repository.")
			return(invisible(FALSE))
		}
	}

	rgrass::execGRASS("g.extension", operation = "add", extension = x, flags = .quiet())
	invisible(TRUE)

}

#' @aliases removeAddon
#' @rdname addons
#' @export
removeAddon <- function(x, check = TRUE) {

	if (check) {
		avails <- rgrass::execGRASS("g.extension", flags = c("a", .quiet()))
		if (!(x %in% avails)) warning("The addon is not installed.")
		return(invisible(FALSE))
	}

	rgrass::execGRASS("g.extension", operation = "removeadd", extension = x, flags = c(.quiet(), "f"))
	invisible(TRUE)

}

# # Tests if addons folder is installed
# addons <- function(xx = NULL, fail = TRUE, verbose = TRUE) {

# 	out <- TRUE

# 	ao <- faster("addonsDir")
# 	if (is.null(ao) || !file.exists(ao)) {
		
# 		msg <- paste0("The `addons` folder is incorrect. See `faster()` and `vignette(", dQuote("addons", q = FALSE), ", package = ", dQuote("fasterRaster", q = FALSE), ").")

# 		if (fail) {
# 			stop(msg)
# 		} else if (!fail & verbose) {
# 			warning(msg)
# 		}

# 		out <- FALSE

# 	}
	
# 	if (!is.null(x)) {

# 		extensions <- list.files(paste0(faster("addonsDir"), "/bin"))
# 		exts <- .fileExt(extensions)
# 		extensions <- substr(extensions, 1L, nchar(extensions) - nchar(exts) - 1L)

# 		if (!(x %in% extensions)) {

# 			msg <- paste0("The addon extension `", x, "` cannot be found. See `vignette(", dQuote("addons", q = FALSE), ", package = ", dQuote("fasterRaster", q = FALSE), ").")
# 			if (fail) {
# 				stop(msg)
# 			} else if (!fail & verbose) {
# 				warning(msg)
# 			}
		
# 			out <- FALSE

# 		}
# 	}
	
# 	if (verbose) {
# 		if (is.null(x)) {
# 			omnibus::say("Addons directory exists.")
# 		} else {
# 			if (out) {
# 				omnibus::say("Addon `", x, "` is installed.")
# 			} else {
# 				omnibus::say("Addon `", x, "` cannot be found.")
# 			}
# 		}
# 	}
# 	invisible(out)

# }
