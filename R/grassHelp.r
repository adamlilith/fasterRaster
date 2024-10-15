#' Open the help page for a GRASS module
#'
#' @description This function opens the manual page for a **GRASS** module (function) in your browser.
#'
#' @param x Character: Any of:
#' * The name of a **GRASS** module (e.g., `"r.mapcalc"`).
#' * `"type"`: Display a page wherein modules are classified by types.
#' * `"topics"`: Display an index of topics.
#'
#' @param online Logical: If `FALSE` (default), show the manual page that was included with your installation of **GRASS** on your computer.  If `FALSE`, show the manual page online (requires an Internet connection). In either case, the manual page will display for the version of **GRASS** you have installed.
#'
#' @returns Nothing (opens a web page).
#'
#' @example man/examples/ex_grassHelp.r
#'
#' @aliases grassHelp
#' @rdname grassHelp
#' @export
grassHelp <- function(x, online = FALSE) {

	if (interactive()) {

		if (!grassStarted()) {
			omnibus::say("You must start GRASS first by using fast() at least once before opening a manual page.")
			return()
		}

		x <- tolower(x)
		if (length(x) != 1L) stop("Only one help page can be opened at a time.")
		
		args <- list(
			cmd = "g.manual",
			entry = x,
			flags = .quiet()
		)
		
		if (x == "type") {
			args$flags <- c(args$flags, "i")
		} else if (x == "index") {
			args$flags <- c(args$flags, "t")
		}

		do.call(rgrass::execGRASS, args = args)

	} else {
		warning("You can only open GRASS manual pages in an interactive R session.")
	}

}
