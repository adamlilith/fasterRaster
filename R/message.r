#' Display warning or message
#'
#' @description Display a warning or message if the given warning has not been displayed since **fasterRaster** was attached or if a given number or hours has passed since then.
#'
#' @param msg Character: Name for the message (used internally). Should be able to be assigned to a list (i.e., no spaces, punctuation, etc.).
#' 
#' @param message Text for the message.
#' 
#' @returns `TRUE` (invisibly).
#' 
#' @aliases .message
#' @rdname .message
#' @noRd
.message <- function(msg, message) {

	# How long do we wait in hours to re-display same message?
	lag_hours <- 8
	lag_sec <- lag_hours * 60 * 60

	message <- paste0(message, "\n  This warning will not be displayed again until ", lag_hours, " hours have passed.")

	now <- Sys.time()
	if (any(names(.fasterRaster$messages) == msg)) {

		if (now - .fasterRaster$messages[[msg]] > lag_sec) {
			warning(message, immediate. = TRUE)
			utils::flush.console()
		    .fasterRaster$messages[[msg]] <- now
		}

	} else {

		.fasterRaster$messages[[msg]] <- now
		warning(message, immediate. = TRUE)
		utils::flush.console()

	}

	invisible(TRUE)
}
