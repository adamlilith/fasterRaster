#' Display warning or message
#'
#' Display a warning or message if not yet displayed in this **GRASS** session or if a certain time period has passed.
#'
#' @param msg Character: Name for the message. See code for accepted names.
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

	message <- paste0(message, "\n  This warning will not be displayed again until ", lag_hours, " hours has passed.")

	now <- Sys.time()
	if (any(names(.fasterRaster$messages) == msg)) {

		if (now - .fasterRaster$messages[[msg]] > lag_sec) {
			warning(message, immediate. = TRUE)
		    .fasterRaster$messages[[msg]] <- now
		}

	} else {

		.fasterRaster$messages[[msg]] <- now
		warning(message, immediate. = TRUE)

	}

	invisible(TRUE)
}
