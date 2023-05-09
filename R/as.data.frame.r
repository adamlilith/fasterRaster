#' Convert GVector to a data frame
#'
#' @description Convert a `GVector`'s data table to a data frame.
#'
#' @param x A `GVector`.
#' @param row.names `NULL` (default) or a character vector giving names for rows.
#' @param optional Ignored.
#'
#' @returns A `data.frame`.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases as.data.frame
#' @rdname as.data.frame
#' @exportMethod as.data.frame
methods::setMethod(
	f = 'as.data.frame',
	signature = c(x = 'GVector'),
	definition = function(x, row.names = NULL, optional = FALSE) {
	
		data <- rgrass::execGRASS('v.db.select', map=gnames(x), intern=TRUE)
		
		# column names
		cols <- data[1L]
		cols <- strsplit(cols, '\\|')[[1L]]
		data <- data[-1L]
	
		# data values
		data <- strsplit(data, '\\|')
		out <- list()
		for (i in seq_along(cols)) out[[i]] <- rep(NA, length(data))
		names(out) <- cols
		out <- as.data.frame(out)

		# collate data
		for (i in 1L:nrow(out)) out[i , ] <- data[[i]]
		
		# everything is exported as a character
		ints <- which(datatype(x)$datatype == 'integer')
		nums <- which(datatype(x)$datatype == 'numeric')
		
		if (length(ints) > 0L) {
			for (int in ints) {
				if (any(out[ , int] == '')) {
					out[out[ , int] == '', int] <- NA_character_
				}
				out[ , int] <- as.integer(out[ , int])
			}
		}
		
		if (length(nums) > 0L) {
			for (num in nums) {
				if (any(out[ , num] == '')) {
					out[out[ , num] == '', num] <- NA_character_
				}
				out[ , num] <- as.numeric(out[ , num])
			}
		}
		
		if (!is.null(row.names)) rownames(out) <- row.names
		out
	
	} # EOF
)
