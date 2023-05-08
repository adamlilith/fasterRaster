#' Convert GVector to a data frame
#'
#' @description Convert a `GVector`'s data table to a data frame.
#'
#' @param x A `GVector`.
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
	definition = function(x) {
	
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
		
		if (length(ints) > 0L) out[ , ints] <- as.integer(out[ , ints])
		if (length(nums) > 0L) out[ , nums] <- as.numeric(out[ , nums])
		
		out
	
	} # EOF
)