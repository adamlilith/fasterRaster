#' Frequencies of cell values in a raster
#'
#' @description `freq()` tabulates the frequency of cell values in a raster. For rasters where [datatype()] is `integer` or `factor`, the frequency of each value or level is reported. For other rasters, the range of values is divided into bins, and the number of cells with values in each bin is reported.
#'
#' @param x A `GRaster`.
#' @param digits Numeric integer: Number of digits by which to round raster values.
#' @param value Numeric or `NULL` (default): If numeric, only cells with this value will be counted. If `NULL`, all values will be counted.
#' @param bins Positive numeric integer: Number of bins in which to divide values of `numeric` rasters. The default is 100. For `integer` and categorical rasters, each value is tallied.
#'
#' @returns A `data.frame` or a named `list` of `data.frame`s, one per layer in `x`.
#'
#' @seealso [terra::freq()], module `r.stats` in **GRASS**
#'
#' @example man/examples/ex_GRaster.r
#'
#' @aliases freq
#' @rdname freq
#' @exportMethod freq
methods::setMethod(
	f = "freq",
	signature = c(x = "GRaster"),
	definition = function(
		x,
		digits = 3,
		value = NULL,
		bins = 100
	) {

	.restore(x)
	region(x)

	if (bins <= 0) stop("Argument ", sQuote("bins"), " must be a positive integer.")
	
	# get values for each raster
	out <- list()
	for (i in 1L:nlyr(x)) {

		thisGn <- sources(x)[i]
	
		if (!is.null(value)) {

			thisGn <- .makeSourceName("value", "rast")
			ex <- paste0(thisGn, " = if(", sources(x)[i], " == ", value, ", ", value, ", null())")
			info <- rgrass::execGRASS(
				"r.mapcalc",
				expression=ex,
				flags=c("quiet", "overwrite"),
				intern=TRUE
			)
		
		}

		args <- list(
			cmd = "r.stats",
			input = thisGn,
   			separator = "pipe",
			flags = c("c", "n", "quiet"),
			intern = TRUE
		)
		
		if (datatype(x, "GRASS")[i] != "CELL") args$nsteps = bins
		
		data <- do.call(rgrass::execGRASS, args=args)
		
		# bads <- which(grepl(data, pattern="no data"))
		# if (length(bads) > 0L) data <- data[-bads]
		
		bads <- which(grepl(data, pattern="\b"))
		if (length(bads) > 0L) data <- data[-bads]
	
		data <- strsplit(data, split = "\\|")

		# categorical/integer data
		if (datatype(x, "GRASS")[i] == "CELL") {
			
			n <- length(data)

			freqs <- data.table::data.table(value = rep(NA_character_, n), count = rep(NA_integer_, n))

			for (j in seq_along(data)) {
				freqs$value[j] <- data[[j]][1L]
				freqs$count[j] <- data[[j]][2L]
			}
			
			freqs$value <- as.numeric(freqs$value)
			freqs$count <- as.numeric(freqs$count)
			data.table::setkeyv(freqs, "value")

			# add level labels
			if (is.factor(x)[i]) {

          		levs <- levels(x)[[i]]
				xValCol <- names(freqs)[1L]
				yValCol <- names(levs)[1L]
				freqs <- merge(freqs, levs, by.x = xValCol, by.y = yValCol, all.y = TRUE)
				
				freqs <- replaceNAs(freqs, cols = "count", replace = 0)

			}
		
	  		data.table::setkeyv(freqs, "value")

		# continuous data
		} else {
	
			n <- length(data)
			freqs <- data.table::data.table(from = rep(NA_real_, n), to = rep(NA_real_, n), count = rep(NA_integer_, n))

			for (j in seq_along(data)) {
			
				count <- data[[j]][2L]
				count <- as.numeric(count)

				fromTo <- strsplit(data[[j]][1L], split="-")[[1L]]
				fromTo <- as.numeric(fromTo)

				if (length(fromTo) == 2L) {
					from <- fromTo[1L]
					to <- fromTo[2L]
				} else if (length(fromTo) == 3L) {
					if (is.na(fromTo[1L])) {
						from <- -1 * fromTo[2L]
						to <- fromTo[3L]
					} else {
						from <- fromTo[1L]
						to <- -1 * fromTo[3L]
					}
				} else {
					from <- -1 * fromTo[2L]
					to <- -1 * fromTo[4L]
				}
					
				freqs$from[j] <- from
				freqs$to[j] <- to
				freqs$count[j] <- count

			} # next datum
			
			freqs$from <- round(freqs$from, digits)
			freqs$to <- round(freqs$to, digits)
		
		}

		if (!getFastOptions("useDataTable")) freqs <- as.data.frame(freqs)
		out[[i]] <- freqs
		names(out)[i] <- names(x)[i]

	} # next layer
	if (length(out) == 1L) out <- out[[1L]]
	out
	
	} # EOF
)
