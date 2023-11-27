#' Frequencies of cell values in a raster
#'
#' @description `freq()` tabulates the frequency of cell values in a raster. For rasters where [datatype()] is `integer` or `factor`, the frequency of each value or level is reported. For other rasters, the range of values is divided into bins, and the number of cells with values in each bin is reported.
#'
#' @param x A `GRaster`.
#' @param digits Numeric integer: Number of digits by which to round raster values. Ignored for integer and categorical rasters.
#' @param bins Positive numeric integer: Number of bins in which to divide values of `numeric` rasters. The default is 100. For `integer` and categorical rasters, each value is tallied (i.e., this is ignored).
#' @param value Numeric or `NULL` (default): If numeric, only cells with this value will be counted. If `NULL`, all values will be counted.
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
		bins = 100,
		value = NULL
	) {

	dtype <- datatype(x, type = "GRASS")
	.freq(x = x, dtype = dtype, digits = digits, bins = bins, value = value)
	
	} # EOF
)

#' @param x `GRaster` or [sources()] name(s)
#' @param dtype [datatype()] using `GRASS`-style output
#' @param digits Integer
#' @param bins Integer > 0
#' @param value NULL or numeric
#'
#' @noRd
.freq <- function(x, dtype, digits = 3, bins = 100, value = NULL) {

	if (inherits(x, "GRaster")) {
		.restore(x)
		region(x)
		src <- sources(x)
	} else {
		src <- x
	}

	nLayers <- length(src)

	if (dtype != "CELL" & bins <= 0) stop("Argument ", sQuote("bins"), " must be a positive integer.")
	
	# get values for each raster
	out <- list()
	for (i in seq_len(nLayers)) {

		thisSrc <- src[i]

		if (!is.null(value)) {

			thisSrc <- .makeSourceName("r_mapcalc", "raster")
			ex <- paste0(thisSrc, " = if(", src[i], " == ", value, ", ", value, ", null())")
			rgrass::execGRASS(
				cmd = "r.mapcalc",
				expression = ex,
				flags = c(.quiet(), "overwrite")
			)
		
		}

		args <- list(
			cmd = "r.stats",
			input = thisSrc,
   			separator = "pipe",
			# flags = c("c", "n")
			flags = c("c", "n", .quiet()),
			intern = TRUE
		)
		
		if (dtype[i] != "CELL") args$nsteps = bins
		
		data <- do.call(rgrass::execGRASS, args = args)
		
		bads <- which(grepl(data, pattern = "\b"))
		if (length(bads) > 0L) data <- data[-bads]
	
		data <- strsplit(data, split = "\\|")

		# categorical/integer data
		if (dtype[i] == "CELL") {
			
			freqs <- do.call(rbind, data)
			freqs <- data.table::as.data.table(freqs)
			names(freqs) <- c("value", "count")

			freqs[ , c("value", "count") := lapply(.SD, as.integer), .SDcols = c("value", "count")]

			data.table::setkeyv(freqs, "value")

			# add level labels
			if (is.factor(x)[i]) {

          		levs <- levels(x)[[i]]
				xValCol <- names(freqs)[1L]
				yValCol <- names(levs)[1L]
				freqs <- merge(freqs, levs, by.x = xValCol, by.y = yValCol, all.y = TRUE)
				
				freqs <- replaceNAs(freqs, cols = "count", replace = 0)

			}
		
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

}
