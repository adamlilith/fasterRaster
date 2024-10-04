#' Create one GRaster layer per unique value in a GRaster
#'
#' @description This function creates a multi-layered `GRaster` for every unique values in an input `GRaster`. By default, the output will have a value of 1 wherever the input has the given value, and 0 elsewhere. This is useful for creating dummy variable `GRaster` layers for use with models that have factors, especially if the input `GRaster` is categorical.
#'
#' @param x A `GRaster`.
#'
#' @param classes Either `NULL` (default) or a character vector with category labels for which to create outputs. If the input is not a categorical/factor or `integer` `GRaster`, this is ignored.
#'
#' @param keep Logical: If `FALSE` (default), then the original value in the input `GRaster` will be retained in the each of the output `GRaster` layers wherever the input had the respective value. Other cells will be assigned a value of `other`.
#'
#' @param other Numeric or `NA`: Value to assign to cells that do not have the target value.
#'
#' @param bins Numeric: Number of bins in which to put values. This is only used for `GRaster`s that are not categorical/factor rasters or `integer` rasters.
#'
#' @param digits Numeric: Number of digits to which to round input if it is a `numeric` or `double` `GRaster` (see `vignettes("GRasters", package = "fasterRaster")`).
#'
#' @returns If the input `x` is a single-layered `GRaster`, the output will be a multi-layered `GRaster` with one layer per value in the input, or one layer per values in `classes`. If the input is a multi-layered `GRaster`, the output will be a `list` of multi-layered `GRaster`s.
#'
#' @example man/examples/ex_segregate.r
#'
#' @seealso [terra::segregate()]
#'
#' @aliases segregate
#' @rdname segregate
#' @exportMethod segregate
methods::setMethod(
	f = "segregate",
	signature = c(x = "GRaster"),
	function(x, classes = NULL, keep = FALSE, other = 0, bins = 100, digits = 3) {

	.locationRestore(x)
	.region(x)

	if (!is.null(classes) & (all(is.factor(x)) | all(is.integer(x)))) x <- x[x %in% classes]
	if (is.na(other)) other <- "null()"

	out <- list()
	nLayers <- nlyr(x)
	for (i in seq_len(nLayers)) {
	
		# unique values
		dtype <- datatype(x, type = "GRASS")[i]
		freqs <- freq(x[[i]], digits = digits, bins = bins)
		freqs <- freqs[count > 0]
		vals <- freqs$value

		nVals <- length(vals)
		if (nVals == 0) {
			thisOut <- NA
		} else {
			
			thisSrcs <- .makeSourceName("segregate", "raster", nVals)
			for (j in seq_len(nVals)) {

				if (keep) {
					thisKeep <- vals[j]
				} else {
					thisKeep <- 1
				}
				ex <- paste0(thisSrcs[j], " = if(", sources(x)[i], " == ", vals[j], ", ", thisKeep, ", ", other, ")")
				rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

			}

			names <- if (is.factor(x)[i]) {
				freqs[[3]]
			} else {
				as.character(vals)
			}
			thisOut <- .makeGRaster(thisSrcs, names = names)

		}
	
		out[[i]] <- thisOut
	
	}

	if (nLayers == 1L) {
		out <- out[[1L]]
	} else {
		names(out) <- names(x)
	}
	out
	
	} # EOF
)
