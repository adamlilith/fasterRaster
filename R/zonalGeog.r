#' Geographic statistics for sets of cells with the same values
#'
#' @description This function calculates geographic statistics for each set of cells in an [integer or factor raster][tutorial_raster_data_types]. Statistics include:
#' * Area
#' * Perimeter length
#' * "Compact square" statistic: \eqn{4 \sqrt(area) / perimeter)}
#' * "Compact circle" statistic: \eqn{4 * P / ( 2 \sqrt(\pi * A))} where *P* is the perimeter length and *A* the area.
#' * fractal dimension: \eqn{2 ( log(P) / log(A + 0.001))} where *P* is perimeter length and *A* is area.
#' * The average x- and y-coordinates of each zone.
#'
#' @param x A `GRaster`.
#' 
#' @param unit Character: Units of the output. Any of:
#' * `"meters"` (default)
#' * `"kilometers"` or `"km"`
#' * `"miles"` or `"mi"`
#' * `"yards"` or `"yd"`
#' * `"feet"` or `"ft"`: International foot; 1 foot exactly equal to 0.3048 meters
#' * `"cells"`: Number or cells
#'
#' Partial matching is used and case is ignored.
#'
#' @returns A list of `data.frame`s or a `data.table`s, one per layer in `x`. Only layers that are integers or factors have their geographies calculated. Other layers have `NULL` tables returned.
#'
#' @example man/examples/ex_zonalGeog.r
#'
#' @aliases zonalGeog
#' @rdname zonalGeog
#' @exportMethod zonalGeog
methods::setMethod(
	f = "zonalGeog",
	signature(x = "GRaster"),
	function(x, unit = "meters") {
	
	.locationRestore(x)
	.region(x)

	units <- c("m", "meters", "kilometers", "km", "miles", "feet", "ft", "yards", "yd", "cells")
	unit <- omnibus::pmatchSafe(unit, units, useFirst = TRUE, nmax = 1L)
	unit <- omnibus::expandUnits(unit)

	flags <- if (unit != "cells") {
		c(.quiet(), "m")
	} else {
		.quiet()
	}

	isFact <- is.factor(x)
	isInt <- is.int(x)

	nLayers <- nlyr(x)
	out <- list()
	for (i in seq_len(nLayers)) {

		if (isInt[i] | isFact[i]) {

			args <- list(
				cmd = "r.object.geometry",
				input = sources(x)[i],
				intern = TRUE
			)

			if (!is.null(flags)) args$flags <- flags
			info <- do.call(rgrass::execGRASS, args = args)

			info <- strsplit(info, split = "\\|")
			n <- length(info) - 1L
			if (isFact[i]) {

				xDrop <- x[[i]]
				xDrop <- droplevels(xDrop)
				labels <- levels(xDrop)[[1L]]
				catName <- names(labels)[activeCat(xDrop) + 1L]
				labels <- labels[[catName]]
				thisOut <- data.table::data.table(
					value = rep(NA_integer_, n),
					label = labels,
					area = rep(NA_real_, n),
					perimeter = rep(NA_real_, n),
					compactSquare = rep(NA_real_, n),
					compactCircle = rep(NA_real_, n),
					fractalDimension = rep(NA_real_, n),
					meanX = rep(NA_real_, n),
					meanY = rep(NA_real_, n)
				)

			# integer raster
			} else {
			
				thisOut <- data.table::data.table(
					value = rep(NA_integer_, n),
					area = rep(NA_real_, n),
					perimeter = rep(NA_real_, n),
					compactSquare = rep(NA_real_, n),
					compactCircle = rep(NA_real_, n),
					fractalDimension = rep(NA_real_, n),
					meanX = rep(NA_real_, n),
					meanY = rep(NA_real_, n)
				)

			} # pre-assign data table

			for (j in 2L:length(info)) {

				val <- info[[j]][1L]
				val <- as.integer(val)

				vals <- info[[j]]
				vals <- as.numeric(vals)

				index <- j - 1L
				thisOut$value[index] <- val
				thisOut$area[index] = vals[2L]
				thisOut$perimeter[index] = vals[3L]
				thisOut$compactSquare[index] = vals[4L]
				thisOut$compactCircle[index] = vals[5L]
				thisOut$fractalDimension[index] = vals[6L]
				thisOut$meanX[index] = vals[7L]
				thisOut$meanY[index] = vals[8L]

			}

			area <- perimeter <- NULL
			if (unit == "kilometers") {
				f <- omnibus::convertUnits(from = "meters2", to = "kilometers2")
				thisOut[ , area := area * f]
				f <- omnibus::convertUnits(from = "meters", to = "kilometers")
				thisOut[ , perimeter := perimeter * f]
			} else if (unit == "miles") {
				f <- omnibus::convertUnits(from = "meters2", to = "miles2")
				thisOut[ , area := area * f]
				f <- omnibus::convertUnits(from = "meters", to = "miles")
				thisOut[ , perimeter := perimeter * f]
			} else if (unit == "feet") {
				f <- omnibus::convertUnits(from = "meters2", to = "feet2")
				thisOut[ , area := area * f]
				f <- omnibus::convertUnits(from = "meters", to = "feet")
				thisOut[ , perimeter := perimeter * f]
			} else if (unit == "yards") {
				f <- omnibus::convertUnits(from = "meters2", to = "yards2")
				thisOut[ , area := area * f]
				f <- omnibus::convertUnits(from = "meters", to = "yards")
				thisOut[ , perimeter := perimeter * f]
			}

		# if raster is NOT integer or factor
		} else {
			thisOut <- data.table::data.table(NULL)
		}

		out[[i]] <- thisOut
	
	} # next layer

	names(out) <- names(x)
	if (!faster("useDataTable")) {
		for (i in seq_along(out)) out[[i]] <- as.data.frame(out)
	}
	out

	} # EOF
)
