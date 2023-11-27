#' Find which cells of a GRaster match certain values
#'
#' @description The `match()` takes a `GRaster` and a vector as inputs and returns a `GRaster` with cell values that correspond to the index of each element in the vector that equaled the original cell value. For example, if a 4-cell raster had values {3, `NA`, 5, 4}, and the vector was `c(3, 4)`, then the output would be a 3-cell raster with values {1, `NA`, `NA`, 2} because the first value in the vector was 3 (so the cell with 3 is assigned 1), and because the second value in the vector was 4 (so the cell with 4 was assigned 2).
#'
#' If the `GRaster` is [categorical][tutorial_raster_data_types], then the vector can be category labels instead of numeric values.
#'
#' The `%in%` operator returns a `GRaster` with cell values that are 1 if their original values appeared in the vector, and 0 if not (or `NA` if the original value was `NA`). If the `GRaster` is categorical, then the vector can be category labels instead of numeric values.
#'
#' The `%notin%` operator returns 1 for cells with values that are *not* found in the vector, and 0 otherwise. If the `GRaster` is categorical, then the vector can be category labels instead of numeric values.
#'
#' @param x A `GRaster`: Note that any kind of `GRaster` is acceptable (integer, float, double, or categorical), but matching may not work as intended for float and double rasters because of problems with comparing floating-point values.
#'
#' @param table A numeric, integer, or character vector.
#'
#' @param nomatch Numeric or integer: Value to return when no match is found.
#'
#' @returns A `GRaster`.
#'
#' @seealso [terra::match()], [match()], [omnibus::notIn()]
#'
#' @example man/examples/ex_match.r
#'
#' @aliases match
#' @rdname match
#' @exportMethod match
methods::setMethod(
	f = "match",
	signature = c(x = "GRaster"),
	function(x, table, nomatch = NA) {
	
	if (is.character(table) & !all(is.factor(x))) {
		stop("Can only match category labels to categorical rasters.")
	}

	.restore(x)
	region(x)

	nLayers <- nlyr(x)
	srcs <- .makeSourceName("r_mapcalc", "raster", nLayers)

	for (i in seq_len(nLayers)) {
	
		if (is.character(table)) {

			levs <- levels(x)[[i]]
			ac <- activeCat(x)[i]
			labels <- levs[[ac + 1L]]
			matches <- match(table, labels)
			vals <- levs[[1L]][matches]

		} else {
			vals <- table
		}
	
		ex <- paste0(srcs[i], " = if(isnull(", sources(x)[i],"), null(), if(", sources(x)[i], "==", vals[1L], ",1,")

		if (length(vals) > 1L) {
			for (count in 2L:length(vals)) {
				ex <- paste0(ex, "if(", sources(x)[i], "==", vals[count], ",", count, ",")
			}
			
			ex <- if (is.na(nomatch)) {
				paste0(ex, "null())")
			} else {
				paste0(ex, nomatch, ")")
			}
			
			for (count in 2L:length(vals)) {
				ex <- paste0(ex, ")")
			}
		}

		ex <- paste0(ex, ")")

		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)

	} # next raster

	# if (!is.na(nomatch)) {
	
	# 	srcsIn <- srcs
	# 	srcs <- .makeSourceName("r_mapcalc", "raster", nLayers)
	# 	for (i in seq_len(nLayers)) {

	# 		ex <- paste0(srcs[i], " = if(isnull(", sources(x)[i], "), null(), ", srcsIn[i], ")")
	# 		rgrass::execGRASS(
	# 			cmd = "r.mapcalc",
	# 			expression = ex,
	# 			flags = c(.quiet(), "overwrite")
	# 		)

	# 	}
	
	# }

	# add category levels
	if (is.character(table)) {
	
		levs <- .freq(srcs, dtype = rep("CELL", length(srcs)))

  		if (!inherits(levs, "list")) levs <- list(levs)
		acs <- activeCat(x, names = TRUE)

		for (i in seq_along(acs)) {

			if (nrow(levs[[i]]) == 0L) {
				levs[[i]] <- ""
			} else {

				levs[[i]] <- levs[[i]][ , "count" := NULL]
				values <- levs[[i]][["value"]]
				labels <- table[match(values, seq_along(table))]
				labels[is.na(labels)] <- "nomatch"

				levs[[i]][ , acs[i] := labels]

				levs[[i]][ , "value" := lapply(.SD, as.integer), .SDcols = "value"]
				data.table::setorderv(levs[[i]], col = "value")

			}
				
		}
		
	} else {
		levs <- NULL
	}
	.makeGRaster(srcs, names(x), levels = levs)
	
	} # EOF
)

#' @aliases %in%
#' @rdname match
#' @exportMethod %in%
methods::setMethod(
	f = "%in%",
	signature = c(x = "GRaster"),
	function(x, table) {
	
	.restore(x)
	region(x)

	if (is.character(table) & !all(is.factor(x))) {
		stop("Can only match category labels to categorical rasters.")
	}

	nLayers <- nlyr(x)
	srcs <- .makeSourceName("r_mapcalc", "raster", nLayers)

	for (i in seq_len(nLayers)) {
	
		if (is.character(table)) {

			levs <- levels(x)[[i]]
			ac <- activeCat(x)[i]
			labels <- levs[[ac + 1L]]
			matches <- match(table, labels)
			vals <- levs[[1L]][matches]

		} else {
			vals <- table
		}

		ex <- paste0(srcs[i], " = if(isnull(", sources(x)[i], "), null(), if(", sources(x)[i], "==", vals[1L])

		if (length(vals) > 1L) {
			for (count in 2L:length(vals)) {
				ex <- paste0(ex, "|", sources(x)[i], "==", vals[count])
			}
		}

		ex <- paste0(ex, ",1,0))")

		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)
	
	} # next raster

	.makeGRaster(srcs, names(x))

	} # EOF
)

#' @aliases %notin%
#' @rdname match
#' @exportMethod %notin%
methods::setMethod(
	f = "%notin%",
	signature = c(x = "GRaster"),
	function(x, table) {
	
	.restore(x)
	region(x)

	if (is.character(table) & !all(is.factor(x))) {
		stop("Can only match category labels to categorical rasters.")
	}

	nLayers <- nlyr(x)
	srcs <- .makeSourceName("r_mapcalc", "raster", nLayers)

	for (i in seq_len(nLayers)) {
	
		if (is.character(table)) {

			levs <- levels(x)[[i]]
			ac <- activeCat(x)[i]
			labels <- levs[[ac + 1L]]
			matches <- match(table, labels)
			vals <- levs[[1L]][matches]

		} else {
			vals <- table
		}
	
		ex <- paste0(srcs[i], " = if(isnull(", sources(x)[i], "), null(), if(", sources(x)[i], "!=", vals[1L])

		if (length(vals) > 1L) {
			for (count in 2L:length(vals)) {
				ex <- paste0(ex, "&", sources(x)[i], "!=", vals[count])
			}
		}

		ex <- paste0(ex, ",1,0))")

		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)
	
	} # next raster

	.makeGRaster(srcs, names(x))
	
	} # EOF
)
