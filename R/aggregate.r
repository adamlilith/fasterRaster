#' Aggregate raster cells into larger cells or combine geometries of a vector
#'
#' @description When applied to a `GRaster`, `aggregate()` creates a new raster with cells that are a multiple of the size of the cells of the original raster. The new cells can be larger or smaller than the original cells (this function thus emulates both the `terra::aggregate()` and [terra::disagg()] functions in **terra**.)
#' 
#' When applied to a `GVector`, all geometries are combined into a "multipart" geometry, in which geometries are treated as if they were a single unit. Borders between aggregated geometries can be dissolved if the `dissolve` argument is `TRUE`. If the `GVector` has a data table associated with it, the output will also have a data table as long as there is at least one column with values that are all the same. Values of columns that do not have duplicated values will be converted to `NA`.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param fact Numeric vector (rasters only): One, two, or three positive values. These reflect the size of the new cells as multiples of the size of the old cells. If just one value is supplied, this is used for all two or three dimensions. If two values are supplied, the first is multiplied by the east-west size of cells, and the second north-south size of cells (the raster must be 2D). If three values are supplied, the third value is used as the multiplier of the vertical dimension of cells. Values are calculated using all cells that have their centers contained by the target cell.
#'
#' Note that unlike `terra::aggregate()` and [terra::disagg()], these values need not be integers.
#'
#' @param fun Character (rasters only): Name of the function used to aggregate. For `GRaster`s, this is the function that summarizes across cells. For `GVector`s, this function will be used to calculate new values of numeric or integer cells.
#' * `mean`: Average (default)
#' * `median`: Median
#' * `mode`: Most common value
#' * `min`: Minimum
#' * `max`: Maximum
#' * `range`: Difference between maximum and minimum
#' * `sum`: Sum
#' * `varpop`: Population variance
#' * `sdpop`: Population standard deviation
#' * `quantile`: Quantile (see argument `prob`)
#' * `count`: Number of non-`NA` cell
#' * `diversity`: Number of unique values
#'
#' @param prob Numeric (rasters only): Quantile at which to calculate `quantile`.
#'
#' @param na.rm Logical (rasters only): If `FALSE` (default), propagate `NA` cells or `NA` values.
#'
#' @param weight Logical (rasters only): If `FALSE`, each source cell that has its center in the destination cell will be counted equally. If `TRUE`, the value of each source will be weighted the proportion of the destination cell the source cell covers.
#' 
#' @param dissolve Logical (vectors only): If `TRUE` (default), then aggregated geometries will have their borders dissolved. This is ignored if the input `GVector` is not a "polygons" vector.
#' 
#' @returns A `GRaster` or `GVector`.
#' 
#' @seealso [stats::aggregate()], [terra::aggregate()], [diagg()], [terra::disagg()]
#'
#' @example man/examples/ex_aggregate_disagg.r
#'
#' @aliases aggregate
#' @rdname aggregate
#' @exportMethod aggregate
methods::setMethod(
	f = "aggregate",
	signature = c(x = "GRaster"),
	definition = function(
		x,
		fact = 2,
		fun = "mean",
		weight = FALSE,
		prob = NULL,
		na.rm = FALSE
	) {

	if (any(fact <= 0)) stop("Values of ", sQuote("fact"), " must be > 0.")

	funs <- c("mean", "median", "mode", "min", "maximum", "range", "quantile", "sum", "varpop", "sdpop", "count", "diversity")
	fun <- omnibus::pmatchSafe(tolower(fun), funs)
	
	if (fun == "mean") {
		fun <- "average"
	} else if (fun == "min") {
		fun <- "minimum"
	} else if (fun == "max") {
		fun <- "maximum"
	} else if (fun == "varpop") {
		fun <- "variance"
	} else if (fun == "sdpop") {
		fun <- "stdev"
	} else if (fun == "quantile") {
		
		if (is.null(prob)) stop("A value must be specified for ", sQuote("prob"), " if the aggregating function is ", sQuote("quantile"), ".")
		
		if (prob < 0 | prob > 1) stop("Argument ", sQuote("prob"), " must be in the range [0, 1].")
	
	}
	
	.locationRestore(x)
	.region(x)

	if (is.2d(x)) {
		
		if (length(fact) == 1L) fact <- rep(fact, 2L)
		if (length(fact) == 3L) stop("This is a 2D raster. Only 1 or 2 values are allowed for ", sQuote("resol"), ".")
	
		resol <- res(x)
		resol <- resol * fact
		.regionRes(resol, respect="extent")
	
	} else if (is.3d(x)) {
	
		if (length(fact) == 1L) fact <- rep(fact, 3L)
		if (length(fact) == 2L) {
			warning("This is a 3D raster, but ", sQuote("resol"), " has only 2 values.\n  Assuming third dimension will not be aggregated.")
			fact[3L] <- 1
		}
	
		resol <- res3d()
		resol <- resol * fact
		.regionRes(resol)

	}

	args <- list(
		cmd = "r.resamp.stats",
		input = NA_character_,
		output = NA_character_,
		method = fun,
		flags = c(.quiet(), "overwrite"),
		intern = TRUE
	)
	
	if (weight) args$flags <- c(args$flags, "w")
	if (!na.rm) args$flags <- c(args$flags, "n")
	if (fun == "quantile") args <- c(args, quantile = prob)
	
	nLayers <- nlyr(x)
	for (i in seq_len(nLayers)) {
		
		src <- .makeSourceName(names(x)[i], "rast")
		
		args$input <- sources(x)[i]
		args$output <- src
		
		do.call(rgrass::execGRASS, args=args)
		
		if (fun %in% c("median", "mode", "min", "max")) {
			levs <- levels(x[[i]])
		} else {
			levs <- NULL
		}

		this <- .makeGRaster(src, names(x)[i], levels = levs)
		if (i == 1L) {
			out <- this
		} else {
			out <- c(out, this)
		}
		
	}
	out

	} # EOF
)

#' @aliases aggregate
#' @rdname aggregate
#' @exportMethod aggregate
methods::setMethod(
	f = "aggregate",
	signature = c(x = "GVector"),
	function(x, dissolve = TRUE) {

	.locationRestore(x)
	gtype <- geomtype(x)
	src <- .aggregate(x, dissolve = dissolve, gtype = gtype, copy = TRUE)

	# aggregate data table
	if (nrow(x) == 0L) {
		aggTable <- NULL
	} else {

		table <- as.data.table(x)
			
		if (nrow(x) == 1L) {
			aggTable <- table
		} else {

			dups <- rep(FALSE, ncol(table))
			rowsMinus1 <- nrow(table) - 1L
			
			for (i in 1L:ncol(table)) {
				if (sum(duplicated(table[ , ..i])) == rowsMinus1) dups[i] <- TRUE
			}

			if (!any(dups)) {
				aggTable <- NULL
			} else {

				aggTable <- table[1L, ]
				classes <- sapply(table, class)
				
				for (i in 1L:ncol(table)) {

					if (!dups[i]) {
						
						if (classes[i] == "character") {
							assign <- NA_character_
						} else if (classes[i] == "integer") {
							assign <- NA_integer_
						} else if (classes[i] == "numeric") {
							assign <- NA_real_
						} else {
							assign <- NA
						}
						aggTable[1L, i] <- assign
					}
				} # for each data table column

			} # if there is at least one entirely-duplicated row

		} # if table has >= 1 row

	} # if data table has >= 2 rows

	.makeGVector(src, table = aggTable)

	} # EOF
)

#' @param x `GVector` or [sources()] name
#' @param gtype geomtype (fasterRaster)
#' @param dissolve Logical
#' @param copy Logical: If TRUE, make copy of vector before operations
#' @noRd
.aggregate <- function(x, gtype, dissolve, copy) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
		src <- sources(x)
	} else {
		src <- x
	}
	
	if (copy) src <- .copyGVector(src)
	
	oldcats <- .vCats(src, db = FALSE)
	if (length(oldcats) > 1L) {

		table <- data.table::data.table(fr = rep(1L, length(oldcats)))
		.vAttachDatabase(src, table = table, replace = TRUE)

		# newcats <- data.frame(oldfr = oldcats, fr = rep(1L, length(oldcats)))
		
		# tf <- tempfile(fileext = ".csv")
		# tft <- paste0(tf, "t")
		# utils::write.csv(newcats, tf, row.names = FALSE)
		# # tableType <- '"Integer","Integer"'
		# tableType <- '"Integer"'
		# write(tableType, tft)

		# # import table with new categories
		# srcTable <- .makeSourceName("db_in_ogr", "table")
		
		# rgrass::execGRASS(
			# cmd = "db.in.ogr",
			# input = tf,
			# output = srcTable,
			# flags = c(.quiet(), "overwrite")
		# )

		# # connect table to copy of vector
		# srcIn <- .copyGVector(src)

		# rgrass::execGRASS(
			# cmd = "v.db.join",
			# map = srcIn,
			# other_table = srcTable,
			# other_column = "oldfr",
			# column = "fr",
			# flags = .quiet()
		# )

		srcIn <- src
		src <- .makeSourceName("v_reclass", "vector")
		
		rgrass::execGRASS(
			"v.reclass",
			input = srcIn,
			output = src,
			column = "fr",
			flags = c(.quiet(), "overwrite")
		)

		if (dissolve & gtype == "polygons") {
					
			table <- data.table::data.table(fr = rep(1L, length(oldcats)))

			src <- .copyGSpatial(x)
			.vAttachDatabase(src, table = table, replace = TRUE)

			srcIn <- src
			src <- .makeSourceName("v_extract", "vector")

			rgrass::execGRASS(
				cmd = "v.extract",
				input = srcIn,
				output = src,
				new = 1L,
				flags = c(.quiet(), "overwrite", "d", "t")
			)
		
		}

	} # if >1 geometry

	src

} # EOF
