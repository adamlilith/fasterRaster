#' Aggregate values of raster cells into larger cells
#'
#' @description When applied to a `GRaster`, `aggregate()` creates a new raster with cells that are a multiple of the size of the cells of the original raster. The new cells can be larger or smaller than the original cells (this function thus emulates the `terra::aggregate()` and [terra::disagg()] functions in **terra**.)
#' 
#' When applied to a `GVector`, `aggregate()` all geometries into a single "multipart" geometry, in which sets of points, lines, or polygons are treated as if they were a unit. If the `GVector` has a data table associated with it, the output will also have a data table as long as there is at least one column with values that are all the same. Columns that do not have duplicated values will be converted to `NA`.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param fact Numeric vector (rasters only): One, two, or three positive values. These reflect the size of the new cells as multiples of the size of the old cells. If just one value is supplied, this is used for all two or three dimensions. If two values are supplied, the first is multiplied by the east-west size of cells, and the second north-south size of cells (the raster must be 2D). If three values are supplied, the third value is used as the multiplier of the vertical dimension of cells. Values are calculated using all cells that have their centers contained by the target cell.
#'
#' Note that unlike `terra::aggregate()` and [terra::disagg()], these values need not be integers.
#'
#' @param fun Character (rasters only): Name of the function used to aggregate. For `GRaster`s, this is the function that summarizes across cells. For `GVector`s, this function will be used to calculate new values of numeric or integer cells.
#' * `mean``: Average (default)
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
#' @param by Either a character, a numeric or integer vector, or `NULL` (vectors only):
#' * If `NULL` (default), then all geometries will be collated into a single multipart geometry.
#' * If a character, then rows with the same value in the named column of the vector's data table will be aggregated.
#' * If a numeric integer or integer vector, the vector must have the same length as the number of geometries (see [ngeom()]). The values must be integers. Geometries with the same value will be aggregated together.
#' 
#' @param dissolve Logical (vectors only): If `TRUE` (default), then aggregated geometries will have their borders dissolved. This is ignored if the input `GVector` is not a "polygons" vector.
#' 
#' @returns A `GRaster` or `GVector`.
#' 
#' @seealso [stats::aggregate()], [terra::aggregate()], [terra::disagg()], **GRASS** module `r.resamp.stats`
#'
#' @example man/examples/ex_aggregate.r
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
	fun <- pmatchSafe(tolower(fun), funs)
	
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
	
	.restore(x)
	region(x)

	if (is.2d(x)) {
		
		if (length(fact) == 1L) fact <- rep(fact, 2L)
		if (length(fact) == 3L) stop("This is a 2D raster. Only 1 or 2 values are allowed for ", sQuote("resol"), ".")
	
		resol <- res(x)
		resol <- resol * fact
		regionRes(resol, respect="extent")
	
	} else if (is.3d(x)) {
	
		if (length(fact) == 1L) fact <- rep(fact, 3L)
		if (length(fact) == 2L) {
			warning("This is a 3D raster, but ", sQuote("resol"), " has only 2 values.\n  Assuming third dimension will not be aggregated.")
			fact[3L] <- 1
		}
	
		resol <- res3d()
		resol <- resol * fact
		regionRes(resol)

	}

	args <- list(
		cmd = "r.resamp.stats",
		input = NA_character_,
		output = NA_character_,
		method = fun,
		flags = c("quiet", "overwrite"),
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
		
		this <- .makeGRaster(src, names(x)[i])
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

	.restore(x)

	# use v.reclass to reclassify
	oldcats <- .vCats(x, table = TRUE)
	
	newcats <- data.frame(oldcat = oldcats, newcat = rep(1L, length(oldcats)))
	
	tf <- tempfile(fileext = ".csv")
	tft <- paste0(tf, "t")
	utils::write.csv(newcats, tf, row.names = FALSE)
	tableType <- '"Integer"'
	write(tableType, tft)

	# import table with new categories
	srcTable <- .makeSourceName("db_in_ogr", "table")
	
	rgrass::execGRASS(
		cmd = "db.in.ogr",
		input = tf,
		output = srcTable,
		flags = c("quiet", "overwrite")
	)

	# connect table to copy of vector
	srcIn <- .copyGVector(x)

	rgrass::execGRASS(
		cmd = "v.db.join",
		map = srcIn,
		column = "cat",
		other_table = srcTable,
		other_column = "oldcat",
		flags = "quiet"
	)

	src <- .makeSourceName("v_reclass", "vector")
	
	rgrass::execGRASS("v.reclass", input = srcIn, output = src, column = "newcat", flags = c("quiet", "overwrite"))

	if (dissolve & geomtype(x) == "polygons") {
	
		srcIn <- src
		src <- .makeSourceName("v_dissolve", "vector")

		rgrass::execGRASS(
			cmd = "v.dissolve",
			input = srcIn,
			output = src,
			column = "cat",
			flags = c("quiet", "overwrite")
		)
	
	}

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
