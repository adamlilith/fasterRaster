#' Combine values/categories of multiple GRasters into a single GRaster
#'
#' @description This function takes from 2 to 10 integer or categorical (factor) `GRaster`s and creates a single `GRaster` that has one value per combination of values in the inputs. For example, say that there were two input rasters, with values 1 and 2 in the one raster, and 3 and 4 in the other. If the following combinations of values occurred between the two rasters, then the output raster would be re-coded with the new values:
#'
#' | `input_raster1` | `input_raster2` | `output_raster` |
#' | --------------- | --------------- | --------------- |
#' | 1               | 3               | 0               |
#' | 1               | 4               | 1               |
#' | 2               | 3               | 2               |
#' | 2               | 4               | 3               |
#'
#' If the argument `na.rm` is set to `TRUE` (which it is, by default), then whenever at least one cell has an `NA` value, then the output will also have an `NA` (i.e., a new category number is not created). However, if `na.rm` is `FALSE`, then combinations that include an `NA` are assigned a new category number, unless all values are `NA` (in which case the output will be `NA`).
#'
#' The difference between this function and [combineLevels()] is that this one creates a "combined" `GRaster` with a combined levels table, whereas `combineLevels()` just merges the levels tables.
#'
#' If the inputs are all categorical rasters, then a [levels()] table will also be returned with the new levels.
#'
#' @param x A `GRaster` with one or more layers, each of which must be have cells that represent integers or categories (factors).
#'
#' @param ... Either missing or integer/categorical (factor) `GRaster`s.
#'
#' @param na.rm Logical: If `TRUE` (default), then any combinations that include an `NA` cell will result in an `NA` cell in the output.
#'
#' @returns A `GRaster`. If the inputs are all categorical (factor) rasters, then a levels table will also be returned with the new combined levels.
#'
#' @example man/examples/ex_GRaster_categorical.r
#'
#' @seealso [combineLevels()], `vignette("GRasters", package = "fasterRaster")`, [terra::crosstab()]
#'
#' @aliases combineCats
#' @rdname combineCats
#' @exportMethod combineCats
methods::setMethod(
	f = "combineCats",
	signature = c(x = "GRaster"),
	function(x, ..., na.rm = TRUE) {

	.locationRestore(x)
	.region(x)

	dt <- datatype(x)
	if (any(!(dt %in% c("integer", "factor")))) stop("All rasters must be of type integer or factor.")
	
	allFactors <- all(is.factor(x))
	inSrcs <- sources(x)

	dots <- list(...)
	if (length(dots) > 0L) {

		dots <- omnibus::unlistRecursive(dots)
		xFirst <- x[[1L]]
	
		for (i in seq_along(dots)) {

			compareGeom(xFirst, dots[[i]])
			
			dt <- datatype(dots[[i]])
			if (any(!(dt %in% c("integer", "factor")))) stop("All rasters must be of type integer or factor.")
			allFactors <- allFactors & all(is.factor(dots[[i]]))
			
			inSrcs <- c(inSrcs, sources(dots[[i]]))
		
		}
		nLayersDots <- sapply(dots, nlyr)
	} else {
  		nLayersDots <- 0L
	}

	nl <- nlyr(x) + nLayersDots
	if (nl < 2L | nl > 10L) stop("From 2 to 10 raster layers can be combined. Too few/many layers.")

	# combine
	src <- .makeSourceName("r_cross", "raster")
	args <- list(
		cmd = "r.cross",
		input = inSrcs,
		output = src,
		flags = c(.quiet(), "overwrite")
	)

	if (na.rm) args$flags <- c(args$flags, "z")
	do.call(rgrass::execGRASS, args = args)

	# collate levels tables
	if (allFactors) {
	
		# get levels tables from rasters
		levs <- levels(x)
		if (length(dots) > 0L) {
			for (i in seq_along(dots)) levs <- c(levs, levels(dots[[i]]))
		}

		# get levels table from output
		glevs <- rgrass::execGRASS(
			cmd = "r.category",
			map = src,
			separator = "pipe",
			intern = TRUE
		)

		glevs <- sapply(glevs, strsplit, split = "\\|")
		n <- length(glevs)
	
		# populate new levels table
		table <- data.table::data.table(value = rep(NA_integer_, n), category = rep(NA_character_, n))

		for (i in seq_along(glevs))	{

			val <- as.integer(glevs[[i]][1L])
			table$value[i] <- val
			
			gcats <- strsplit(glevs[[i]][2L], split = "; ")[[1L]]
			gcats <- gsub(gcats, pattern = "category ", replacement = "")
			gcats[gcats == "NULL"] <- NA_integer_
			gcats <- as.integer(gcats)

			comboCat <- rep(NA_character_, nl)
			for (j in seq_along(levs)) {

				if (is.na(gcats[j])) {
					comboCat[j] <- "NA"
				} else {
					comboCat[j] <- unlist(levs[[j]][levs[[j]][[1L]] == gcats[j], 2L])
				}

			}

			comboCat <- paste(comboCat, collapse = " | ")
			table$category[i] <- comboCat
		
		}

	} else {
		table <- NULL
	}

	.makeGRaster(src, names = "combineCats", levels = table)
	
	} # EOF
)
