#' Convert a GVector to a GRaster
#'
#' @description The `rasterize()` function converts a `GVector` into a `GRaster`.
#'
#' @param x A `GVector`.
#'
#' @param y A `GRaster`: The new raster will have the same extent and resolution as this raster.
#'
#' @param field Character: Name of a column in the data table of `y` to "burn" into the raster. If not `""` (default), then the output will be a [categorical][tutorial_raster_data_types] raster. If `field` is `""`, then all geometries will be "burned" to the raster and have the same value.
#'
#' @param background Numeric or `NA` (default): Value to put in cells that are not covered by the `GVector`. Note that if this is not `NA` and not an integer, then the output cannot be a categorical raster (i.e., there will be no "levels" table associated with it).
#'
#' @param by Either `NULL` (default) or character: If this is not `NULL`, then the `GVector` will be subset by the values in the field named by `by`. The output will be a multi-layer raster, with one layer per unique value in `by`.
#'
#' @param verbose Logical: If `by` is not `NULL`, display progress.
#'
#' @returns A `GRaster`.
#'
#' @seealso [terra::rasterize()], module [`v.to.rast`](https://grass.osgeo.org/grass84/manuals/v.to.rast.html) in **GRASS**
#'
#' @example man/examples/ex_rasterize.r
#'
#' @aliases rasterize
#' @rdname rasterize
#' @exportMethod rasterize
methods::setMethod(
	f = "rasterize",
	signature = c(x = "GVector", y = "GRaster"),
	function(
		x,
		y,
		field = "",
		background = NA,
		by = NULL,
		verbose = TRUE
	) {
	
	compareGeom(x, y)
	.locationRestore(x)
	.region(y)

	out <- .rasterize(x = x, y = y, field = field, background = background, by = by, verbose = verbose)
	.makeGRaster(out$src, levels = out$levels)

	} # EOF
)

#' @param x [sources()] name of a `GVector`.
#' @param y [sources()] name of a `GRaster`
#' @param field Name of a field in the data table of `x` (can be `""`).
#' @param background Numeric or `NA`.
#' @param by `NULL` or name of a field in the data table of `x`.
#' @param verbose Logical.
#'
#' @returns A `list` with the [sources()] name of the output raster, plus a `levels` table (can be `NULL`).
#'
#' @noRd
.rasterize <- function(x, y, field, background, by, verbose) {

	### create different raster layer for each geometry
	if (!is.null(by)) {

		bys <- unique(x@table[[by]])
		nBys <- length(bys)
		src <- rep(NA_character_, nBys)
		levels <- list()
		if (verbose & nBys > 1L) pb <- utils::txtProgressBar(min = 0, max = nBys, initial = 0, style = 3)

		for (i in seq_len(nBys)) {

			if (verbose) {
				utils::setTxtProgressBar(pb, i)
				utils::flush.console()
			}

			index <- which(x@table[[by]] == bys[i])
			xx <- x[index]

			thisOut <- .rasterize(xx, y, field = field, background = background, by = NULL, verbose = FALSE)
			src[i] <- thisOut$src
			levels[[i]] <- thisOut$levels
		
		}
		if (verbose) close(pb)

	} else {

		gtype <- geomtype(x, grass = TRUE)
		src <- .makeSourceName("rasterize_v_to_rast", "raster")

		# if by geometry but burned to the same raster
		if (field == "") {

			levels <- data.table::data.table(NULL)

			args <- list(
				cmd = "v.to.rast",
				input = sources(x),
				output = src,
				use = "val",
				value = 1,
				type = gtype,
				memory = faster("memory"),
				flags = c(.quiet(), "overwrite")
			)
		
		} else {
			
			# rasterize by field in data table
			cats <- .vCats(x)
			uniCats <- sort(unique(cats))
			fieldVals <- x@table[[field]]
			renums <- omnibus::renumSeq(fieldVals)
			levels <- data.table::data.table(value = unique(renums), DUMMYDUMMY_ = unique(fieldVals))
			names(levels)[2L] <- field
			levels <- levels[order(value)]
			db <- data.table::data.table(cat = uniCats, DUMMYDUMMY_ = renums)
			.vAttachDatabase(x, db)

			args <- list(
				cmd = "v.to.rast",
				input = sources(x),
				output = src,
				use = "attr",
				attribute_column = "DUMMYDUMMY_",
				type = gtype,
				memory = faster("memory"),
				flags = c(.quiet(), "overwrite")
			)

		}

		if (gtype == "line") args$flags <- c(args$flags, "d")
		do.call(rgrass::execGRASS, args = args)

		if (!is.na(background)) {

			srcIn <- src
			src <- .makeSourceName("rasterize_r_mapcalc", "vector")
			ex <- paste0(src, " = if(isnull(", srcIn, "), ", background, ", ", srcIn, ")")

			rgrass::execGRASS("r.mapcalc", expression  = ex, flags = c(.quiet(), "overwrite"))
			if (faster("clean")) .rm(srcIn, type = "raster", warn = FALSE)

			if (nrow(levels) > 0L) {
			
				if (is.integer(background) | omnibus::is.wholeNumber(background)) {

					background <- as.integer(background)

					levAdd <- data.table::data.table(value = background, DUMMYDUMMY_ = "background")
					names(levAdd) <- names(levels)
					levels <- rbind(levels, levAdd)
					levels <- levels[!duplicated(levels)]
					levels <- levels[order(value)]

				} else {
					levels <- data.table::data.table(NULL)
				}
				
			}

		}

	}

	list(src = src, levels = levels)

}
