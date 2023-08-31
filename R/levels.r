#' Set and get categories for categorical rasters
#'
#' @description `GRaster`s can represent [categorical data][tutorial_raster_data_types]. Cell values are actually integers, each corresponding to a category, such as "desert" or "wetland." A categorical raster is associated with a table that matches each value to a category name. The two `levels()` functions are related:
#'
#' * `levels()`: Reports category values and their codes (only those represented in the raster).
#' * `levels() <-`: Sets category values.
#'
#' @param x A `GRaster`.
#'
#' @param value A `data.frame`, `data.table`, a list of `data.frames` or `data.tables` with one per raster layer, or a categorical `SpatRaster`. The table's first column is the "value" column and must contain numeric values (of class `numeric` or `character`). If a `SpatRaster` is supplied, then its categories will be transferred to the `GRaster`.
#'
#' @returns Values returned are:
#' `levels` list of `data.frame`s or `data.table`s, one per raster.
#' `levels <-` A categorical `GRaster`.
#'
#' @seealso [ncat()], [terra::cats()], [terra::levels()], [terra::addCats()], [terra::droplevels()], [categorical rasters][tutorial_raster_data_types]
#'
#' @example man/examples/ex_GRaster_categorical.r
#'
#' @aliases levels<-
#' @rdname levels
#' @exportMethod levels<-
methods::setMethod(
	f = "levels<-",
	signature = c(x = "GRaster", value = "data.frame"),
	function(x, value) {
	
	if (getFastOptions("useDataTable")) value <- data.table::as.data.table(value)
	value <- list(value)
	levels(x) <- value
	
	} # EOF
)

#' @aliases levels<-
#' @rdname levels
#' @exportMethod levels<-
methods::setMethod(
	f = "levels<-",
	signature = c(x = "GRaster", value = "data.table"),
	function(x, value) {
	
	value <- list(value)
	levels(x) <- value
	
	} # EOF
)

#' @aliases levels<-
#' @rdname levels
#' @exportMethod levels<-
methods::setMethod(
	f = "levels<-",
	signature = c(x = "GRaster", value = "SpatRaster"),
	function(x, value) {

	value <- levels(value)
	levels(x) <- value
	x

	} # EOF
)


#' @aliases categories
#' @rdname levels
#' @exportMethod categories
methods::setMethod(
	f = "categories",
	signature = c(x = "GRaster"),
	function(x, layer = 1, value, active = 1) {

	.restore(x)

	# export category values and labels to file to be read in by GRASS
	if (inherits(value, "data.table")) {
		vals <- value[[1L]]
		labs <- value[[active + 1L]]
	} else if (inherits(value, "data.frame")) {
		vals <- value[ , 1L, drop = TRUE]
		labs <- value[ , active + 1L, drop = TRUE]
	}
	
	rules <- paste0(vals, "|", labs)
	rules <- list(rules)
	tempFile <- tempfile(fileext = ".csv")
	tempFile <- forwardSlash(tempFile)

	# using fwrite because write.csv quotes everything, and this breaks
	data.table::fwrite(
		rules,
		file = tempFile,
		na = "<undefined>",
		col.names = FALSE,
		quote = FALSE,
		nThread = getFastOptions("cores")
	)
	
	args <- list(
		cmd = "r.category",
		# map = gns[i],
		map = sources(x)[layer],
		separator = "pipe",
		rules = tempFile,
		flags = "quiet",
		intern = TRUE
	)
	info <- do.call(rgrass::execGRASS, args = args)
	.refresh(x)

	} # EOF
)

#' @aliases levels
#' @rdname levels
#' @exportMethod levels
methods::setMethod(
	f = "levels",
	signature = c(x = "GRaster"),
	function(x) {
	
	.restore(x)
	out <- list()
	nLayers <- nlyr(x)

	for (i in seq_len(nLayers)) {
	
		if (datatype(x)[i] != "CELL") {

			out[[i]] <- ""

		} else {

			args <- list(
				cmd = "r.category",
				map = sources(x)[i],
				separator = "pipe",
				flags = "quiet",
				intern = TRUE
			)
			info <- do.call(rgrass::execGRASS, args = args)

			info <- strsplit(info, split = "\\|")
			if (all(sapply(info, length) == 1L)) {
				out[[i]] <- ""
			} else {

				info <- data.table::data.table(
					Value = sapply(info, "[[", 1L),
					Label = sapply(info, "[[", 2L)
				)
				if (!getFastOptions("useDataTable")) info <- as.data.frame(info)

				out[[i]] <- info
			
			} # if CELL raster

		}

	} # next layer
	names(out) <- names(x)
	out

	} # EOF
)
