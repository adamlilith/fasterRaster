#' Set and get categories for categorical rasters
#'
#' @description `GRaster`s can represent [categorical data][tutorial_categorical_rasters]. The actual call values are integers, but each of these corresponds to a category "name", such as "desert" or "wetland." A categorical raster is associated with a table that matches each integer value to a category name. The two `levels()` functions are related:
#'
#' * `levels()`: Reports category values and their codes (only those represented in the raster).
#' * `levels() <-`: Sets category values.
#'
#' @param x A `GRaster`.
#'
#' @param value A `data.frame`, `data.table`, or a list of `data.frames` or `data.tables`, one per raster layer. The table's first column is the "ID" column and must contain integer values. The second column defines category labels.  (Subsequent columns are ignored.)
#'
#' @returns Values returned are:
#' `levels` list of `data.frame`s or `data.table`s, one per raster.
#' `levels <-` A categorical `GRaster`.
#'
#' @seealso [ncat()], [terra::cats()], [terra::levels()], [terra::addCats()], [terra::droplevels()], [categorical rasters][tutorial_categorical_rasters]
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
	
	if (getFastOptions("useDataTable")) value <- data.table::data.table(value)
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
	signature = c(x = "GRaster", value = "list"),
	function(x, value) {

	if (nlyr(x) != length(value)) stop("There must be one data frame or data table per raster layer in x.")

	.restore(x)

	gns <- .copyGSpatial(x)
	for (i in seq_len(x)) {
	
		# export category values and labels to file to be read in by GRASS
		rules <- paste0(value[[i]][[1L]], "|", value[[i]][[2L]])
		rules <- list(rules)
		tempFile <- tempfile(fileext = ".csv")
		tempFile <- forwardSlash(tempFile)
		
		data.table::fwrite(
			rules,
			file = tempFile,
			na = "NA",
			col.names = FALSE,
			quote = FALSE,
			nThread = getFastOptions("cores")
		)

		args <- list(
			cmd = "r.category",
			map = gns[i],
			separator = "pipe",
			rules = tempFile,
			flags = "quiet",
			intern = TRUE
		)
		do.call(rgrass::execGRASS, args = args)
	
	}
	.makeGRaster(gns, names(x))
	
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
	
		args <- list(
			cmd = "r.category",
			map = .gnames(x)[i],
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
		
		}

	} # next layer
	names(out) <- names(x)
	out

	} # EOF
)
