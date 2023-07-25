#' Sub-setting operator for GRasters and GVectors
#'
#' @description The `[` and `[[` operators do different things depending on whether they are applied to a `GRaster` or `GVector`:
#' * `GVector`s:
#'     * `[` operator: Returns a subset of geometries (i.e., points, lines, or polygons) of the `GVector`. For example, `vector[2:4]` will return the second through the fourth geometries.
#'     * `[[` operator: Returns a vector with the selected columns in its data frame. For example, `vector[[2:4]]` returns the `GVector, but with just columns 2 through 4 of the data frame.
#' * `GRaster`s:
#'     * `[[` operator: Returns `GRaster`s from a "stack" of `GRaster`s. For example, `raster[[2:3]]` returns the second and third rasters in a stack of `GRaster`s.
#'
#' @param x A `GRaster` with one or more layers, or a `GVector`.
#' @param i A character, numeric, integer, or logical vector:
#' * `GVector`s:
#'     * `[` operator: Indicates the geometries/rows to retain. `i` can be a number indicating the index of the rows, or a logical vector the same length as there are rows.
#'     * `[[` operator: Indicates which columns to retain. `i` can be a number indicating the index of the features, a logical vector the same length as there are columns, or a character vector of the names of the columns to keep.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases subset
#' @rdname subset
#' @exportMethod [[
methods::setMethod(
	"[[",
	signature = c(x = "GRaster"),
	function(x, i) {

	# test indices
	if (inherits(i, "character")) {
		if (any(!(i %in% names(x)))) stop("At least one name does not match a raster in this stack.")
		i <- match(i, names(x))
	}
	if (any(!(i %in% seq_len(nlyr(x))))) stop("Index out of bounds.")
	
	mm <- minmax(x)
	
	out <- new(
		"GRaster",
		location = location(x),
		mapset = mapset(x),
		crs = crs(x),
		nLayers = length(i),
		dimensions = dim(x),
		topology = topology(x),
		extent = as.vector(ext(x)),
		zextent = zext(x),
		gnames = .gnames(x)[i],
		names = names(x)[i],
		datatypeGRASS = datatype(x)[i],
		resolution = res(x),
		nCats = ncat(x)[i],
		minVal = mm["min", i],
		maxVal = mm["max", i]
	)
	
	if (length(anyDuplicated(out@names)) > 0L) out@names <- make.unique(out@names)
	out

	} # EOF
)

#' @aliases subset
#' @rdname subset
#' @exportMethod [
methods::setMethod(
	"[",
	signature = c(x = "GVector"),
	function(x, i) {

	nr <- nrow(x)
	if (is.logical(i)) i <- which(i)
	if (any(i < 1L | i > nr)) stop("Index out of bounds.")

	# select rows to drop	
	rows <- seq_len(nr)
	drops <- rows[!(rows %in% i)]

	if (length(i) >= nr) {
		where <- paste0("cat IN (", paste0(drops, collapse = ","), ")")
	} else {
		where <- paste0("cat NOT IN (", paste0(i, collapse = ","), ")")
	}
	
cat("Command line too long when selecting > ~1500 records.")
utils::flush.console()

	gn <- .makeGName(NULL, rastOrVect = "vector")
	args <- list(
		cmd = "v.db.droprow",
		input = .gnames(x),
		layer = .dbLayer(x),
		output = gn,
		where = where,
		flags = c("quiet", "overwrite"),
		intern = FALSE
	)

	do.call(rgrass::execGRASS, args=args)
	.makeGVector(gn)

	} # EOF
)

#' @aliases subset
#' @rdname subset
#' @exportMethod [[
methods::setMethod(
	"[[",
	signature = c(x = "GVector"),
	function(x, i) {

	nc <- ncol(x)
	if (is.logical(i)) {
		i <- which(i)
	} else if (is.character(i)) {
		misses <- !(i %in% names(x))
		if (any(misses)) stop("At least one named column does not exist in this vector\"s data table.")
		i <- which(names(x) %in% i)
	}

	if (any(i < 1L | i > nc)) stop("Index out of bounds.")
	
	cols <- seq_len(nc)
	drops <- cols[!(cols %in% i)]
	drops <- names(x)[drops]

	gn <- .copyGSpatial(x)
	args <- list(
		cmd = "v.db.dropcolumn",
		map = gn,
		columns = drops,
		flags = "quiet",
		layer = "1",
		intern = FALSE
	)

	do.call(rgrass::execGRASS, args=args)
	.makeGVector(gn)

	} # EOF
)
