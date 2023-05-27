#' Sub-setting operator for GRasters and GVectors
#'
#' @description The `[` and `[[` operators do different things depending on whether they are applied to a `GRaster` or `GVector`:
#' * `GVector`s:
#'     * `[` operator: Returns a subset of geometries (i.e., points, lines, or polygons) of the `GVector`. For example, `vector[2:4]` will return the second through the fourth geometries.
#'     * `[[` operator: Returns columns of the vector's data frame. For example, `vector[[2:4]]` returns columns 2 through 4 of the data frame.
#' * `GRaster`s:
#'     * `[[` operator: Returns `GRaster`s from a "stack" of `GRaster`s.
#'
#' @param x A `GRaster` with one or more layers, or a `GVector`.
#' @param i A character, numeric, integer, or logical vector:
#' * `GVector`s:
#'     * `[` operator: Indicates the columns to extract. `i` can be the name of the column(s), a number indicating the index of the columns, or a logical vector the same length as there are columns.
#'     * `[[` operator: Indicates which geometries to extract. `i` can be a number indicating the index of the features, or a logical vector the same length as there are rows.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases subset
#' @rdname subset
#' @exportMethod [[
methods::setMethod(
	'[[',
	signature = c(x = 'GRaster'),
	function(x, i) {

	# test indices
	if (inherits(i, 'character')) {
		if (any(!(i %in% names(x)))) stop('At least one name does not match a raster in this stack.')
		i <- match(i, names(x))
	}
	if (any(!(i %in% seq_len(nlyr(x))))) stop('Index out of bounds.')
	
	mm <- minmax(x)
	
	out <- new(
		'GRaster',
		location = location(x),
		mapset = mapset(x),
		crs = crs(x),
		nLayers = length(i),
		dimensions = dim(x),
		topology = topology(x),
		extent = as.vector(ext(x)),
		zextent = zext(x),
		gnames = gnames(x)[i],
		names = names(x)[i],
		datatypeGRASS = datatype(x)[i],
		resolution = res(x),
		nCats = ncat(x)[i],
		minVal = mm['min', i],
		maxVal = mm['max', i]
	)
	
	if (length(anyDuplicated(out@names)) > 0L) out@names <- make.unique(out@names)
	out

	} # EOF
)

#' @aliases subset
#' @rdname subset
#' @exportMethod [[
methods::setMethod(
	'[[',
	signature = c(x = 'GVector'),
	function(x, i) {

	out <- as.data.frame(x)
	out <- out[ , i, drop=FALSE]	
	out

	} # EOF
)

