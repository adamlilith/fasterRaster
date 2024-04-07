#' Subset layers from a GRaster, or specific columns from a GVector
#'
#' @description The `[[` operator can be used to subset or remove one or more layers from a `GRaster`. It can also be used to subset or remove columns from a `GVector` with a data table.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param i Numeric integer, integer, logical, or character: Indicates the layer(s) of a `GRaster` to subset, or the column(s) of a `GVector` to return. If negative numeric or integer values are supplied, then these layers or columns will be removed from the output.
#'
#' @param j Ignored for `[[`.
#'
#' @returns A `GRaster` or `GVector`.
#'
#' @example man/examples/ex_GRaster_GVector_subset_assign.r
#'
#' @name [[
#' @aliases [[,GRaster,ANY,ANY-method
#' @docType methods
#' @rdname subset_double_square_brackets
#' @exportMethod [[
methods::setMethod(
	"[[",
	signature = c(x = "GRaster", i = "ANY", j = "ANY"),
	function(x, i, j) {

	# test indices
	i <- .layerIndex(i, x, recycle = TRUE)
	mm <- minmax(x)
	dims <- dim(x)[1L:3L]
	
	out <- new(
		"GRaster",
		location = .location(x),
		mapset = .mapset(x),
		workDir = faster("workDir"),
		crs = crs(x),
		projection = .projection(x),
		nLayers = length(i),
		dimensions = dims,
		topology = topology(x),
		extent = as.vector(ext(x)),
		zextent = zext(x),
		sources = sources(x)[i],
		names = names(x)[i],
		datatypeGRASS = datatype(x, "GRASS")[i],
		resolution = res3d(x),
		minVal = mm["min", i],
		maxVal = mm["max", i],
		activeCat = x@activeCat[i],
		levels = cats(x)[i]
	)
	
	if (length(anyDuplicated(out@names)) > 0L) out@names <- make.unique(out@names)
	out

	} # EOF
)

#' @name [[
#' @aliases [[,GVector,ANY,ANY-method
#' @docType methods
#' @rdname subset_double_square_brackets
#' @exportMethod [[
methods::setMethod(
	"[[",
	signature = c(x = "GVector", i = "ANY", j = "ANY"),
	function(x, i, j) {

	nc <- ncol(x)
	if (nc == 0L) stop("Vector has no data table associated with it.")

	# turn i into integer
	if (is.logical(i)) {
		if (length(i) <- nc) i <- rep(i, length.out = nc)
		i <- which(i)
	} else if (is.character(i)) {
		i <- match(i, names(x))
  		if (anyNA(i)) stop("At least one named column does not exist in this vector\'s data table.")
	}

	if (any(i < 0L) & any(i > 0L)) stop("Cannot have both positive and negative indices.")

	if (all(i < 0L)) {
		reverseSelect <- TRUE
		i <- i * -1L
		removeAll <- length(i) == ncol(x) && all(sort(i) == seq_len(ncol(x)))
	} else {
		reverseSelect <- removeAll <- FALSE
	}

	if (any(i > nc)) stop("Index out of bounds.")
	
	# removed all columns
	if (removeAll) {
		x@table <- data.table::data.table(NULL)
	# keep some columns
	} else {

		# ..i <- NULL
		if (reverseSelect) {
			i <- setdiff(names(x), names(x)[i])
		} else {
			i <- names(x)[i]
		}

		x@table <- x@table[ , ..i, with = FALSE]
	}
	x
	
	} # EOF
)
