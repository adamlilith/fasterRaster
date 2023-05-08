#' Sub-setting operator for GRasters
#'
#' Get a subset of a "stack" of `GRaster`s. Note that using the `[[` operator for `GRaster`s and `GVector`s makes a copy of the object in **GRASS** so is not fast. If you are simply wanting to obtain properties of a particular set of raster or vector layers, use [properties()].
#'
#' @param x A `GRaster` or `GVector` with more than one layer.
#' @param i A character (`GRaster`s only), numeric, or integer vector: Indicates the name(s) or indices of the objects to access.
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
	.subset(x)
	} # EOF
)

#' @aliases subset
#' @rdname subset
#' @exportMethod [[
methods::setMethod(
	'[[',
	signature = c(x = 'GVector'),
	function(x, i) {
	.subset(x)
	} # EOF
)

.subset <- function(x, i) {

	### test indices
	if (inherits(i, 'character')) {
		if (inherits(x, 'GVector')) stop('Index cannot be a character when subsetting GVectors.')
		if (any(!(i %in% names(x)))) stop('At least one name does not match a raster in this stack.')
		i <- match(i, names(x))
	}
	if (any(!(i %in% seq_len(nlyr(x))))) stop('Index out of bounds.')
	
	type <- fastClass(x)

	if (inherits(x, 'GRaster')) {
		
		mm <- minmax(x)
		z <- zext(x)
		out <- new(
			'GRaster',
			location = location(x),
			mapset = mapset(x),
			crs = crs(x),
			nLayers = length(i),
			dimensions = c(nrow(x)[i], ncol(x)[i], ndepth(x)[i]),
			topology = topology(x)[i],
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
		
	} else if (inherits(x, 'GVector')) {
	
		z <- zext(x)
		out <- new(
			'GVector',
			location = location(x),
			mapset = mapset(x),
			crs = crs(x),
			nLayers = length(i),
			gnames = gnames(x)[i],
			extent = as.vector(ext(x)),
			ztop = zext(x)[1L],
			zbottom = zext(x)[2L],
			nFields = ncat(x)[i],
			fields = names(x)[[i]],
			fieldClasses = ncat(x)[i]
		)
	
	}
	
	if (inherits(out, 'GRaster') && length(anyDuplicated(out@names)) > 0L) out@names <- make.unique(out@names)
	out

}