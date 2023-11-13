#' Subset a GRaster layer, or return values from a column of a GVector's table
#'
#' @description The `dollar` notation can be used to get a single layer of a multi-layer `GRaster` or the values of a column from a `GVector`'s data table.
#'
#' @param x A `GRaster` or `GVector`.
#' @param name Character: The name of a `GRaster` or of a column of a `GVector`'s data table. Names of rasters and vectors can be found using [names()].
#' @returns A `GRaster` or vector of the same type as the `GVector`'s column.
#'
#' @example man/examples/ex_GRaster_GVector_subset_assign.r
#' 
#' @name $
#' @aliases $,GRaster-method
#' @docType methods
#' @rdname subset_dollar
#' @exportMethod $
methods::setMethod(
    f = "$",
    signature = c(x = "GRaster"),
    function(x, name) {
	
	# test indices
	i <- .layerIndex(i, x, recycle = TRUE)
	mm <- minmax(x)
	
	new(
		"GRaster",
		location = location(x),
		mapset = mapset(x),
		crs = crs(x),
		projection = .projection(x),
		nLayers = length(i),
		dimensions = dim(x),
		topology = topology(x),
		extent = as.vector(ext(x)),
		zextent = zext(x),
		sources = sources(x)[i],
		names = names(x)[i],
		datatypeGRASS = datatype(x, "GRASS")[i],
		resolution = res(x),
		minVal = mm["min", i],
		maxVal = mm["max", i],
		activeCat = x@activeCat[i],
		levels = cats(x)[i]
	)
	
	} # EOF
)

#' @name $
#' @aliases $,GVector-method
#' @docType methods
#' @rdname subset_dollar
#' @exportMethod $
methods::setMethod(
	"$",
	signature = c(x = "GVector"),
	function(x, name) x@table[[name]]
)
