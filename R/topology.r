#' Topology (2- or 3-dimensions) of a 'GRaster' or 'GVector'
#'
#' Rasters and vectors in **GRASS** can have 2-dimensional or 3-dimensional coordinates. This function returns the dimension of the coordinates of a `GSpatial` object or any that inherits from it (i.e., a `GRaster` or `GVector`).
#'
#' @param x A `GSpatial` object (i.e., a `GRaster` or `GVector`).
#'
#' @example man/examples/example_topology.r
#'
#' @export

# if (!isGeneric('topology')) setGeneric(name='topology', def=function(x) standardGeneric('topology'))
setMethod(
	f = 'topology',
	signature = 'GSpatial',
	definition = function(x) x@topology
)
