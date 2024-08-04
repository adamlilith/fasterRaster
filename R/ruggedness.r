#' Terrain ruggedness index
#'
#' @description For a given focal grid cell, the terrain ruggedness index (TRI) is calculated by taking the square root of the average of the squared difference between the focal cell's elevation and the elevations of the 8 surrounding cells, or \deqn{\sqrt(\sum_{i = 1}^{8}(m_i - m_0)^2 / 8)} where \eqn{m_0} is the elevation of the focal cell and \eqn{m_i} is the elevation of the *i*th grid cell.
#'
#' @param x A `GRaster`.
#'
#' @returns A `GRaster`.
#'
#' @seealso [terrain()], [wetness()], [geomorphons()]
#'
#' @references Riley, S.J., DeGloria, S.D., and Elliot, R. 1999. A terrain ruggedness index that quantifies topographic heterogeneity. *Intermountain Journal of Sciences* 5:23-27.
#'
#' @example man/examples/ex_ruggedness_wetness.r
#'
#' @aliases ruggedness
#' @rdname ruggedness
#' @exportMethod ruggedness
methods::setMethod(
	f = "ruggedness",
	signature = c(x = "GRaster"),
	function(x) {

	.locationRestore(x)
	.region(x)

	nLayers <- nlyr(x)
	srcs <- .makeSourceName("terrainRuggednessIndex_r_mapcalc", "raster", n = nLayers)
	for (i in seq_len(nLayers)) {
	
		ex <- paste0(srcs[i], " = sqrt((",
			"(", sources(x)[i], "[-1, -1] - ", sources(x), ")^2 + ",
			"(", sources(x)[i], "[-1, 0] - ", sources(x), ")^2 + ",
			"(", sources(x)[i], "[-1, 1] - ", sources(x), ")^2 + ",
			"(", sources(x)[i], "[0, -1] - ", sources(x), ")^2 + ",
			"(", sources(x)[i], "[0, 1] - ", sources(x), ")^2 + ",
			"(", sources(x)[i], "[1, -1] - ", sources(x), ")^2 + ",
			"(", sources(x)[i], "[1, 0] - ", sources(x), ")^2 + ",
			"(", sources(x)[i], "[1, 1] - ", sources(x), ")^2) / 8)")
		
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

	} # next layer
	.makeGRaster(srcs, names = paste0(names(x), "_TRI"))

	} # EOF
)
