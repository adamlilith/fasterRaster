#' Terrain ruggedness index
#'
#' @description For a given focal grid cell, the terrain ruggedness index (TRI) is calculated by taking the square root of the average of the squared difference between the focal cell's elevation and the elevations of the 8 surrounding cells, or \deqn{\sqrt(\sum_{i = 1}^{8}(m_i - m_0)^2 / 8)} where \eqn{m_0} is the elevation of the focal cell and \eqn{m_i} is the elevation of the *i*th grid cell. Areas that are entirely flat will have a value of `NA` assigned to them.
#'
#' By default, TRI is calculated for a 3x3 moving window around each focal cell. However, you can use a larger-sized window. In this case, the window size must be an odd number >= 3, and you must have the `r.tri` **GRASS** addon installed. If it is not installed, the function will try to install it.
#'
#' @param x A `GRaster`.
#' @param size Integer (default is 3): Size of the moving window. Must be an odd integer >= 3.
#' @param exponent Numeric >= 0 and <= 4. Used to reduce the influence of cells farther from the focal cell (larger areas can yield noisier results if the exponent small). All cells are weighted equally when `exponent = 0`.
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
	function(x, size = 3, exponent = 0) {

	if (size %% 2 != 1) stop("Argument `size` must be an odd integer >= 3.")
	if (exponent < 0 | exponent > 4) stop("Argument `exponent` must in the range [0, 4].")
	if (size > 3) .addons("r.tri")

	.locationRestore(x)
	.region(x)

	nLayers <- nlyr(x)
	srcs <- .makeSourceName("terrainRuggednessIndex_r_mapcalc", "raster", n = nLayers)
	for (i in seq_len(nLayers)) {
	
		if (size == 3) {

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

		} else {
		
			rgrass::execGRASS(
				cmd = "r.tri",
				input = sources(x)[i],
				output = srcs[i],
				size = size,
				exponent = exponent,
				# processes = faster("cores"), # does not create raster
				flags = c(.quiet(), "overwrite")
			)
		
		}

	} # next layer
	.makeGRaster(srcs, names = paste0(names(x), "_TRI"))

	} # EOF
)
