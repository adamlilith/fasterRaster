#' Topographic wetness index
#'
#' @description This function creates a raster map with values equal to the topographic wetness index (TWI), which is a measure of how much overland water flow tends to accumulate in or flow away from a location.
#'
#' @param x A `GRaster` (typically representing elevation). The raster must be projected (i.e., not in WGS84, NAD83, et cetera).
#'
#' @returns A `GRaster`.
#'
#' @seealso [terrain()], [ruggedness()], [geomorphons()], module [`r.topidx`](https://grass.osgeo.org/grass84/manuals/r.topidx.html) in **GRASS**
#'
#' @example man/examples/ex_ruggedness_wetness.r
#'
#' @aliases wetness
#' @rdname wetness
#' @exportMethod wetness
methods::setMethod(
	f = "wetness",
	signature = c(x = "GRaster"),
	function(x) {
	
	if (.projection(x) == "Latitude-Longitude") stop("Raster must be projected before calculating TWI.")

	.locationRestore(x)
	.region(x)

	nLayers <- nlyr(x)
	srcs <- .makeSourceName("r_topidx", "raster", n = nLayers)

	for (i in seq_len(nLayers)) {
	
		rgrass::execGRASS(
			cmd = "r.topidx",
			input = sources(x)[i],
			output = srcs[i],
			flags = c(.quiet(), "overwrite")
		)
	
	}
	.makeGRaster(srcs, paste0(names(x), "_twi"))

	} # EOF
)
