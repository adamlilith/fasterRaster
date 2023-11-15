#' Statistics on cells of a GRaster stratified by cells of another raster
#'
#' @description Function `zonal()` calculates statistics (mean, sum, etc.) on cells of a raster by "zones" created by cells of another or vector.
#'
#' @param x A `GRaster` for which to calculate summary statistics.
#'
#' @param z A `GRaster` or `GVector` used to define zones: If `z` is a `GRaster`, then it must be of type [integer or factor][tutorial_raster_data_types]. Zones will be established based on cells that have the same value in this raster. If `z` is a `GVector`, zones will be created for each geometry. If geometries overlap, then some cells may be summarized more than once.
#'
#' @param fun Character vector: Name of the function(s) to summarize `x` with. These can include:
#' * `"countNonNA"`: Total number of non-`NA` cells.
#' * `"countNA"`: Total number of `NA` cells.
#' * `"cv"`: Sample coefficient of variation (expressed as a proportion of the mean).
#' * `"cvpop"`: Population coefficient of variation (expressed as a proportion of the mean).
#' * `"max"` and `"min"`: Highest and lowest values across non-`NA` cells.
#' * `"mean"` (default): Average.
#' * `"meanAbs"`: Mean of absolute values.
#' * `"median"`: Median.
#' * `"quantile"`: Quantile (see also argument `prob`).
#' * `"range"`: Range.
#' * `"sd"`: Sample standard deviation.
#' * `"sdpop"`: Population standard deviation.
#' * `"sum"`: Sum.
#' * `"var"`: Sample variance.
#' * `"varpop"`: Population variance.
#'
#' @param prob Numeric: Quantile at which to calculate `quantile`. Only a single value between 0 and 1 is allowed.
#'
#' @returns A `data.frame` or `data.table`.
#'
#' @example man/examples/ex_zonal.r
#'
#' @aliases zonal
#' @rdname zonal
#' @setMethod zonal
methods::setMethod(
	f = "zonal",
	signature = c(x = "GRaster", z = "GRaster"),
	function(x, z, fun = "mean", w = NULL, prob = 0.5) {
	
	if (!is.int(z)[1L] & !is.factor(z)[1L]) stop("GRaster ", sQuote("z"), " must be an integer or factor raster, or a GVector.")

	if (nlyr(z) > 1L) {
	
		warning("The zone raster, ", sQuote("z"), ", has more than one layer. Only the first will be used.")
	
		z <- z[[1L]]
	
	}

	compareGeom(x, z)
	.restore(x)
	region(x)

	### get zone labels
	if (is.factor(z)) z <- droplevels(z)
	freqs <- freq(z)
	zones <- freqs[["value"]]
	numZones <- length(zones)

	### calculate statistics
	nLayers <- nlyr(x)
	out <- data.table()
	for (i in seq_len(nLayers)) {
	
		srcs <- .makeSourceName("r_mapcalc", "raster", numZones)
	
		for (j in seq_along(zones)) {
		
			ex <- paste0(srcs[j], " = if (", sources(z), " == ", zones[j], ", ", sources(x)[i], ", null())")
		
			rgrass::execGRASS(
				cmd = "r.mapcalc",
				expression = ex,
				flags = c(.quiet(), "overwrite")
			)

			thisOut <- .global(srcs[j], fun = fun, prob = prob, nLayers = nLayers)

			prepend <- data.table::data.table(layer = i, name = names(x)[i], zone = zones[j])

			thisOut <- cbind(prepend, thisOut)
		
			if (nrow(out) == 0L) {
				out <- thisOut
			} else {
				out <- rbind(out, thisOut)
			}

		} # next zone in z
	
	} # next x layer

	if (!getFastOptions("useDataTable")) out <- as.data.frame(out)
	out
	
	} # EOF
)
