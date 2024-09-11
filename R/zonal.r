#' Statistics on cells of a GRaster stratified by cells of another raster
#'
#' @description Function `zonal()` calculates statistics (mean, sum, etc.) on cells of a `GRaster` by "zones" created by cells of another `GRaster` or `GVector`.
#'
#' @param x A `GRaster` for which to calculate summary statistics.
#'
#' @param z A `GRaster` or `GVector` used to define zones:
#' * If `z` is a `GRaster`, then it must be of type [integer or factor][tutorial_raster_data_types]. Zones will be established based on cells that have the same value in this raster.
#' * If `z` is a `GVector`, zones will be created for each geometry. If geometries overlap, then the zonal statistics will be calculated for the ones on top. Thus statistics for the zones defined by geometries below these may not represent all the cells covered by that geometry.
#'
#' @param fun Character vector: Name of the function(s) to summarize `x` with. These can include:
#' * `"*"`: All of the functions below.
#' * `"cv"`: Sample coefficient of variation (expressed as a proportion of the mean).
#' * `"cvpop"`: Population coefficient of variation (expressed as a proportion of the mean).
#' * `"max"` and `"min"`: Highest and lowest values across non-`NA` cells.
#' * `"mean"` (default): Average.
#' * `"meanAbs"`: Mean of absolute values.
#' * `"median"`: Median.
#' * `"quantile"`: Quantile (see also argument `probs`).
#' * `"range"`: Range.
#' * `"sd"`: Sample standard deviation.
#' * `"sdpop"`: Population standard deviation.
#' * `"sum"`: Sum.
#' * `"var"`: Sample variance.
#' * `"varpop"`: Population variance.
#'
#' @param probs Numeric: Quantile at which to calculate `quantile`. Only a single value between 0 and 1 is allowed.
#'
#' @returns A `data.frame` or `data.table`.
#'
#' @example man/examples/ex_zonal.r
#'
#' @aliases zonal
#' @rdname zonal
#' @exportMethod zonal
methods::setMethod(
	f = "zonal",
	signature = c(x = "GRaster"),
	function(x, z, fun = "mean", probs = 0.5) {
	
	if (inherits(z, "GRaster")) {
		.zonalByRaster(x = x, z = z, fun = fun, probs = probs)
	} else if (inherits(z, "GVector")) {
		gtype <- geomtype(z, grass = TRUE)
		.zonalByVector(x = x, z = z, fun = fun, probs = probs, gtype = gtype)
	} else {
		stop("Argument `z` must be a GRaster or GVector.")
	}

	} # EOF
)

#' @noRd
.zonalByRaster <- function(x, z, fun, probs) {

	if (!is.cell(z)[1L] & !is.factor(z)[1L]) stop("GRaster `z` must be an integer or factor raster, or a GVector.")

	if (nlyr(z) > 1L) {
	
		warning("The zone raster in `z` has more than one layer. Only the first will be used.")
		z <- z[[1L]]
	
	}

	compareGeom(x, z)
	.locationRestore(x)
	.region(x)

	### get zone labels
	if (is.factor(z)) z <- droplevels(z)
	freqs <- freq(z)
	zones <- freqs[["value"]]
	
	.zonal(
		x = sources(x),
		z = sources(z),
		fun = fun,
		probs = probs,
		zones = zones,
		xnames = names(x)
	)

}

#' Internal function for zonal() when y is a GVector
#'
#' @param x	GRaster [sources()] name.
#' @param z	GVector [sources()] name.
#' @param fun	Character
#' @param gtype `geomtype(z, grass = TRUE)` ("area", "line", or "point")
#'
#' @keywords internal
.zonalByVector <- function(x, z, fun, probs, gtype) {

	compareGeom(x, z)
	.locationRestore(x)
	.region(x)

	# rasterize by field in data table
	cats <- .vCats(z)
	uniCats <- unique(cats)
	db <- data.table::data.table(cat = uniCats, recats = uniCats)
	.vAttachDatabase(z, db)

	zonalSrc <- .makeSourceName("zonal_v_to_rast", "raster")
	args <- list(
		cmd = "v.to.rast",
		input = sources(z),
		output = zonalSrc,
		use = "attr",
		attribute_column = "recats",
		type = gtype,
		memory = faster("memory"),
		flags = c(.quiet(), "overwrite")
	)

	if (gtype == "line") args$flags <- c(args$flags, "d")
	do.call(rgrass::execGRASS, args = args)
	if (faster("clean")) on.exit(.rm(zonalSrc, type = "raster", warn = FALSE), add = TRUE)
	
	zones <- .freq(zonalSrc, dtype = "CELL")
	zones <- zones[["value"]]

	.zonal(
		x = sources(x),
		z = zonalSrc,
		fun = fun,
		probs = probs,
		zones = zones,
		xnames = names(x)
	)

}

#' Internal function for zonal()
#' 
#' @param x [sources()] name of `GRaster`.
#' @param z [sources()] name of "zones" `GRaster`.
#' @param fun Character: Name of function(s).
#' @param probs Numeric in [0, 1].
#' @param zones Vector of zone values (integers).
#' @param xnames Character: Names of `x`.
#' @keywords internal
.zonal <- function(x, z, fun, probs, zones, xnames) {

	### get zone labels
	numZones <- length(zones)

	### calculate statistics
	nLayers <- length(x)
	out <- data.table()
	for (i in seq_len(nLayers)) {
	
		srcs <- .makeSourceName("zonal_r_mapcalc", "raster", numZones)
	
		for (j in seq_along(zones)) {
		
			ex <- paste0(srcs[j], " = if (", z, " == ", zones[j], ", ", x[i], ", null())")
		
			rgrass::execGRASS(
				cmd = "r.mapcalc",
				expression = ex,
				flags = c(.quiet(), "overwrite")
			)

			thisOut <- .global(x = srcs[j], fun = fun, probs = probs)

			prepend <- data.table::data.table(layer = i, name = xnames[i], zone = zones[j])

			thisOut <- cbind(prepend, thisOut)
			if (nrow(out) == 0L) {
				out <- thisOut
			} else {
				out <- rbind(out, thisOut)
			}

		} # next zone in z
	
	} # next x layer

	if (!faster("useDataTable")) out <- as.data.frame(out)
	out

}
