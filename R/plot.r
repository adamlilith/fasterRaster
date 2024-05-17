#' Display a raster or vector
#'
#' @description `plot()` displays a `GRaster` or `GVector`.
#'
#' This function is essentially a hack, as it it not possible to dependably call the appropriate **GRASS** modules and display a raster or vector without potential confusion on the user side. Instead, this function 1) simplifies the focal `GRaster` or `GVector` (if needed); 2) writes it to disk as a `SpatRaster` or `SpatVector`; and 3) plots the object using [terra::plot()]. Thus, if you are interested in making maps, it will always be faster to make them directly with **terra**.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param y Missing--leave as empty.
#'
#' @param maxcell Positive integer (rasters only): Maximum number of cells a raster can have before it is simplified before saving to disk then plotted. The [aggregate()] function will be applied so that it has approximately this number of cells. The default is 5000000.
#'
#' @param maxGeoms Positive integer (vectors only): Maximum number of features before vector simplification is applied before saving to disk then creating a `SpatVector` for plotting. The default is 10000.
#' 
#' @param ... Other arguments to send to [terra::plot()].
#'
#' @returns Nothing (displays a raster or vector).
#'
#' @seealso [terra::plot()]
#'
#' @example man/examples/ex_plot.r
#'
#' @aliases plot
#' @rdname plot
#' @exportMethod plot
methods::setMethod(
	f = "plot",
	signature = c(x = "GRaster", y = "missing"),
	function(x, y, maxcell = 5000000, ...) {

	# simplify
	nc <- ncell(x)
	if (nc > maxcell) {

		rows <- nrow(x)
		cols <- ncol(x)
		rescale <- ceiling(nc / (2 * maxcell))

		if (all(datatype(x) %in% c("integer", "factor"))) {
			fun <- "mode"
		} else {
			fun <- "mean"
		}

		if (rescale != 1L) x <- aggregate(x, fact = rescale, fun = fun)

	}
	
	dtype <- datatype(x, "GRASS")
	if (all(dtype %in% "CELL")) {
	
		if (all(.minVal(x) >= 0L & .maxVal(x) <= 255L)) {
			dtype <- "Byte"
		} else if (all(.minVal(x) >= 0L & .maxVal(x) <= 65534L)) {
			dtype <- "UInt16"
		} else if (all(.minVal(x) >= -32767L & .maxVal(x) <= 32767L)) {
			dtype <- "Int16"
		} else if (all(.minVal(x) >= -2147483647L & .maxVal(x) <= 2147483647L)) {
			dtype <- "Int32"
		} else if (all(.minVal(x) >= -3.4E+38 & .maxVal(x) <= 3.4E+38)) {
			dtype <- "Float32"
		} else {
			dtype <- "Float64"
		}

	} else {
		dtype <- "Float64"
	}

	tf <- tempfile(fileext = ".tif")
	writeRaster(x, filename = tf, format = "GeoTIFF", overwrite = TRUE, warn = FALSE, datatype = dtype, ...)
	out <- terra::rast(tf)
	
	facts <- is.factor(x)
	if (any(facts)) {
	
		for (i in which(facts)) {
			levels(out[[i]]) <- levels(x)[[i]]
		}
	
	}
	
	names(out) <- names(x)
	terra::plot(out, ...)
	
	} # EOF
)

#' @aliases plot
#' @rdname plot
#' @exportMethod plot
methods::setMethod(
	f = "plot",
	signature = c(x = "GVector", y = "missing"),
	function(x, y, maxGeoms = 10000, ...) {
	
	# simplify
	if (ngeom(x) > maxGeoms) x <- simplifyGeom(x)
	
	tf <- tempfile(fileext = ".gpkg")
	writeVector(x, filename = tf, format = "GPKG", overwrite = TRUE, attachTable = FALSE)
	v <- terra::vect(tf)
	terra::plot(v, ...)
	
	} # EOF
)

#' Plot using a G-object's [sources()] name
#'
#' @param x The [sources()] name of a `GRaster` or `GVector`
#' @param ... Other arguments to pass to [plot()].
#'
#' @noRd
.plot <- function(x, ...) {
	files <- .ls()
	what <- names(files)[files == x]
	if (what == "raster") {
		x <- .makeGRaster(x)
	} else if (what == "vector") {
		x <- .makeGVector(x)
	}
	plot(x, ...)
}
