#' Display a raster or vector
#'
#' @description `plot()` displays a `GRaster` or `GVector`.
#'
#' This function is essentially a hack, as it it not possible to dependably call the appropriate **GRASS** modules and display a raster or vector without potential confusion on the user side. Instead, this function 1) simplifies the focal `GRaster` or `GVector` to make it smaller when saved to disk; 2) writes the object to disk; 3) (internally) creates a `SpatRaster` or `SpatVector` object; then 4) plots the object using [terra::plot()]. Thus, if you are interested in making maps, it will always be faster to make them directly with **terra** or **sf**.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param y Missing--leave as empty.
#'
#' @param simplify Logical: If `TRUE` (default) and the raster has an x- and/or y-resolution that is greater than the screen resolution, then [aggregate()] will be applied to the raster before it is saved to reduce the time it takes to save the raster.
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
	function(x, y, simplify = TRUE, ...) {

	# simplify
	if (simplify & !is.3d(x)) {
		
		# screenRes <- rpanel::rp.screenresolution()
		screenRes <- omnibus::screenRes()
		screenWidth <- screenRes[["width"]]
		screenHeight <- screenRes[["width"]]

		rows <- nrow(x)
		cols <- ncol(x)
		# pow <- 5 # power by which to reduce dimensions, set at 5 semi-empirically
		if (screenWidth < cols | screenHeight < rows) {
		
			ewScale <- max(1, floor(0.5 * cols / screenWidth))
			nsScale <- max(1, floor(0.5 * rows / screenHeight))

			if (ewScale >= 2 | nsScale >= 2) {

				if (any(datatype(x) %in% c("integer", "factor"))) {
					fun <- "mode"
				} else {
					fun <- "mean"
				}

				fact <- c(ewScale, nsScale)
				x <- aggregate(x, fact = fact, fun = fun)

			}
			
		}

	}

	# save temporary file
	tf <- tempfile(fileext = ".tif")
	writeRaster(x, filename = tf, format = "GeoTIFF", bigTiff = TRUE, overwrite = TRUE, warn = FALSE, ...)
	out <- terra::rast(tf, lyrs = 1L:nlyr(x))
	
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
#' @keywords internal
.plot <- function(x, ...) {
	files <- .ls()
	what <- names(files)[files == x]
	if (what == "raster") {
		x <- makeGRaster(x)
	} else if (what == "vector") {
		x <- makeGVector(x)
	}
	plot(x, ...)
}
