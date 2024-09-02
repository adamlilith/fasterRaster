#' Create red-green-blue plot from a raster with RGB layers
#'
#' @description This function takes as its main argument a `GRaster` with at least three layers typically representing red, green, and blue components (plus possibly an "alpha", or transparency layer). As with [plot()], this function is somewhat of a hack in that it downsamples the layers to a coarser resolution using [aggregate()], saves the raster to disk, then uses [terra::plotRGB()] to do the actual plotting.
#'
#' @param x A `GRaster`. Values must be in the range from 0 to 255.
#'
#' @param r,g,b Either a numeric integer or the [names()] of layers representing red, green, and blue components.
#'
#' @param a Either `NULL` (default), or a numeric integer or the [names()] of a layer representing transparency.
#'
#' @param simplify Logical: If `TRUE` (default), then downsample the `GRaster` before plotting. This can save time for very dense rasters.
#'
#' @param ... Arguments to pass to [terra::plotRGB()].
#'
#' @returns Nothing (makes a plot).
#'
#' @seealso [terra::plotRGB()], [plot()], [compositeRGB()]
#'
#' @example man/examples/ex_plot.r
#'
#' @aliases plotRGB
#' @rdname plotRGB
#' @exportMethod plotRGB
methods::setMethod(
	f = "plotRGB",
	signature = c(x = "GRaster"),
	function(x, r = 1, g = 2, b = 3, a = NULL, simplify = TRUE, ...) {
	
	.locationRestore(x)
	.region(x)
	
	# simplify
	# simplify
	if (simplify & !is.3d(x)) {
		
		screenRes <- rpanel::rp.screenresolution()
		screenWidth <- screenRes$width
		screenHeight <- screenRes$height

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
				rgb <- c(r, g, b)
				if (!is.null(a)) rgb <- c(rgb, a)
				xx <- xx[[rgb]]
				x <- aggregate(xx, fact = fact, fun = fun)

			}
			
		}

	}
	
	tf <- tempfile(fileext = ".tif")
	writeRaster(x, tf, overwrite = TRUE)
	xx <- terra::rast(tf)
	
	terra::plotRGB(xx, r = r, g = g, b = b, a = a, ...)
	
	} # EOF
)
