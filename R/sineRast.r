#' Sine wave rasters
#'
#' @description This function creates one or more rasters sine waves in the north-south and east-west directions.
#'
#' @param x A `GRaster`.
#'
#' @param ns,ew Numeric: Number of sine waves (i.e., wavelengths) in the north-south and east-west directions. A wavelength of 1 creates a "full" sine wave (e.g., starting at 0 at one end and ending at 0 at the other). A wavelength of 2 would create two such waves, and so on. A value of 0 creates no waves in the given direction (i.e., each row or column has constant values). The default value is 1.
#'
#' @param nsOffset,ewOffset Numeric: Offset of the sine waves from the edges of the raster, expressed as a proportion of the length of the raster. The default is 0, so the values of the outermost cells will be 0 (or nearly so, depending on machine precision) since the waves "begin" and "end" at the edges. If an offset value is 0.2, for example, then it will be pushed "inward" toward the middle of the raster by 20% of the raster's extent.
#'
#' @param mask Either `NULL` (default), or a `GRaster` or `GVector`: Used as a mask for the output. If this is a `GVector`, then only cells that are not `NA` in the mask have values. If this is a `GVector`, then only cells that overlap the vector have SEV values. All other values will be `NA`.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_sineRast.r
#'
#' @aliases sineRast
#' @rdname sineRast
#' @exportMethod sineRast
methods::setMethod(
	f = "sineRast",
	signature = c(x = "GRaster"),
	function(x, ns = 1, ew = 1, nsOffset = 0, ewOffset = 0, mask = NULL) {

	if (ns < 0 | ew < 0) stop("Arguments `ns` and `ew` must be >= 0.")

	.locationRestore(x)
	.region(x)

	if (nlyr(x) > 1) x <- x[[1L]]

	src <- .makeSourceName("sineRast", "raster")

	### create expression
	# no waves
	if (omnibus::compareFloat(ns, 0, "==") & omnibus::compareFloat(ew, 0, "==")) {
		ex <- paste0(src, " = 0")
	# waves in at least on direction
	} else {

		# scale wavelength
		extent <- ext(x, vector = TRUE)

		# set cell values to [0, 2* pi] along extent
		ymin <- extent[3L] + 0.5 * res(x)[2L]
		ymax <- extent[4L] - 0.5 * res(x)[2L]
		yext <- ymax - ymin

		xmin <- extent[1L] + 0.5 * res(x)[1L]
		xmax <- extent[2L] - 0.5 * res(x)[1L]
		xext <- xmax - xmin

		ewOffset <- 360 * ewOffset
		nsOffset <- 360 * nsOffset

		if (ns > 0 & ew > 0) {
			ex <- paste0(src, " = sin(", ewOffset, " + ", ew * 360, " * (x() - ", xmin, ") / ", xext, ") + sin(", nsOffset, " + ", ns * 360, " * (y() - ", ymin, ") / ", yext, ")")
		} else if (ns > 0 & omnibus::compareFloat(ew, 0, "==")) {
			ex <- paste0(src, " = sin(", nsOffset, " + ", ns * 360, " * (y() - ", ymin, ") / ", yext, ")")
		} else if (omnibus::compareFloat(ns, 0, "==") & ew > 0) {
			ex <- paste0(src, " = sin(", ewOffset, " + ", ew * 360, " * (x() - ", xmin, ") / ", xext, ")")
		}

	}
	rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

	# mask
	if (!is.null(mask)) {

		if (inherits(mask, "GRaster")) {
			maskType <- "raster"
		} else if (inherits(mask, "GVector")) {
			maskType <- "vector"
		} else {
			stop("Argument `mask` must be a `GRaster` or `GVector`.")
		}

		src <- .mask(x = src, mask = sources(mask), maskType = maskType, inverse = FALSE, maskvalues = NA, updatevalue = NA)
	
	}

	.makeGRaster(src, "sine")

	} # EOF
)
