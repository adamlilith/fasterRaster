#' Combine red, green, and blue color bands to make a composite GRaster
#'
#' @description This function takes as arguments three rasters typically representing red, green, and blue color bands, and returns a single raster with values based on their combination. Typically, this raster should be plotted in grayscale.
#'
#' @param r,g,b Either:
#' * One `GRaster` with one band each for `r`, `g`, or `b` representing red, green, and blue color bands; or
#' * `r` is  single `GRaster` with 3 bands (R, G, and B bands), and `g` and `b` are `NULL`.
#'
#' @param levels Either a single value that is an integer, or a vector of integers: Number of levels of red, green, and blue intensities represented in `r`, `g`, and `b`. If a single value is supplied, it is assumed that all three have the same number of levels. If three values are supplied, then they correspond to the R, G, and B bands. The default is 256 (assume that R, G, and B rasters have values between 0 and 255).
#'
#' @param dither Logical: If `TRUE`, apply Floyd-Steinberg dithering. Default is `FALSE`.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_plot.r
#'
#' @seealso [plotRGB()], [terra::plotRGB()]
#'
#' @aliases compositeRGB
#' @rdname compositeRGB
#' @exportMethod compositeRGB
methods::setMethod(
	f = "compositeRGB",
	signature(r = "GRaster"),
	function(r, g = NULL, b = NULL, levels = 256, dither = FALSE) {

	msg <- paste0("Argument ", sQuote("r"), " must have 1 band (in which case arguments ", sQuote("g"), " and ", sQuote("b"), " must also be single-layer GRasters),\n  or ", sQuote("r"), " must have 3 bands (and ", sQuote("g"), " and ", sQuote("b"), " must be NULL).")

	if (!(nlyr(r) %in% c(1L, 3L))) stop(msg)

	if (nlyr(r) == 3L) {
 		if (!is.null(g) | !is.null(b)) stop(msg)
		g <- r[[2L]]
		b <- r[[3L]]
		r <- r[[1L]]
	} else if (nlyr(r) == 1L) {
		if (nlyr(g) != 1L | nlyr(b) != 1L) stop(msg)
	}

	compareGeom(r, g)
	compareGeom(r, b)
	.locationRestore(r)
	.region(r)

	if (!(length(levels %in% c(1L, 3L)))) stop("Argument ", sQuote("levels"), " must have 1 or 3 values.")
	if (length(levels) == 1L) levels <- rep(levels, 3L)

 	src <- .makeSourceName("r_composite", "raster")
	args <- list(
		cmd = "r.composite",
		red = sources(r),
		green = sources(g),
		blue = sources(b),
		level_red = levels[1L],
		level_green = levels[2L],
		level_blue = levels[3L],
		output = src,
		flags = c(.quiet(), "overwrite")
	)

	if (dither) args$flags <- c(args$flags, "d")

	do.call(rgrass::execGRASS, args = args)
	.makeGRaster(src, "compositeRGB")
	
	} # EOF
)
