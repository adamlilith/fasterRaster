#' Contour lines from a "GRaster"
#'
#' @description
#' Create a `GVector` of contour lines from a `GRaster`.
#'
#' @param x A `GRaster`.
#'
#' @param nlevels Numeric: A positive integer or missing (default). Number of levels at which to calculate contours. Levels will be calculated in equal-sized steps from the smallest to the largest value of `x`. Either `nlevels` or `levels` must be specified.
#'
#' @param levels Numeric vector: A numeric vector of values at which to calculate contour lines. Either `nlevels` or `levels` must be specified.
#'
#' @seealso [terra::as.contour()], **GRASS** manual page for module `r.contour` (see `grassHelp("r.contour")`)
#'
#' @returns A `GVector` representing contour lines.
#'
#' @example man/examples/ex_asContour.r
#'
#' @aliases as.contour
#' @rdname as.contour
#' @exportMethod as.contour
setMethod(
	"as.contour",
	signature(x = "GRaster"),
	function(x, nlevels, levels) {
	
	.locationRestore(x)
	.region(x)

	if (!missing(nlevels) & !missing(levels)) stop("Please specify either `nlevels` or `levels`, but not both.")
	if (missing(nlevels) & missing(levels)) stop("Please specify either `nlevels` or `levels`.")

	if (!missing(nlevels)) {
		mm <- minmax(x)
		levels <- seq(mm[1L, 1L], mm[2L, 1L], length.out=nlevels + 2L) # adding 2 bc creates no contours for min/max values
	}

	input <- sources(x)
	src <- .makeSourceName("r_contour", "vector")

	### execute
	args <- list(
		cmd = "r.contour",
		input = input,
		output = src,
		levels = levels,
		cut = 2,
		flags = .quiet()
	)
	
	do.call(rgrass::execGRASS, args=args)
	.makeGVector(src)
	
	} # EOF
)
