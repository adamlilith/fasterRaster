#' Create stream network
#'
#' @description This function estimates the course of streams and rivers from an elevation raster. It is based on the **GRASS** module `r.stream.extract`, where more details can be found (see `grassHelp("r.stream.extract")`)
#'
#' @param x A `GRaster` representing elevation.
#'
#' @param accumulation Either `NULL` (default) or a raster representing flow accumulation. If not supplied, an accumulation will created internally. You can generate an accumulation raster using [flow()].
#'
#' @param depression Either `NULL` (default) or a `GRaster` representing depressions (areas from which streams will not flow out of).
#'
#' @param flowThreshold Numeric > 0: Minimum threshold for a stream to be generated. The default is 1, which is not necessarily a reasonable value.
#'
#' @param dirThreshold Numeric (default is `Inf`): When flow exceeds this threshold, its direction is estimated using a single-flow direction algorithm. Below this threshold, a multi-direction flow model is used. This is the `d8cut` parameter in `r.stream.extract`, and it is only used if `accumulation` is `NULL`.  The default is 1, which is not necessarily a reasonable value.
#'
#' @param montgomery Numeric: The "Montgomery" exponent for slope, multiplied by accumulation as per `accumulation * slope^montgomery`. This value is then compared to the threshold to determine if it is sufficient. The default is 0 (i.e., no slope scaling).
#'
#' @param minLength Numeric: First-order streams less than this length are removed (units in cells). Default is 0 (no removal).
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_streams.r
#'
#' @seealso [flow()], [flowPath()], **GRASS** module `r.stream.extract` (see `grassHelp("r.stream.extract")`)
#'
#' @aliases streams
#' @rdname streams
#' @exportMethod streams
methods::setMethod(
	f = "streams",
	signature = "GRaster",
	function(
		x,
		accumulation = NULL,
		depression = NULL,
		flowThreshold = 1,
		dirThreshold = 1,
		montgomery = 0,
		minLength = 1
	) {

	if (nlyr(x) > 1L) stop("Only a single-layer GRaster can be supplied to `x`.")
	if (!is.null(accumulation) && nlyr(accumulation) > 1L) stop("Only a single-layer GRaster can be supplied to `accumulation`.")

	.locationRestore(x)
	.region(x)
	if (!is.null(accumulation)) compareGeom(x, accumulation)

	src <- .makeSourceName("streams_r_stream_extract", "raster")
	args <- list(
		cmd = "r.stream.extract",
		stream_raster = src,
		elevation = sources(x),
		threshold = flowThreshold,
		d8cut = dirThreshold,
		mexp = montgomery,
		stream_length = minLength,
		memory = faster("memory"),
		flags = c(.quiet(), "overwrite")
	)
	
	do.call(rgrass::execGRASS, args = args)
	.makeGRaster(src, names = "streams")
	
	} # EOF
)
