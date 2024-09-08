#' Correlation between GRasters
#'
#' @description This function returns a correlation or covariance matrix between two or more `GRaster` layers. This function returns the sample correlation and covariance (i.e., the denominator is n - 1).
#' 
#' @param x A `GRaster` with two or more layers.
#' @param fun Character: Name of the statistic to calculate; either `"cor"` (default) or `"cov"`. 
#' 
#' @returns A numeric `matrix`.
#' 
#' @example man/examples/ex_layerCor.r
#'
#' @seealso [terra::layerCor()], [stats::cor()], [stats::cov()]
#' 
#' @aliases layerCor
#' @rdname layerCor
#' @exportMethod layerCor
methods::setMethod(
	f = "layerCor",
	signature = c(x = "GRaster"),
	function(x, fun = "cor") {
	
	if (nlyr(x) == 1L) stop("The raster must have >= 2 layers.")
	fun <- omnibus::pmatchSafe(fun, c("cor", "cov"))
	
	.locationRestore(x)
	.region(x)

	args <- list(
		cmd = "r.covar",
		map = paste(sources(x), collapse = ","),
		flags = c(.quiet()),
		intern = TRUE
	)

	if (fun == "cor") args$flags <- c(args$flags, "r")
	info <- do.call(rgrass::execGRASS, args = args)

	n <- substr(info[1L], 5L, nchar(info[1L]))
	n <- as.integer(n)

	nLayers <- nlyr(x)
	out <- matrix(NA_real_, ncol = nLayers, nrow = nLayers, dimnames = list(names(x), names(x)))

	for (i in seq_len(nLayers)) {

		this <- info[i + 1L]
		this <- strsplit(this, " ")[[1]]
		this <- as.numeric(this)
		out[i, ] <- this

	}
	attr(out, "n") <- n
	out	
	
	}
)
