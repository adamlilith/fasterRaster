#' Correlation between GRasters
#'
#' @description `cor()` returns a correlation matrix between two or more `GRaster` layers, and `cov()` returns a covariance matrix between two or more `GRaster` layers.
#' 
#' @param x A `GRaster`.
#' 
#' @returns A numeric `matrix`.
#' 
#' @example man/examples/ex_GRaster.r
#' 
#' @aliases cor
#' @rdname cor
#' @exportMethod cor
methods::setMethod(
	f = "cor",
	signature = c(x = "GRaster"),
	function(x) .corCovar(x, "cor")
)

#' @aliases cov
#' @rdname cor
#' @exportMethod cov
methods::setMethod(
	f = "cov",
	signature = c(x = "GRaster"),
	function(x) .corCovar(x, "cov")
)

# x A GRaster
# stat "cov" or "cor"
.corCovar <- function(x, stat) {

	stat <- omnibus::pmatchSafe(stat, c("cor", "cov"))

	if (nlyr(x) == 1L) stop("The raster must have >= 2 layers.")
	
	.locationRestore(x)
	.region(x)

	args <- list(
		cmd = "r.covar",
		map = paste(sources(x), collapse = ","),
		flags = c(.quiet()),
		intern = TRUE
	)

	if (stat == "cor") args$flags <- c(args$flags, "r")
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