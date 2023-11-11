#' Kernel density estimator of points
#'
#' @description `kernel()` creates a raster using a kernel density estimator of the density of points in a "points" `GVector`.
#'
#' @param x A "points" `GVector`.
#'
#' @param y A `GRaster`: The extent and resolution of this raster will be used to create the density raster. Otherwise, values in this raster are ignored.
#'
#' @param kernel Character: Name of the kernel function to use. Possible values include:
#' * `"Epanechnikov"` (default)
#' * `"Gaussian"`
#' * `"uniform"`
#' * `"triangular"`
#' * `"quartic"`
#' * `"triweight"`
#' * `"cosine"`
#'
#' Partial matching is used, and case is ignored.
#'
#' @param optimize Logical: If `TRUE` (default), then attempt to find the optimal radius less than or equal to the `radius` value using the "Gaussian" kernel. If `FALSE`, use the `radius` value as-is.
#'
#' @param h Numeric or `NULL` (default): Smoothing bandwidth of kernel estimator.
#'
#' If this is `NULL`, the Epanechnikov kernel is used, and `optimize` is `TRUE`, then Silverman's rule-of-thumb is used to estimate the optimal value of `h`:
#' \deqn{h = 0.9 * min(\sigma_x / n^1/6, \sigma_y / n^1/6)}
#'
#' If the Gaussian kernel is used, and `optimize` is `TRUE`, then the **GRASS** `v.kernel` function will attempt to identify the optimal bandwidth, up to the value of `h`, if `h` is defined.
#'
#' Otherwise, if `h` is `NULL`, then the value will be arbitrarily set at 1/5th of the shorter of the distance of the x- and y-extent of the points.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_kernel.r
#'
#' @aliases kernel
#' @rdname kernel
#' @exportMethod kernel
methods::setMethod(
	f = "kernel",
	signature = c(x = "GVector"),
	function(
		x,
		y,
  		kernel = "Epanechnikov",
		optimize = TRUE,
		h = NULL
	) {
	
 	kernFx <- pmatchSafe(kernel, c("gaussian", "uniform", "triangular", "epanechnikov", "quartic", "triweight", "cosine"), nmax = 1L)

	.restore(y)
	compareGeom(x, y)
	region(y)

	# assign value to radius
	if (is.null(h) & optimize & kernFx == "epanechinokov") {
	
		coords <- crds(x)
		sds <- apply(coords, 2L, "sd")
		n_1_6 <- nrow(x) ^ (1 / 6)
		h <- 0.9 * min(sds[1L] / n_1_6, sds[2L] / n_1_6)

	} else if (is.null(h)) {

		extent <- ext(x, vector = TRUE)
		xext <- extent[2L] - extent[1L]
		yext <- extent[4L] - extent[3L]
	
		extent <- min(xext, yext)
		h <- 0.2 * extent

	}

	src <- .makeSourceName("v_kernel", "raster")
	args <- list(
		cmd = "v.kernel",
		input = sources(x),
		output = src,
		radius = h,
		kernel = kernFx,
		flags = c("quiet", "overwrite")
	)

	if (optimize & kernFx == "gaussian") args$flags <- c(args$flags, "o")
	do.call(rgrass::execGRASS, args = args)

	.makeGRaster(src, "kernelDensity")
	
	} # EOF
)
