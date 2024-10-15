#' Regression intercept, slope, r2, and t-value across each set of cells
#'
#' @description This function performs a regression on each set of cells in a multi-layered `GRaster`. The output is a `GRaster` with the intercept, slope, r^2 value, and Student's t value. The regression formula is as `y ~ 1 + x`, where `x` is the layer number of each layer (e.g., 1 for the first or top layer in the input `GRaster`, 2 for the second or second-to-top layer, etc.). Note that this is restricted version of the functionality in [terra::regress()].
#'
#' @param y A multi-layer `GRaster`.
#'
#' @param x Ignored.
#'
#' @param na.rm Logical: If `FALSE`, any series of cells with `NA` in at least one cell results in an `NA` in the output.
#'
#' @returns A multi-layer `GRaster`.
#'
#' @seealso [terra::regress()]
#'
#' @example man/examples/ex_GRaster_arithmetic_across_layers.r
#'
#' @aliases regress
#' @rdname regress
#' @exportMethod regress
methods::setMethod(
	f = "regress",
	signature = c(y = "GRaster", x = "missing"),
	function(y, x, na.rm = FALSE) {
	
	if (nlyr(y) < 2) stop("The `GRaster` must have >1 layer.")

	.locationRestore(y)
	.region(y)

	fx <- "offset"
	fxName <- "intercept"
	intercept <- .genericMultiLayer(fx = fx, fxName = fxName, x = y, na.rm = na.rm)

	fx <- "slope"
	fxName <- "slope"
	slope <- .genericMultiLayer(fx = fx, fxName = fxName, x = y, na.rm = na.rm)

	fx <- "detcoeff"
	fxName <- "r2"
	r2 <- .genericMultiLayer(fx = fx, fxName = fxName, x = y, na.rm = na.rm)
		
	fx <- "tvalue"
	fxName <- "tvalue"
	tvalue <- .genericMultiLayer(fx = fx, fxName = fxName, x = y, na.rm = na.rm)
		
	c(intercept, slope, r2, tvalue)

	} # EOF
)
