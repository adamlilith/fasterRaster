#' Mask all non-NA cells or all NA cells
#'
#' @description This function converts all non-`NA` cells in a `GRaster` to a single user-defined value, leaving `NA` cells as `NA`. Alternatively, it can convert `NA` cells to a user-defined value, and all non-`NA` cells to `NA.`
#'
#' @param x A `GRaster`.
#' @param value Numeric: Value to which to assign to masked cells. The default is 1.
#' @param invert Logical: If `FALSE` (default), convert non-`NA` cells to `value`, and leave `NA` cells as-is. If `TRUE`, convert all `NA` cells to `value`, and non-`NA` cells to `NA`.
#' @param retain Logical: If `invert` is `TRUE` and `retain` is `FALSE` (default), non-`NA` cells will retain their value. This argument is ignored if `invert` is `FALSE`.
#'
#' @seealso [not.na()], [app()], [mask()]
#'
#' @example man/examples/ex_maskNA.r
#'
#' @returns A `GRaster`.
#'
#' @aliases maskNA
#' @rdname maskNA
#' @exportMethod maskNA
methods::setMethod(
	f = "maskNA",
	signature = c(x = "GRaster"),
	function(x, value = 1, invert = FALSE, retain = FALSE) {
	
	.locationRestore(x)
	.region(x)
	
	nLayers <- nlyr(x)
	srcs <- .makeSourceName("maskNA_r_mapcalc", "raster", n = nLayers)
	
	for (i in seq_len(nLayers)) {

		if (!invert) {
			ex <- paste0(srcs[i], " = if(!isnull(", sources(x)[i], "), ", value, ", null())")
		} else {
			ret <- if (retain) { sources(x)[i] } else { "null()"}
			ex <- paste0(srcs[i], " = if(isnull(", sources(x)[i], "), ", value, ", ", ret, ")")
		}
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

	}
	.makeGRaster(srcs, names = names(x))
	
	} # EOF
)
