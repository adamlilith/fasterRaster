#' Mask all non-NA cells or all NA cells
#'
#' @description This function work in two ways:
#' * If `byLayer = TRUE` (default), it will convert all non-`NA` cells in a `GRaster` to a single user-defined value, leaving `NA` cells as `NA`. Alternatively, it can convert `NA` cells to a user-defined value, and all non-`NA` cells to `NA.` Regardless, if the input is a "stack" of `GRaster`s, it will return a stack with the same number of raster layers.
#' * If `byLayer = FALSE` and `x` is a "stack" of `GRaster`s,, it will return a single `GRaster` layer. This layer will have a user-defined value in all cells that had no `NA`s across all rasters, and `NA`s in cells where at least one raster had an `NA`. This is useful for masking out areas where data are missing in any of the rasters. If `invert = TRUE`, it will return a single `GRaster` layer with a user-defined value in all cells that had at least one `NA` across all rasters, and `NA`s in cells where all rasters had non-`NA` values.
#'
#' @param x A `GRaster`.
#' @param value Numeric: Value to which to assign to masked cells. The default is 1.
#' @param invert Logical: If `FALSE` (default), convert non-`NA` cells to `value`, and leave `NA` cells as-is. If `TRUE`, convert all `NA` cells to `value`, and non-`NA` cells to `NA`.
#' @param retain Logical: If `invert` is `TRUE` and `retain` is `FALSE` (default), non-`NA` cells will retain their value. This argument is ignored if `invert` is `FALSE`. Ignored if `byLayer = FALSE`.
#' @param byLayer Logical: If `TRUE` (default), implement the masking layer-by-layer. If `x` is a stack of raster, it will return a stack with the same number of layers. If `FALSE`, return a single raster layer with `NA` in all cells that had at least one `NA` across all layers, and `value` in all cells that had no `NA`s across all layers. If `invert = TRUE`, it will return a single raster layer with `value` in all cells that had at least one `NA` across all layers, and `NA` in all cells that had no `NA`s across all layers.
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
	function(x, value = 1, invert = FALSE, retain = FALSE, byLayer = TRUE) {
	
	if (is.na(value)) {
		value <- "null()"
		if (invert) stop("If `value` is NA, then `invert` must be FALSE.")
	}
	
	.locationRestore(x)
	.region(x)
	
	nLayers <- nlyr(x)
	if (byLayer) {

		srcs <- .makeSourceName("maskNA_r_mapcalc", "raster", n = nLayers)
		nms <- names(x)

		for (i in seq_len(nLayers)) {

			if (!invert) {
				ex <- paste0(srcs[i], " = if(!isnull(", sources(x)[i], "), ", value, ", null())")
			} else {
				ret <- if (retain) { sources(x)[i] } else { "null()"}
				ex <- paste0(srcs[i], " = if(isnull(", sources(x)[i], "), ", value, ", ", ret, ")")
			}
			rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

		}

	} else if (!byLayer) {

		srcs <- .makeSourceName("maskNA_r_mapcalc", "raster")
		nms <- "mask"

		if (!invert) {
			ex <- paste0(srcs, " = if(", paste("!isnull(", sources(x), ")", collapse = " & ", sep = ""), ", ", value, ", null())")
		} else {
			ex <- paste0(srcs, " = if(", paste("isnull(", sources(x), ")", collapse = " & ", sep = ""), ", ", value, ", null())")
		}
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

	}
	makeGRaster(srcs, names = nms)
	
	} # EOF
)
