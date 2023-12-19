#' Area of GRaster cells
#'
#' @description `cellArea()` returns a raster will cell values equal to their area. To get the area of all cells of a raster, see [expanse()].
#'
#' @param x A `GRaster`.
#'
#' @param mask Logical: If `TRUE`, then cells that are `NA` in `x` are also `NA` in the output.
#'
#' @param lyrs Logical:
#' * If `lyrs` is `FALSE` (default), then the output has a single layer. In this case, if `mask` is `TRUE`, then cells that are `NA` in the *first* layer are also `NA` in the output.
#' * If `lyrs` is `TRUE`, then the output has the same number of layers as `x` if `mask` is also `TRUE`. In this case, cells that area `NA` in the input will also be `NA` in the output in the respective layer.
#'
#' @param unit Character: Units of area. Partial matching is used, and case is ignored. Can be any of:
#' * `"meters2"` (default), `"metres2"`, or `"m2"`
#' * `"km2"` or `"kilometers2"`
#' * `"ha"` or `"hectares"`
#' * `"acres"`
#' * `"mi2"` or `"miles2"`
#' * `"ft2"` or `"feet2"`
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_cellSize.r
#'
#' @seealso [terra::cellSize()], [expanse()], [zonalGeog()], [omnibus::convertUnits()]
#'
#' @aliases cellSize
#' @rdname cellSize
#' @exportMethod cellSize
methods::setMethod(
	f = "cellSize",
	signature = c(x = "GRaster"),
	function(x, mask = FALSE, lyrs = FALSE, unit = "meters2") {
	
	units <- c("m2", "meters2", "metres2", "km2", "kilometers2", "ha", "hectares", "ac", "acres", "mi2", "miles2", "ft2", "feet2")
	unit <- omnibus::pmatchSafe(unit, units, useFirst = TRUE, nmax = 1L)

	if (unit == "metres") unit <- "meters"
	unit <- omnibus::expandUnits(unit)

	f <- omnibus::convertUnits(from = "meters2", to = unit)
	f <- format(f, scientific = FALSE)

	.region(x)

	if (lyrs & mask) {
		nLayers <- nlyr(x)
	} else {
		nLayers <- 1L
	}

	### mask
	if (mask) {
		
		# remove mask on exit
		on.exit(.removeMask(), add = TRUE)

		# make mask from first layer
		rgrass::execGRASS(
			cmd = "r.mask",
			raster = sources(x)[1L],
			flags = c(.quiet(), "overwrite")
		)

	}

	srcs <- .makeSourceName("r_mapcalc", "raster", n = nLayers)

	for (i in seq_len(nLayers)) {

		# mask
		if (i > 1L & mask & lyrs) {
		
			rgrass::execGRASS(
				cmd = "r.mask",
				raster = sources(x)[i],
				flags = c(.quiet(), "overwrite")
			)
		
		}

		ex <- if (mask) {
			paste0(srcs[i], " = area() * MASK * ", f)
		} else {
			paste0(srcs[i], " = area() * ", f)
		}

		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)

	} # next raster layer

	.makeGRaster(srcs, paste0("area_", unit, "_", seq_len(nLayers)))
		
	} # EOF
)
