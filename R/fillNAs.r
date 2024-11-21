#' Fill NA cells in a raster using interpolation
#'
#' @description This function uses splines to fill `NA` cells in a raster based on the values of nearby cells. Depending on the method used, not all `NA` cells can be filled.
#'
#' @param x A `GRaster`.
#'
#' @param lambda Either `NULL` (default), or a numeric value > 0: If `NULL`, then the function will use leave-one-out crossvalidation to find the optimal value.
#'
#' @param method Character: Type of spline, either "`bilinear`" (default), "`bicubic`", or "`RST`" (regularized splines with tension). Partial matching is used and case is ignored.
#'
#' **Note**: The RST method will often display warnings, but these can be ignored.
#'
#' @param min,max Numeric: Lowest and highest values allowed in the interpolated values. Values outside these bounds will be truncated to the minimum/maximum value(s) allowed. The default imposes no constraints. For multi-layered rasters, you can supply a single value for `min` and/or `max`, or multiple values (one per layer). Values will be recycled if there are fewer than one or them per layer in the raster.
#'
#' @param cells Integer or numeric integer: Number of cells away from the non-`NA` cells to fill. For example, if `cells = 2`, then only cells within a 2-cell buffer of non-`NA` cells will be filled. The default is `Inf` (fill all possible cells--some methods may not be able to do this, depending on the configuration of the raster).
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_fillNAs.r
#'
#' @seealso [terra::interpNear()], **GRASS** module `r.fillnulls` (see `grassHelp("r.fillnulls")`)
#'
#' @aliases fillNAs
#' @rdname fillNAs
#' @exportMethod fillNAs
methods::setMethod(
	f = "fillNAs",
	signature = c(x = "GRaster"),
	function(x, lambda = NULL, method = "bilinear", min = -Inf, max = Inf, cells = Inf) {
	
	method <- tolower(method)
	method <- omnibus::pmatchSafe(method, c("bilinear", "bicubic", "rst"), n = 1L)

	.locationRestore(x)
	.region(x)

	nLayers <- nlyr(x)

	if (length(min) < nLayers) min <- rep(min, length.out = nLayers)
	if (length(max) < nLayers) max <- rep(max, length.out = nLayers)

	mm <- minmax(x)
	if (any(min > mm["min", ])) warning("The `min` value is greater than the actual minimum value in at least one raster layer.\n  Observed values will be truncated.")

	if (any(max > mm["max", ])) warning("The `max` value is less than the actual maximum value in at least one raster layer.\n  Observed values will be truncated.")

	srcs <- .makeSourceName("r_fillnulls", "raster", nLayers)

	for (i in seq_len(nLayers)) {
	
		# mask
		if (!is.infinite(cells)) {
		
			maskSrc <- .makeSourceName("r_grow", "raster")

			rgrass::execGRASS(
				cmd = "r.grow",
				input = sources(x)[i],
				output = maskSrc,
				radius = cells + 0.01,
				metric = "euclidean",
				old = 1,
				new = 1,
				flags = c(.quiet(), "overwrite")
			)

			rgrass::execGRASS(
				cmd = "r.mask",
				raster = maskSrc,
				flags = c(.quiet(), "overwrite")
			)
		
		}

		args <- list(
			cmd = "r.fillnulls",
			input = sources(x)[i],
			output = srcs[i],
			method = method,
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite"),
			Sys_show.output.on.console = FALSE
		)
	
		do.call(rgrass::execGRASS, args = args)

		# unmask
		if (!is.infinite(cells)) .removeMask()

		# constrain interpolation
		if (!is.infinite(min[i]) | !is.infinite(max[i])) {

			srcIn <- srcs[i]
			srcs[i] <- .makeSourceName("r_mapcalc", "raster")

			if (!is.infinite(min[i]) & is.infinite(max[i])) {
		
				ex <- paste0(srcs[i], " = if(", srcIn, " < ", min[i], ", ", min[i], ", ", srcIn, ")")

			} else if (is.infinite(min[i]) & !is.infinite(max[i])) {

				ex <- paste0(srcs[i], " = if(", srcIn, " > ", max[i], ", ", max[i], ", ", srcIn, ")")

			} else if (!is.infinite(min[i]) & !is.infinite(max[i])) {
			
				ex <- paste0(srcs[i], " = if(", srcIn, " < ", min[i], ", ", min[i], ", if (", srcIn, " > ", max[i], ", ", max[i], ", ", srcIn, "))")

			}

			rgrass::execGRASS(
				cmd = "r.mapcalc",
				expression = ex,
				flags = c(.quiet(), "overwrite")
			)

			.rm(srcIn, type = "raster", warn = FALSE)
		
		}

	}
	.makeGRaster(srcs, names(x))

	} # EOF
)