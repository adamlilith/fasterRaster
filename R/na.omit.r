#' Force cells with NA in any layer of a GRaster stack to NA
#'
#' @description 'na.omit()' returns a `GRaster` stack with the same number of layers as the input, but with all cells that have `NA` in any layer set to `NA` in all layers. This is useful for masking out areas where data are missing in any of the rasters.
#'
#' @param object A "stack" of `GRaster`s.
#' @param verbose Logical: If `TRUE`, display progress.
#'
#' @returns A `GRaster`.
#'
#' @seealso [terra::na.omit()], [stats::na.omit()], **GRASS** tool `r.mapcalc` (see `grassHelp("r.mapcalc")`)
#'
#' @example man/examples/ex_na.omit.r
#'
#' @aliases na.omit
#' @rdname na.omit
#' @exportMethod na.omit
methods::setMethod(
	f = "na.omit",
	signature = c(object = "GRaster"),
	definition = function(object, verbose = FALSE) {

	.locationRestore(object)
	.region(object)

	nLayers <- nlyr(object)
	if (verbose | faster("verbose")) {
		nSteps <- 2 * nLayers
		pb <- utils::txtProgressBar(min = 0, max = nSteps, initial = 0, style = 3, width = 30)
		steps <- nLayers
		utils::setTxtProgressBar(pb, steps)
	}

	maskAcrossLayers <- maskNA(object, value = 1, invert = FALSE, retain = FALSE, byLayer = FALSE)
	on.exit(.rm(maskAcrossLayers, type = "raster", warn = FALSE, verify = FALSE), add = TRUE)

	srcs <- .makeSourceName("na_omit", "raster", n = nLayers)

	for (i in 1L:nLayers) {

		if (verbose | faster("verbose")) {
			steps <- steps + 1
			utils::setTxtProgressBar(pb, steps)
		}

		ex <- paste0(srcs[i], " = if(", sources(maskAcrossLayers), "==1, ", sources(object)[i], ", null())")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

	}

	if (verbose | faster("verbose")) close(pb)
	makeGRaster(srcs, names = names(object), levels = levels(object), ac = activeCats(object))

	} # EOF
)
