#' Multivariate environmental similarity surface (MESS)
#'
#' @description The multivariate environmental similarity surface (MESS) indicates the degree to which a set of "projection" environmental conditions fall inside or outside a set of "reference" conditions. Values of 1 indicate a location falls at the exact median of all variables. Values of 0 indicate that the location has at least one environmental covariate that is at the upper or lower end of the range of reference conditions, and values <1 indicate that at least one variable falls above or below the reference conditions. MESS can be used, for example, to indicate the degree to which a model constructed in one time period and/or location must extrapolate when projected to another time period and/or location.
#'
#' @param ref A `data.frame`, `data.table`, a points `GVector`, or "stack" of `GRasters`: This represents the set of "reference" environmental conditions:
#' * `data.frame` or `data.table`: There must be one column per layer in `proj`, and the columns must have the same names as the layers in `proj`.
#' * `GRaster` with one or more layers: Must have the same [names()] as the `GRaster`s in `proj`. Values are assumed to be continuous (not categorical/factors).
#'
#' @param proj A `GRaster` or missing. If a `GRaster`, it must have the same layers as can have with one or more layers as `ref`. Values are assumed to be continuous (not categorical/factors). If missing, then `ref` is used, in which case the output represents the relative difference of each cell from the overall layer median.
#'
#' @returns A `GRaster` "stack". There will be one layer per layer in `ref`, indicating the MESS score for that variable. There will also be a layer named "MESS" which represents the MESS value across all variables (the minimum value of each of the individual MESS rasters). A final layer represents the layer which is most different (has the lowest MESS value).
#'
#' @references Elith, J, Kearney, M, and Phillips, S. 2010. The art of modelling range-shifting species. *Methods in Ecology and Evolution* 1:330-342. \doi{10.1111/j.2041-210X.2010.00036.x} (see especially the Supplement)
#' 
#' @example man/examples/ex_multivarEnvSim.r
#'
#' @aliases multivarEnvSim
#' @rdname multivarEnvSim
#' @exportMethod multivarEnvSim
methods::setMethod(
	f = "multivarEnvSim",
	signature = c(ref = "GRaster", proj = "GRaster"),
	definition = function(
		ref,
		proj
	) {

	lyrs <- nlyr(ref) == nlyr(proj)
	names1 <- names(ref) %in% names(proj)
	names2 <- names(proj) %in% names(ref)

	if (!all(lyrs, names1, names2)) stop("The `ref` and `proj` set of GRasters must have the same number of layers with the same names.")

	ref <- ref[[names(proj)]]

	medians <- global(ref, "median")
	mm <- minmax(ref)

	.locationRestore(proj)
	.region(proj)

	### MESS values for each variable
	nLayers <- nlyr(proj)
	srcs <- .makeSourceName("mess_r_mapcalc", "raster", nLayers)
	for (i in seq_len(nLayers)) {
	
		layer <- names(proj)[i]
		x <- ref[[i]]
		y <- proj[[names(x)]]
		ySrc <- sources(y)

		thisMedian <- medians[layer, "median"]
		thisMin <- mm["min", layer]
		thisMax <- mm["max", layer]

		medianToMax <- thisMax - thisMedian
		medianToMin <- thisMedian - thisMin

		ex <- paste0(srcs[i], " = if(", ySrc," >= ", thisMedian, ", 1 - ((", ySrc, " - ", thisMedian, ") / ", medianToMax, "), 1 - ((", thisMedian, " - ", ySrc, ") / ", medianToMin, "))")

		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
	
	}

	### MESS values across all variables
	srcOverall <- .makeSourceName("mess_r_mapcalc", "raster")
	ex <- paste0(srcOverall, " = min(", paste(srcs, collapse = ","),")")
	rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

	### most different variable
	srcMostDiffFrom0 <- .makeSourceName("mess_r_series", "raster")
	rgrass::execGRASS(
		cmd = "r.series",
		input = paste(srcs, collapse=","),
		output = srcMostDiffFrom0,
		method = "min_raster",
		nprocs = faster("cores"),
		memory = faster("memory"),
		flags = c(.quiet(), "overwrite")
	)

	# add 1 bc "r.series::min_raster" returns 0 for the first raster
	srcMostDiffFrom1 <- .makeSourceName("mess_r_mapcalc", "raster")
	ex <- paste0(srcMostDiffFrom1, " = int(", srcMostDiffFrom0, " + 1)")
	rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

	mostDiffLevels <- data.table::data.table(value = 1L:nLayers, layer = names(proj))
	levs <- vector(mode = "list", length = nLayers + 2)
	levs[[length(levs)]] <- mostDiffLevels

	srcsAll <- c(srcs, srcOverall, srcMostDiffFrom1)
	outNames <- c(names(proj), "MESS", "mostDifferent")
	.makeGRaster(srcsAll, names = outNames)

	} # EOF
)

#' @aliases multivarEnvSim
#' @rdname multivarEnvSim
#' @exportMethod multivarEnvSim
methods::setMethod(
	f = "multivarEnvSim",
	signature = c(ref = "GRaster", proj = "missing"),
	definition = function(ref, proj) multivarEnvSim(ref = ref, proj = ref)
)


