#' Centroid(s) of a vector or clumps in a raster
#'
#' @description This function locates the centroid of each geometry of a `GVector`, or the centroid of each "clump" of same-valued cells in an integer/categorical raster (for information on types of `GRaster`s, see `vignette("GRasters", package = "fasterRaster")`).
#' 
#' To use this function with a `GVector`, you need the **GRASS** `v.centerpoint` addon. To use the function with a `GRaster`, you need the addon `r.centroids`. In either case, the function will try to install the respective addon (i.e., you need to have an internet connection). Once installed, a tool will not need to be installed again.
#'
#' @param x A `GVector` or `GRaster`.
#'
#' @param method `GVector`s: Character or `NULL` (default): Method used for calculating centroids. The method of calculation depends on whether the input is a `points`, `lines`, or `polygons` `GVector`. If the value is `NULL`, then the default method will be chosen, depending on the geometry type of the `GVector`:
#' * `points`:
#'    * `"mean"` (default for `points`): Mean of coordinates.
#'    * `"median"`: Geometric median; more robust to outliers.
#'    * `"pmedian"`: Point in `x` closest to the geometric median.
#' * `lines`:
#'    * `"mid"` (default for `lines`): Mid-point on each line; will fall exactly on the line.
#'    * `"mean"`: Center of gravity of all line segments; may not fall on the line.
#'    * `"median`: Geometric median; may not fall on the line.
#' * `polygons`:
#'    * `"mean"` (default for `polygons`): Center of gravity (area), calculated using area triangulation.
#'    * `"median"`: Geometric mean; may not fall inside the polygon.
#'    * `"bmedian"`: Geometric mean; minimum distance to boundaries; may not fall inside the polygon.
#' 
#' Partial matching is used and case is ignored.
#'
#' @returns If the input is a `GVector`, the output will be a "points" `GVector`. If the input is a `GRaster`, the output will be a "points" `GVector` with a table with statistics on each clump. If the input is a `GRaster` with more than one layer, the output will be a `list` of `GVector`s, with one `GVector` per layer.
#'
#' @example man/examples/ex_centroids.r
#' 
#' @seealso [terra::centroids()]; **GRASS** addon tools `v.centerpoint` and `r.centroids`.
#'
#' @aliases centroids
#' @rdname centroids
#' @exportMethod centroids
methods::setMethod(
	f = "centroids",
	signature = c(x = "GVector"),
	function(x, method = NULL) {
	
	.addons("v.centerpoint")

	gtype <- geomtype(x)
	
	if (is.null(method)) {
	
		if (gtype == "points") {
			method <- "mean"
		} else if (gtype == "lines") {
			method <- "mid"
		} else if (gtype == "polygons") {
			method <- "mean"
		}
	
	}

	if (gtype == "points") {
		methods <- c("mean", "median", "pmedian")
	} else if (gtype == "lines") {
		methods <- c("mid", "mean", "median")
	} else if (gtype == "polygons") {
		methods <- c("mean", "median", "bmedian")
	}

	method <- omnibus::pmatchSafe(method, methods, nmax = 1L)

	src <- .makeSourceName("centroids", "vector")
	args <- list(
		cmd = "v.centerpoint",
		input = sources(x),
		output = src,
		flags = c(.quiet(), "overwrite")
	)
	
	if (gtype == "points") {
		args$pcenter <- method
	} else if (gtype == "lines") {
		args$lcenter <- method
	} else if (gtype == "polygons") {
		args$acenter <- method
	}

	do.call(rgrass::execGRASS, args = args)

	if (gtype == "points") {
		table <- NULL
	} else {
		table <- as.data.table(x)
	}

	.makeGVector(src, table = table)
	
	} # EOF
)

#' @aliases centroids
#' @rdname centroids
#' @exportMethod centroids
methods::setMethod(
	f = "centroids",
	signature = c(x = "GRaster"),
	function(x) {
	
	.addons("r.centroids")

	.locationRestore(x)
	.region(x)

	nLayers <- nlyr(x)
	out <- list()
	srcs <- .makeSourceName("r_centroids", "vector", n = nLayers)
	for (i in 1:nLayers) {

		rawTable <- rgrass::execGRASS(
			cmd = "r.centroids",
			input = sources(x)[[i]],
			output = srcs[i],
			flags = c(.quiet(), "overwrite"),
			intern = TRUE
		)

		# parse table
		rawTable <- rawTable[7:(length(rawTable) - 2)]
		lines <- strsplit(rawTable, split = " ")
		table <- data.table::data.table()
		for (j in seq_along(rawTable)) {
		
			line <- lines[[j]]
			line <- line[line != ""]
			line <- as.numeric(line)
			table <- rbind(
				table,
				data.table::data.table(
					clump = line[1L],
					x = line[5L],
					y = line[7L],
					numberOfCells = line[4L],
					meanCellValue = line[2L],
					clumpSum = line[3L]
				)
			)
		
		}

		out[[i]] <- .makeGVector(srcs[i], table = table)

	}

	if (nLayers == 1) out <- out[[1]]
	out
	
	} # EOF
)
