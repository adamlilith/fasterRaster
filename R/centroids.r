#' Centroid(s) of a vector
#'
#' @description This function locates the centroid of each geometry of a `GVector`.
#' 
#' **To use this function**, you must a) have correctly specified the `addonsDir` option using [faster()], and b) installed the **GRASS** addon `v.centerpoint`. See [addons()] and `vignette("addons", package = "fasterRaster")`.
#'
#' @param x A `GVector`.
#' @param method Character or `NULL` (default): Method used for calculating centroids. The method of calculation depends on whether the input is a `points`, `lines`, or `polygons` `GVector`. If the value is `NULL`, then the default method will be chosen, depending on the geometry type of the `GVector`:
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
#' @param fail Logical: If `TRUE` (default), and the addons folder is not correctly specified, the exit the function with an error. If `FALSE`, then `NULL` will be returned with a warning.
#'
#' @returns A points `GVector`.
#'
#' @example man/examples/ex_centroids.r
#' 
#' @seealso [terra::centroids()]; **GRASS** addon module `v.centerpoint`.
#'
#' @aliases centroids
#' @rdname centroids
#' @exportMethod centroids
methods::setMethod(
	f = "centroids",
	signature = c(x = "GVector"),
	function(x, method = NULL, fail = TRUE) {
	
	ok <- addons("v.centerpoint", verbose = TRUE, fail = fail)
	if (!ok) return(NULL)

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
