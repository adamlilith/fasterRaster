#' Identify clusters of points
#'
#' @description `clusterPoints()` partitions points in a "points" `GVector` into clusters.
#'
#' @param x A "points" `GVector`.
#'
#' @param method Character: Method used to identify clusters. Explanations of methods are provided in the help page for the **GRASS** module `v.cluster`, available using `grassHelp("v.cluster")`.
#' * `"DBSCAN"` (default): Density-Based Spatial Clustering of Applications with Noise. 
#' * `"DBSCAN2"`: A modification of DBSCAN.
#' * `"density"`: Cluster points by relative density.
#' * `"OPTICS"`: Ordering Points to Identify the Clustering Structure
#' * `"OPTICS2"`: A modification of OPTICS.
#'
#' Case is ignored, but partial matching is not used.
#'
#' @param minIn Integer, numeric integer, or `NULL` (default): Minimum number of points in a cluster. If `NULL`, then `minIn` is set to 3 for a 2-dimensional vector and 4 for a 3-dimensional vector.
#'
#' @param maxDist Numeric or `NULL` (default): Maximum distance between neighboring points in a cluster for DBSCAN, DBSCAN2, and OPTICS. If `NULL`, the maximum distance will be set to the 99th quantile of observed pairwise distances between points.
#'
#' @returns A vector of integers indicating the cluster to which each point belongs.
#'
#' @seealso **GRASS** manual page for module `v.cluster` (see `grassHelp("v.cluster")`)
#'
#' @aliases clusterPoints
#' @rdname clusterPoints
#' @exportMethod clusterPoints
methods::setMethod(
	f = "clusterPoints",
	signature = c(x = "GVector"),
	function(x, method = "DBSCAN", minIn = NULL, maxDist = NULL) {
	
	if (geomtype(x) != "points") stop("Only points GVectors can be clustered.")
	
	if (!is.null(maxDist)) {
		if (is.infinite(maxDist)) stop("Argument `maxDist` cannot be infinite.")
	}

	method <- tolower(method)

	.locationRestore(x)

	src <- .makeSourceName("v_cluster", "vector")
	args <- list(
		cmd = "v.cluster",
		input = sources(x),
		output = src,
		method = method,
		layer = "2",
		flags = c(.quiet(), "overwrite")
	)
	if (!is.null(minIn)) args$min <- minIn
	if (!is.null(maxDist)) args$distance <- maxDist
	do.call(rgrass::execGRASS, args = args)

	.vCats(src, layer = 2)

	} # EOF
)
