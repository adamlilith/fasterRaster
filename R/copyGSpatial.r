#' Make a copy of an object in GRASS
#'
#' Create a copy of a `GRaster` or `GVector` in **GRASS**.  This function is used internally and is of little use to most users.  This only creates a copy of the object in the **GRASS** session--to make a `GRaster` or `GVector`, [.makeGRaster()] or [.makeGVector()] need to be called after making the copy. Note that if the object is multi-layered, then a copy is made of each layer.
#'
#' @param x `GRaster`, `GVector`, or character: The object or the [sources()] name(s) of the object(s) to be copied. Can take multi-layered objects or multiple `sources`.
#'
#' @param type Character or `NULL` (default): Either "raster" or "vector". If a character, there must be one per value in `x`. If `NULL`, will attempt to auto-detect (takes longer).
#'
#' @param topo `NULL` (default) or `"2D"` or `"3D"`.
#' 
#' @param reshapeRegion Logical: If `TRUE`, reshape the region to match `x` (`GRaster`s only).
#'
#' @returns Character vector representing the new `sources` of each object, plus makes a copy of the given object(s) in **GRASS**.
#'
#' @aliases .copyGSpatial
#' @noRd
methods::setMethod(
	f = ".copyGSpatial",
	signature = c(x = "GRaster"),
	function(x, reshapeRegion = TRUE) .copyGRaster(x, topo = topology(x), reshapeRegion = reshapeRegion)
)

#' @aliases .copyGSpatial
#' @noRd
methods::setMethod(
	f = ".copyGSpatial",
	signature = c(x = "GVector"),
	function(x) .copyGVector(x)
)

#' @aliases .copyGSpatial
#' @noRd
methods::setMethod(
	f = ".copyGSpatial",
	signature = c(x = "character"),
	function(x, type = NULL, topo = NULL, reshapeRegion = TRUE) {
	
	if (is.null(type)) {

		srcs <- .ls()
		types <- names(srcs)
		type <- types[match(x, srcs)]

	} else {

		type <- omnibus::pmatchSafe(type, c("raster", "vector", "raster3d", "vector3d"))
	
	}

	n <- length(x)
	srcs <- rep(NA_character_, n)

	for (i in seq_len(n)) {
		
		if (type[i] %in% c("raster", "raster3d")) {
			srcs[i] <- .copyGRaster(x[i], topo = topo, reshapeRegion = reshapeRegion)
		} else if (type[i] %in% c("vector", "vector3d")) {
			srcs[i] <- .copyGVector(x[i])
		}
	
	}
	srcs
		
	} # EOF

)

#' @param x A `GRaster` or [sources()] name of one.
#' @param topo "2D" or "3D"
#' @param reshapeRegion Logical.
#'
#' @returns [sources()] names of copied rasters.
#'
#' @noRd
.copyGRaster <- function(x, topo = "2D", reshapeRegion = TRUE) {

	# NB This function could use `g.copy`, but in some cases it does not have the desired effect. For example, when a MASK raster is present, it correctly copies cells that are not masked, but when the MASK is removed, the masked cells re-appear. Similarly, it ignores the region when copying.

	if (inherits(x, "GRaster")) {

		.locationRestore(x)
		if (reshapeRegion) .region(x)
		srcs <- sources(x)

	} else {

		srcs <- x
		if (reshapeRegion) {

			args <- list(
				cmd = "g.region",
				flags = .quiet()
			)

			if (is.null(topo)) {

				topo <- "2D" # guessing!
				
				if (!is.null(.quiet)) warning("Assuming raster is 2D.")

				args$raster <- srcs[1L]

			} else if (topo == "2D") {
				args$raster <- srcs[1L]
			} else if (topo == "3D") {
				args$raster_3d <- srcs[1L]
			}

			do.call(rgrass::execGRASS, args = args)

		}

	}

	nLayers <- length(srcs)

	out <- .makeSourceName("r_mapcalc", type = "raster", nLayers)

	for (i in seq_len(nLayers)) {

		ex <- paste0(out[i], " = ", srcs[i])

		rgrass::execGRASS(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c(.quiet(), "overwrite")
		)

	}

	out
	
}

#' @param x A `GVector` or the [sources()] name of one.
#' @noRd
.copyGVector <- function(x) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
		srcs <- sources(x)
	} else {
		srcs <- x
	}

	n <- length(srcs)

	out <- .makeSourceName("g_copy", type = "vector", n = n)

	for (i in seq_len(n)) {
		
		fromTo <- paste0(srcs[i], ",", out[i])
		rgrass::execGRASS(
			cmd = "g.copy",
			vector = fromTo,
			flags = c(.quiet(), "overwrite")
		)

	}

	out

}
