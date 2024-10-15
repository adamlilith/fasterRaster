#' Identify terrain feature types
#'
#' @description Geomorphons are idealized terrain types calculated from an elevator raster based on a moving window of a given size. The window is a torus (which can have an inner radius of 0, so can also be a circle), which allows it to identify geomorphons of a given size while ignoring ones larger or smaller. There are 10 basic geomorphons. Consult the the manual for **GRASS** module `r.geomorphon` using `grassHelp("r.geomorphon")` for more details and diagrams of each type of geomorphon. Geomorphon types include:
#' 1. Flat areas: Focal area has approximately the same elevation as surrounding areas
#' 2. Pits: An area is lower than all other surrounding areas
#' 3. Valley: Focal area has elevation similar to two opposing side of the window but lower than the other two opposing sides
#' 4. Footslope: Focal region is at the "bottom" of a slope
#' 5. Hollow: A small valley/indention in the crest of a hill
#' 6. Slope: Cells in the window form an approximately uniform slope
#' 7. Spur: An extrusion at the foot of a hill (i.e.,, a small hill extending out from the foot of a slope)
#' 8. Shoulder: The crest of a slope
#' 9. Ridge: Opposite of a valley; focal area is higher than two opposing sides but approximately the same elevation as the other two opposing sides
#' 10. Peak: Focal area is higher than any other in the window
#'
#' @param x A single-layer `GRaster`, typically representing elevation.
#'
#' @param inner,outer Integer: Inner and outer radii of the torus used to identify geomorphons, in cells or meters (set by argument `unit`). The inner default value is 0 and the outer default value is 3. The `outer` radius sets the maximum size of a geomorphon that that can be identified, and `inner` sets the smallest size. If `unit` is `"meters"`, the value of `outer` must be larger than the smaller dimension of any cell in the east-west and north-south directions.
#'
#' @param unit Character: Units of `inner` and outer; can be either `"cells"` (default) or `"meters"`. Partial matching is used.
#'
#' @param flat Numeric value >= 0: Minimum difference (in degrees) between the focal area areas around it for a geomorphon to be considered as "flat". Larger cells (i.e., ~1 km resolution or larger) require smaller values (<<1) to correctly identify flat areas. Higher values result in more areas being classified as "flat" geomorphons. The default value is 1.
#'
#' @param flatDist Numeric: Distance (in meters) to correct for the effect of large distances on the diminished capacity to identify "flat" geomorphons. If the distance between the focal area and a surrounding area surpasses this distance, then the effective value of `flat` will be reduced
#'
#' @param mode Character: Method for implementing the zenith/line-of-site search. Partial matching is used:
#' * `"1"` (default): The "original" geomorphon mode (in **GRASS** module `r.geomorphon`, the "anglev1" method)
#' * `"2"`: Better handling of cases with equal zenith/nadir angles (the "anglev2" method)
#' * `"2d"`: As `"2"`, but takes into account zenith/nadir distance ("anglev2_distance" method)
#'
#' @returns A categorical `GRaster` where each geomorphon is a category (see `vignette("GRasters", package = "fasterRaster")`).
#'
#' @seealso **GRASS** module `r.geomorphon` (see `grassHelp("r.geomorphon")`)
#'
#' @example man/examples/ex_geomorphons.r
#'
#' @aliases geomorphons
#' @rdname geomorphons
#' @exportMethod geomorphons
methods::setMethod(
	f = "geomorphons",
	signature = c(x = "GRaster"),
	function(
		x,
		inner = 0,
		outer = 3,
		unit = "cells",
		flat = 1,
		flatDist = 0,
		mode = "1"
	) {
	
	unit <- omnibus::pmatchSafe(unit, c("cells", "meters"), nmax = 1L)
	comparison <- omnibus::pmatchSafe(mode, c("1", "2", "2d"), nmax = 1L)
	comparison <-if (comparison == "1") {
		"anglev1"
	} else if (comparison == "2") {
		"anglev2"
	} else if (comparison == "2d") {
		 "anglev2_distance"
	}

	if (nlyr(x) > 1L) stop("This function requires `x` to have just one layer.")

	.locationRestore(x)
	.region(x)

	src <- .makeSourceName("geomorphon_r_geomorphon", "raster")
	args <- list(
		cmd = "r.geomorphon",
		elevation = sources(x),
		forms = src,
		search = outer,
		skip = inner,
		flat = flat,
		dist = flatDist,
		comparison = comparison,
		flags = c(.quiet(), "overwrite")
	)

	if (unit == "meters") args$flags <- c(args$flags, "m")

	do.call(rgrass::execGRASS, args = args)

	levels <- data.table::data.table(
		value = 1L:10L,
		geomorphon = c("flat", "peak", "ridge", "shoulder", "spur", "slope", "hollow", "footslope", "valley", "pit")
	)

	.makeGRaster(src, levels = levels)

	} # EOF
)
