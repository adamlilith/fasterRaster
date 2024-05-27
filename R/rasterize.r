#' Convert a GVector to a GRaster
#'
#' @description The `rasterize()` function converts a `GVector` into a `GRaster`.
#'
#' @param x A `GVector`.
#'
#' @param y A `GRaster`: The new raster will have the same extent and resolution as this raster.
#'
#' @param background Numeric or `NA` (default): Value to put in cells that are not covered by the `GVector`.
#'
#' @param byGeom,collapse Logical:
#' * If `byGeom` is `TRUE` and `collapse` is `TRUE`, the output will have just one raster layer, but with values that correspond to each geometry (i.e., each geometry will get a different value). If geometries overlap, then category value of the geometry on "top" will be assigned to the relevant cells. 
#' * If `byGeom` is `TRUE`, and `collapse` is `FALSE`, then create a separate raster layer for each geometry in `x`.
#' * If `byGeom` is `FALSE`, then create a single raster layer with the same value for all geometries. Argument `collapse` is ignored.
#'
#' @returns A `GRaster`.
#'
#' @seealso [terra::rasterize()], module [`v.to.rast`](https://grass.osgeo.org/grass84/manuals/v.to.rast.html) in **GRASS**
#'
#' @example man/examples/ex_rasterize.r
#'
#' @aliases rasterize
#' @rdname rasterize
#' @exportMethod rasterize
methods::setMethod(
	f = "rasterize",
	signature = c(x = "GVector", y = "GRaster"),
	function(
		x,
		y,
		background = NA,
		byGeom = FALSE,
		collapse = TRUE
	) {
	
	compareGeom(x, y)
	.locationRestore(x)
	.region(y)

	gtype <- geomtype(x, grass = TRUE)
	src <- .rasterize(x = x, y = y, background = background, byGeom = byGeom, collapse = collapse)
	.makeGRaster(src)

	} # EOF
)

#' @param x [sources()] name of a `GVector`.
#' @param y [sources()] name of a `GRaster`
#' @param background Numeric or `NA`.
#' @param byGeom Logical.
#' @param collapse Logical.
#'
#' @noRd
.rasterize <- function(x, y, background, byGeom, collapse) {

	### create different raster layer for each geometry
	if (byGeom) {

		# if by geometry but burned to the same raster
		if (collapse) {

			gtype <- geomtype(x, grass = TRUE)
			src <- .makeSourceName("rasterize_v_to_rast", "raster")
			args <- list(
				cmd = "v.to.rast",
				input = sources(x),
				output = src,
				use = "cat",
				type = gtype,
				memory = faster("memory"),
				flags = c(.quiet(), "overwrite")
			)

			if (gtype == "line") args$flags <- c(args$flags, "d")

			do.call(rgrass::execGRASS, args = args)
			out <- .makeGRaster(src, "rasterize")

		} else {
			
			ng <- ngeom(x)
			for (i in seq_len(ng)) {
			
				xx <- x[i]

				thisOut <- rasterize(
					x = xx, y = y,
					background = background,
					byGeom = FALSE
				)

				if (i == 1L) {
					out <- thisOut
				} else {
					out <- c(out, thisOut)
				}

			} # next geometry


		} # if one raster per geometry, not collapsed

	### all geometries at once
	} else {

		gtype <- geomtype(x, grass = TRUE)
		src <- .makeSourceName("rasterize_v_to_rast", "raster")
		args <- list(
			cmd = "v.to.rast",
			input = sources(x),
			output = src,
			use = "val",
			value = 1,
			type = gtype,
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite")
		)

		if (gtype == "line") args$flags <- c(args$flags, "d")

		do.call(rgrass::execGRASS, args = args)

		if (!is.na(background)) {

			srcIn <- src
			src <- .makeSourceName("rasterize_r_mapcalc", "vector")
			ex <- paste0(src, " = if(isnull(", srcIn, "), ", background, ", ", srcIn, ")")

			rgrass::execGRASS(
				cmd = "r.mapcalc",
				expression  = ex,
				flags = c(.quiet(), "overwrite")
			)

		}

	}
	src

}
