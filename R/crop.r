#' Remove parts of a GRaster or GVector
#'
#' `crop()` removes parts of a `GRaster` or `GVector` that fall "outside" another raster or vector. You cannot make the `GRaster` or `GVector` larger than it already is (see [extend()]). Rasters may not be cropped to the exact extent, as the extent will be enlarged to encompass an integer number of cells. If you wish to remove certain cells of a raster, see [mask()].
#'
#' @param x A `GRaster` or `GVector` to be cropped.
#' @param y A `GRaster` or `GVector` to serve as a template for cropping.
#' @param ext Logical (when `x` and `y` are `GVector`s): If `TRUE`, use the extent of `y` to crop `x`. Otherwise:
#' * If `ext` is `TRUE, and `x` and `y` are "points" `GVector`s, use the convex hull around `y` to select points in `x`.
#' 
#' @details Known differences from [terra::crop()]:
#' * If `x` and `y` are "points" vectors, and `ext` is `TRUE`, **terra** removes points that fall on the extent boundary. **fasterRaster** does not remove points on the extent boundary.
#' * If `x` is a "points" vector and `y` is a "lines" vectors, and `ext` is `FALSE`, **terra** uses the extent of `y` to crop the points.  **fasterRaster** uses the minimum convex hull of the lines vector.
#'
#' @return A `GRaster` or `GVector`.
#' 
#' @seealso [terra::crop()], [sf::st_crop()]
#' 
#' @example man/examples/ex_crop.r
#'
#' @aliases crop
#' @rdname crop
#' @exportMethod crop
methods::setMethod(
	f = "crop",
	signature = c(x = "GRaster"),
	definition = function(x, y) {
	
	if (inherits(y, "GVector")) {
		compareGeom(x, y)
	} else {
		if (crs(x) != crs(y)) stop("Rasters have different coordinate references systems.")
	}
	.restore(x)

	### change region to match the extent of y but have the same resolution and registration as x
	if (west(y) <= west(x)) {
		w <- west(y)
	} else {
  		cells <- (west(y) - west(x)) / xres(x)
  		cells <- ceiling(cells)
  		w <- west(x) + cells * xres(x)
	}

	if (east(y) >= east(x)) {
		e <- east(x)
	} else {
  		cells <- (east(x) - east(y)) / xres(x)
		if (inherits(y, "GVector")) {
			cells <- floor(cells)
		} else if (inherits(y, "GRaster")) {
			cells <- floor(cells) # !!!
		}
  		e <- east(x) - cells * xres(x)
	}

	if (south(y) <= south(x)) {
		s <- south(x)
	} else {
  		cells <- (south(y) - south(x)) / yres(x)
  		if (inherits(y, "GVector")) {
			cells <- floor(cells)
		} else if (inherits(y, "GRaster")) {
   			cells <- ceiling(cells)
		}
  		s <- south(x) + cells * yres(x)
	}

	if (north(y) >= north(x)) {
		n <- north(x)
	} else {
  		cells <- (north(x) - north(y)) / yres(x)
		if (inherits(y, "GVector")) {
			cells <- floor(cells)
		} else if (inherits(y, "GRaster")) {
			cells <- ceiling(cells)
		}
  		n <- north(x) - cells * yres(x)
	}

	w <- as.character(w)
	e <- as.character(e)
	s <- as.character(s)
	n <- as.character(n)
	ewres <- as.character(xres(x))
	nsres <- as.character(yres(x))

	args <- list(
		cmd = "g.region",
		w = w, e = e, s = s, n = n,
		ewres = ewres, nsres = nsres,
		flags = c("quiet", "overwrite"),
		intern = TRUE
	)

 	do.call(rgrass::execGRASS, args = args)

	### crop by creating copy of focal raster
	gns <- .copyGSpatial(x, reshapeRegion = FALSE)
	.makeGRaster(gns, names(x))

	} # EOF
)

#' @aliases crop
#' @rdname crop
#' @exportMethod crop
methods::setMethod(
	f = "crop",
	signature = c(x = "GVector"),
	definition = function(x, y, ext = FALSE) {

	compareGeom(x, y)
	.restore(x)

	### crop
	gn <- .makeGName("crop", "vector")
	if (inherits(y, "GVector")) {

		xgeom <- geomtype(x, grass = TRUE)
		ygeom <- geomtype(y, grass = TRUE)

		args <- list(
			cmd = "v.clip",
			input = .gnames(x),
			output = gn,
			flags = c("quiet", "overwrite"),
			intern = TRUE
		)
		
		if (ext) {
			regionExt(y, respect = "dim")
			args$flags <- c(args$flags, "r")
			args$clip <- .gnames(y)
		} else if (ygeom %in% c("point", "line")) {
			ch <- convHull(y)
			args$clip <- .gnames(ch)
		} else if (ygeom == "area") {
			args$clip <- .gnames(y)
		}

	} else if (inherits(y, "GRaster")) {

		args <- list(
			cmd = "v.clip",
			input = .gnames(x),
			clip = .gnames(y),
			output = gn,
			flags = c("r", "quiet", "overwrite"),
			intern = TRUE
		)

	}

	do.call(rgrass::execGRASS, args = args)

	.makeGVector(gn)

	} # EOF
)
