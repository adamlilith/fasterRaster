#' Sample random points from a GRaster or GVector
#'
#' @description `spatSample()` randomly locates points across a `GRaster` or `GVector`. It can return the coordinates, values associated with the points, or both.
#' 
#' @param x A `GRaster` or `GVector`.
#' 
#' @param size Numeric integer or integer > 0: Number of points to create.
#' 
#' @param as.points Logical: If `FALSE` (default), the output is a `data.frame` or `data.table`. If `TRUE`, the output is a "points" `GVector`.
#' 
#' @param values Logical: If `TRUE` (default), values of the `GRaster` at points are returned.
#' 
#' @param cat Logical: If `TRUE` and the `GRaster` is [categorical][tutorial_raster_data_types], then return the category label of each cell. If `values` is also `TRUE`, then the cell value will also be returned.
#' 
#' @param xy Logical: If `TRUE`, return the longitude and latitude of each point. Default is `FALSE`.
#'
#' @param strata Either `NULL` (default), or a `GVector` defining strata. If supplied, the `size` argument will be interpreted as number of points to place per geometry in `strata`.
#' 
#' @param zlim Either `NULL` (default), or a vector of two numbers defining the lower and upper altitudinal bounds of coordinates.
#' 
#' @param seed Either `NULL` (default) or an integer: Used as the random seed. If `NULL`, then **GRASS** will generate its own seed.
#'
#' @returns A `data.frame`, `data.table`, or `GVector`.
#' 
#' @seealso [sampleRast()], [terra::spatSample()], module `v.random` in **GRASS**
#'
#' @example man/examples/ex_sampleRast_spatSample.r
#'
#' @aliases spatSample
#' @rdname spatSample
#' @exportMethod spatSample
methods::setMethod(
	f = "spatSample",
	signature = "GRaster",
	function(
		x,
		size,
		as.points = FALSE,
		values = TRUE,
		cat = FALSE,
		xy = FALSE,
		strata = NULL,
		zlim = NULL,
		seed = NULL
	) {

	.restore(x)
	region(x)

	if (!xy) values <- TRUE

	src <- .makeSourceName("v_random", "vector")

	args <- list(
		cmd = "v.random",
		output = src,
		npoints = size,
		flags = c("quiet", "overwrite")
	)

	if (!is.null(strata)) {
		args$restrict <- sources(strata)
		args$flags <- c(args$flags, "a")
	}

	if (!is.null(zlim)) {
		args$zmin <- zlim[1L]
		args$zmax <- zlim[2L]
		args$flags <- c(args$flags, "z")
	}

	if (!is.null(seed)) args$seed <- seed

	do.call(rgrass::execGRASS, args = args)

	# return coordinates
	if (xy) {
		out <- .crdsVect(src, z = is.3d(x), gm = "points")
	}

	# extract values from raster
	if (values | cat) {

		nLayers <- nlyr(x)
		for (i in seq_len(nLayers)) {

			args <- list(
				cmd = "r.what",
				map = sources(x)[i],
				points = src,
				null_value = "NA",
				flags = c("quiet", "overwrite"),
				intern = TRUE
			)

			vals <- do.call(rgrass::execGRASS, args = args)

			pillars <- gregexpr(vals, pattern = "\\|\\|")
			pillars <- unlist(pillars)
			ncs <- nchar(vals)
			vals <- substr(vals, pillars + 2L, ncs)
			vals[vals == "NA"] <- NA

			if (is.cell(x)[i]) {
				vals <- as.integer(vals)
			} else {
				vals <- as.numeric(vals)
			}

			this <- data.table::data.table(TEMPTEMP__ = vals)
			names(this) <- names(x)[i]

			# category label instead of value
			if (cat && is.factor(x)[i]) {

				levs <- levels(x[[i]])[[1L]]
				thisCat <- levs[match(vals, levs[[1L]]), 2L]
				names(thisCat) <- paste0(names(x)[i], "_cat")

				if (values) {
					this <- cbind(this, thisCat)
				}
			}

			if (exists("out", inherits = FALSE)) {
				out <- cbind(out, this)
			} else {
				out <- this
			}

		} # next raster

	} # if wanting values

	if (as.points) {
		.vAttachTable(src)
		out <- .makeGVector(src, table = out)
	} else {
		if (!getFastOptions("useDataTable")) out <- as.data.frame(out)
	}
	out

	} # EOF

)

#' @aliases spatSample
#' @rdname spatSample
#' @exportMethod spatSample
methods::setMethod(
	f = "spatSample",
	signature = "GVector",
	function(
		x,
		size,
		as.points = FALSE,
		values = TRUE,
		xy = FALSE,
		strata = NULL,
		zlim = NULL,
		seed = NULL
	) {

	.restore(x)
	region(x)

	if (geomtype(x) != "polygons") x <- convHull(x)
	if (!xy) values <- TRUE

	# if sampling by strata, keey polygons as-is
	if (!is.null(strata)) {

		srcRestrict <- sources(x)

	# if not sampling by strata, dissolve all polygons
	} else {

		# make copy of vector and force all categories to 1
		col <- rstring(1L)
		.vAddColumn(x, name = col, type = "integer")
		.vUpdateColumn(x, column = col, value = 1L)

		srcRestrict <- .makeSourceName("v_dissolve", "vector")
		args <- list(
			cmd = "v.dissolve",
			input = sources(x),
			output = srcRestrict,
			column = col,
			flags = c("quiet", "overwrite")
		)	
		do.call(rgrass::execGRASS, args = args)

	}
	

	src <- .makeSourceName("v_random", "vector")
	args <- list(
		cmd = "v.random",
		output = src,
		npoints = size,
		restrict = srcRestrict,
		flags = c("quiet", "overwrite")
	)

	if (!is.null(strata)) args$flags <- c(args$flags, "a")

	if (!is.null(zlim)) {
		args$zmin <- zlim[1L]
		args$zmax <- zlim[2L]
		args$flags <- c(args$flags, "z")
	}

	if (!is.null(seed)) args$seed <- seed

	do.call(rgrass::execGRASS, args = args)

	# return coordinates
	if (xy) {
		out <- .crdsVect(src, z = is.3d(x), gm = "points")
	}

	# extract values from vector
	if (values) {

		vals <- extract(x, src)
		vals$id.y <- NULL

		if (exists("out", inherits = FALSE)) {
			out <- cbind(out, vals)
		} else {
			out <- vals
		}

	} # if wanting values

	if (as.points) {
		# .vAttachTable(src)
		out <- .makeGVector(src, table = out)
	} else {
		if (!getFastOptions("useDataTable")) out <- as.data.frame(out)
	}
	out

	} # EOF

)
