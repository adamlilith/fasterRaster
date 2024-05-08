#' Sample random points from a GRaster or GVector
#'
#' @description `spatSample()` randomly locates points across a `GRaster` or `GVector`. It can return a `GVector`, the coordinates, values associated with the points, or all of these. If you want to generate a raster with randomly-sampled cells, see [sampleRast()].
#' 
#' @param x A `GRaster` or `GVector`.
#' 
#' @param size Numeric integer or integer > 0: Number of points to create.
#' 
#' @param as.points Logical: If `FALSE` (default), the output is a `data.frame` or `data.table`. If `TRUE`, the output is a "points" `GVector`.
#' 
#' @param values Logical: If `TRUE` (default), values of the `GRaster` at points are returned.
#' 
#' @param cats Logical: If `TRUE` and the `GRaster` is [categorical][tutorial_raster_data_types], then return the category label of each cell. If `values` is also `TRUE`, then the cell value will also be returned.
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
		cats = FALSE,
		xy = FALSE,
		strata = NULL,
		zlim = NULL,
		seed = NULL
	) {

	.locationRestore(x)
	.region(x)
	
	if (!xy) values <- TRUE

	src <- .makeSourceName("v_random", "vector")

	args <- list(
		cmd = "v.random",
		output = src,
		npoints = size,
		flags = c(.quiet(), "overwrite")
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

	args$flags <- c(args$flags, "b") ### do not create topology... problems?

	do.call(rgrass::execGRASS, args = args)

	# return coordinates
	if (xy) {
		out <- .crdsVect(src, z = is.3d(x), gtype = "points")
	}

	# extract values from raster
	if (values | cats) {

		nLayers <- nlyr(x)
		for (i in seq_len(nLayers)) {

			args <- list(
				cmd = "r.what",
				map = sources(x)[i],
				points = src,
				null_value = "NA",
				flags = c(.quiet(), "overwrite"),
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
			if (cats && is.factor(x)[i]) {

                levs <- levels(x[[i]])[[1L]]
                this <- levs[match(vals, levs[[1L]]), 2L]
                names(this) <- names(x)[i]
				this <- this[ , lapply(.SD, as.factor)]
                # this <- levs[match(vals, levs[[1L]]), 2L]
                # names(this) <- c(names(x)[i], paste0(names(x)[i], "_cat"))

				# if (values) {
				# 	this <- cbind(this, thisCat)
				# }
			}

			if (exists("out", inherits = FALSE)) {
				out <- cbind(out, this)
			} else {
				out <- this
			}

		} # next raster

	} # if wanting values

	if (as.points) {
		.vAttachDatabase(src)
		out <- .makeGVector(src, table = out)
	} else {
		if (!faster("useDataTable")) out <- as.data.frame(out)
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

	.locationRestore(x)
	.region(x)

	if (geomtype(x) != "polygons") x <- convHull(x)

	# if sampling by strata, keey polygons as-is
	if (!is.null(strata)) {

		srcRestrict <- sources(x)

	# if not sampling by strata, dissolve all polygons
	} else {

		# make copy of vector and force all categories to 1
		gtype <- geomtype(x)
		srcRestrict <- .aggregate(sources(x), dissolve = TRUE, gtype = gtype)

	}
	
	src <- .makeSourceName("v_random", "vector")
	args <- list(
		cmd = "v.random",
		output = src,
		npoints = size,
		restrict = srcRestrict,
		flags = c(.quiet(), "overwrite")
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
	if (xy) coords <- .crdsVect(src, z = is.3d(x), gtype = "points")

	# extract values from vector
	if (values) {

		if (!xy) coords <- .crdsVect(x = src, z = !is.null(zlim), gtype = "points")
		vals <- .extractFromVect(x, y = coords, xy = xy)
		vals$id.y <- NULL

	} # if wanting values

	table <- NULL
	if (xy & values) {
		table <- cbind(xy, vals)
	} else if (!xy & values) {
		table <- vals
	} else if (xy & !values) {
		table <- coords
	}

	if (as.points) {
		out <- .makeGVector(src, table = table)
	} else {
		if (!faster("useDataTable")) out <- as.data.frame(out)
	}
	out

	} # EOF

)
