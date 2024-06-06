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
#' @param strata Either `NULL` (default), or a `GVector` defining strata. If supplied, the `size` argument will be interpreted as number of points to place per geometry in `strata`. Note that using strata can dramatically slow the process.
#'
#' @param byStratum Logical: If `FALSE` (default), then `size` number of points will be placed within the entire area delineated by `strata`. If `TRUE`, then `size` points will be placed within each subgeometry of `strata`.
#'
#' @param zlim Either `NULL` (default), or a vector of two numbers defining the lower and upper altitudinal bounds of coordinates. This cannot be combined with `values = TRUE` or `cats = TRUE`.
#'
#' @param seed Either `NULL` (default), or an integer. Used as the random seed.
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
		byStratum = FALSE,
		zlim = NULL
	) {

	if (!is.null(zlim) & (values | cats)) stop("You cannot at present extract values or categories using 3D points.")

	.locationRestore(x)
	.region(x)
	
	if (!xy) values <- TRUE

	src <- .makeSourceName("v_random", "vector")

	# fast point location... use R
	if (is.null(strata)) {

		# for unprojected, we want to adjust for smaller cells near poles, so we oversample, then subsample
		extent <- ext(x, vector = TRUE)
		if (is.lonlat(x)) {

			# sampling scheme for latitude:
			# divide y extent into large number of bins
			# weight each bin by cos(latitude)
			# sample with replacement from bin y values
			# add a uniform random number +- half bin width

			yres <- yres(x)
			yLower <- extent[3L] + 0.5 * yres
			yUpper <- extent[4L] - 0.5 * yres
			ydim <- dim(x)[2L]
			nMarks <- 100 * ydim
			halfDelta <- 0.5 * (yUpper - yLower) / (nMarks - 1)
			yMarks <- seq(yLower, yUpper, length.out = nMarks)
			w <- abs(cos(pi * yMarks / 180))
			ys <- sample(yMarks, size, prob = w, replace = TRUE)
			ys <- ys + runif(size, -halfDelta, halfDelta)
		
		} else {
			# if not long/lat, just sample uniformly
			ys <- runif(size, extent[3L], extent[4L])
		}

		xs <- runif(size, extent[1L], extent[2L])
		if (!is.null(zlim)) zs <- runif(siz3, zlim[1L], zlim[2L])

		# # # # for unprojected, we want to adjust for smaller cells near poles, so we oversample, then subsample
		# # # if (is.lonlat(x)) {
		# # # 	sizeInflated <- round(2 * size)
		# # # } else {
		# # # 	sizeInflated <- size
		# # # }

		# # # extent <- ext(vector = TRUE)
		# # # xs <- runif(sizeInflated, extent[1L], extent[2L])
		# # # ys <- runif(sizeInflated, extent[3L], extent[4L])
		# # # if (!is.null(zlim)) zs <- runif(sizeInflated, zlim[1L], zlim[2L])

		# # # if (is.lonlat(x)) {
		
		# # # 	w <- abs(cos(pi * ys / 180))
		# # # 	w <- size / sum(w) * w
		# # # 	keeps <- sampling::UPrandomsystematic(w)
		# # # 	keeps <- which(keeps == 1)
		# # # 	if (length(keeps) != size) stop("Did not select desired number.")
		# # # 	xs <- xs[keeps]
		# # # 	ys <- ys[keeps]
		# # # 	if (!is.null(zlim)) zs <- zs[keeps]
		
		# # # }

		if (xy) {
			if (is.null(zlim)) {
				out <- data.table::data.table(x = xs, y = ys)
			} else {
				out <- data.table::data.table(x = xs, y = ys, z = zs)
			}
		}

		# coerce points to GRASS vector		
		coords <- rep(NA_character_, size)
		if (is.null(zlim)) {
			for (i in seq_len(size)) coords[i] <- paste(c(xs[i], "|", ys[i]), collapse = "")
		} else {
			for (i in seq_len(size)) coords[i] <- paste(c(xs[i], "|", ys[i], "|", zs[i]), collapse = "")
		}

		tf <- tempfile(fileext = ".txt")
		write(coords, tf, ncolumns = 1L)

		src <- .makeSourceName("spatSampleLarge", "vector")
		args <- list(
			"v.in.ascii",
			input = tf,
			output = src,
			format = "point",
			separator = "pipe",
			flags = c(.quiet(), "verbose", "overwrite", "t", "n")
		)
		if (!is.null(zlim)) args$flags <- c(args$flags, "z")	
		do.call(rgrass::execGRASS, args = args)

	# slow point location... use GRASS
	} else {
		
		if (size > 250000) .message("spatSample_strata", "Using `strata` when selecting a large number of points can take a long time.")

		args <- list(
			cmd = "v.random",
			output = src,
			npoints = size,
			flags = c(.quiet(), "overwrite")
		)

		if (!is.null(strata)) {
			args$restrict <- sources(strata)
			if (byStratum) args$flags <- c(args$flags, "a")
		}

		if (!is.null(zlim)) {
			args$zmin <- zlim[1L]
			args$zmax <- zlim[2L]
			args$flags <- c(args$flags, "z")
		}

		# if (!is.null(seed)) args$seed <- seed
		args$seed <- round(1E9 * runif(1))

		# args$flags <- c(args$flags, "b") ### do not create topology... problems? YES!
		do.call(rgrass::execGRASS, args = args)

		# return coordinates
		if (xy) {
			out <- .crdsVect(src, z = is.3d(x), gtype = "points")
		}

	}

	# extract values from raster
	if (values | cats) {

		nLayers <- nlyr(x)
		for (i in seq_len(nLayers)) {

			vals <- rgrass::execGRASS(
				cmd = "r.what",
				map = sources(x)[i],
				points = src,
				null_value = "NA",
				flags = c(.quiet(), "overwrite"),
				intern = TRUE
			)

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
		if (exists("out", inherits = FALSE)) {
			out <- .makeGVector(src, table = out)
		} else {
			out <- .makeGVector(src)
		}
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
		byStratum = FALSE,
		zlim = NULL,
		seed = NULL
	) {

	.locationRestore(x)
	.region(x)

	if (geomtype(x) != "polygons") x <- convHull(x)

	# # if sampling by strata, keep polygons as-is
	# if (!is.null(strata)) {

	# 	srcRestrict <- sources(x)

	# # if not sampling by strata, dissolve all polygons
	# } else {

	# 	# make copy of vector and force all categories to 1
	# 	gtype <- geomtype(x)
	# 	srcRestrict <- .aggregate(sources(x), dissolve = TRUE, gtype = gtype, copy = TRUE)

	# }
	
	src <- .makeSourceName("spatSample_v_random", "vector")
	args <- list(
		cmd = "v.random",
		output = src,
		npoints = size,
		restrict = src,
		flags = c(.quiet(), "overwrite")
	)

	if (!is.null(strata) & byStratum) args$flags <- c(args$flags, "a")

	if (!is.null(zlim)) {
		args$zmin <- zlim[1L]
		args$zmax <- zlim[2L]
		args$flags <- c(args$flags, "z")
	}

	if (!is.null(seed)) args$seed <- seedz

	do.call(rgrass::execGRASS, args = args)

	# return coordinates
	if (xy) coords <- .crdsVect(src, z = is.3d(x), gtype = "points")

	# extract values from vector
	if (values) {

		if (!xy) coords <- .crdsVect(x = src, z = !is.null(zlim), gtype = "points")
		vals <- .extractFromVect(x, y = coords, xy = xy)
		vals$id.y <- NULL

	} # if wanting values

	out <- NULL
	if (xy & values) {
		out <- cbind(xy, vals)
	} else if (!xy & values) {
		out <- vals
	} else if (xy & !values) {
		out <- coords
	}

	if (as.points) {
		out <- .makeGVector(src, table = out)
	} else {
		if (!faster("useDataTable")) out <- as.data.frame(out)
	}
	out

	} # EOF

)
