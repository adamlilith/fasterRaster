#' Sample random points from a GRaster or GVector
#'
#' @description `spatSample()` randomly locates points across a `GRaster` or `GVector`. It can return a `GVector`, the coordinates, values associated with the points, or all of these. If you want to generate a raster with randomly-sampled cells, see [sampleRast()].
#' 
#' @param x A `GRaster` or `GVector`.
#' 
#' @param size Numeric value > 0: Number of points to create.
#' 
#' @param as.points Logical: If `FALSE` (default), the output is a `data.frame` or `data.table`. If `TRUE`, the output is a "points" `GVector`.
#' 
#' @param values Logical: If `TRUE` (default), values of the `GRaster` at points are returned.
#' 
#' @param cats Logical: If `TRUE` (default) and the `GRaster` is categorical, then return the category label of each cell. If `values` is also `TRUE`, then the cell value will also be returned.
#' 
#' @param xy Logical: If `TRUE`, return the longitude and latitude of each point. Default is `FALSE`.
#'
#' @param strata Either `NULL` (default), or a `GVector` defining strata. If supplied, the `size` argument will be interpreted as number of points to place per geometry in `strata`. Note that using strata can dramatically slow the process.
#'
#' @param byStratum Logical: If `FALSE` (default), then `size` number of points will be placed within the entire area delineated by `strata`. If `TRUE`, then `size` points will be placed within each subgeometry of `strata`.
#'
#' @param zlim Either `NULL` (default), or a vector of two numbers defining the lower and upper altitudinal bounds of coordinates. This cannot be combined with `values = TRUE` or `cats = TRUE`.
#'
#' @param seed Either `NULL` (default) or an integer: Random number seed. If this is `NULL`, the a seed will be set randomly. Values will be rounded to the nearest integer.
#'
#' @param verbose Logical: If `TRUE`, display progress. Default is `FALSE`.
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
		cats = TRUE,
		xy = FALSE,
		strata = NULL,
		byStratum = FALSE,
		zlim = NULL,
		seed = NULL,
		verbose = FALSE
	) {

	if (!is.null(zlim) & (values | cats)) stop("You cannot at present extract values or categories using 3D points.")
	if (!xy & !as.points & !values & !cats) stop("At least one of `xy`, `as.points`, `values`, or `cats` must be TRUE.")

	.locationRestore(x)
	.region(x)
	
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
			ys <- ys + stats::runif(size, -halfDelta, halfDelta)
		
		} else {
			# if not long/lat, just sample uniformly
			ys <- stats::runif(size, extent[3L], extent[4L])
		}

		xs <- stats::runif(size, extent[1L], extent[2L])
		if (!is.null(zlim)) zs <- stats::runif(size, zlim[1L], zlim[2L])

		if (xy) {
			if (is.null(zlim)) {
				out <- data.table::data.table(x = xs, y = ys)
			} else {
				out <- data.table::data.table(x = xs, y = ys, z = zs)
			}
		}

		### NB The script below ingests points in chunks be that has proven faster than doing all at once for larger numbers of points.

		xs <- round(xs, 7L)
		ys <- round(ys, 7L)
		if (!is.null(zlim)) zs <- round(zs, 7L)

		xs[xs < extent[1L]] <- extent[1L]
		xs[xs > extent[2L]] <- extent[2L]
		ys[ys < extent[3L]] <- extent[3L]
		ys[ys > extent[4L]] <- extent[4L]


		### use v.in.ascii to ingest each subset of points
		nAtATime <- 2E5 # optimal-ish size based on manual checks
		sets <- ceiling(size / nAtATime)
		srcs <- .makeSourceName("spatSample_v_in_ascii", "vector", n = sets)

		if ((verbose | faster("verbose")) & sets > 1)  {
			omnibus::say("Ingesting points...")
			pb <- utils::txtProgressBar(min = 0, max = sets, initial = 0, style = 3, width = 30)
		}

		for (set in seq_len(sets)) {

			if ((verbose | faster("verbose")) & sets > 1) utils::setTxtProgressBar(pb, set)

			index <- (nAtATime * (set - 1) + 1):(min(nAtATime * set, size))
			thisXs <- xs[index]
			thisYs <- ys[index]
			if (!is.null(zlim)) thisZs <- zs[index]

			if (is.null(zlim)) {
				coords <- data.table::data.table(cat = index, x = thisXs, y = thisYs)
			} else {
				coords <- data.table::data.table(cat = index, x = thisXs, y = thisYs, z = thisZs)
			}
			coords[ , coords := do.call(paste, c(.SD, sep = "|"))]
			coords <- coords[ , "coords", drop = FALSE]

			tf <- tempfile(fileext = ".txt")
			data.table::fwrite(coords, tf, col.names = FALSE, quote = FALSE, scipen = 20L)

			args <- list(
				"v.in.ascii",
				input = tf,
				output = srcs[set],
				format = "point",
				separator = "pipe",
				cat = 1, x = 2, y = 3,
				flags = c(.quiet(), "overwrite", "t", "n")
			)
			if (!is.null(zlim)) {
				args$flags <- c(args$flags, "z")	
				args$z <- 4
			}
			do.call(rgrass::execGRASS, args = args)

			# if (set > 1) {
				
			# 	topCat <- min(nAtATime * set, size)
			# 	srcs[set] <- .vIncrementCats(srcs[set], add = topCat)

			# }

		}

		if ((verbose | faster("verbose")) & sets > 1)  close(pb)

		### use v.patch to combine subsets of points
		# seems like we can combine at least 11 vectors at a time, but not a lot at a time
		srcsAtATime <- 10L # number of sources to combine at a time (plus the running `x` source)

		nSrcs <- length(srcs)
		sets <- ceiling(nSrcs / srcsAtATime)
		
		if ((verbose | faster("verbose")) & sets > 1)  {
			omnibus::say("Collapsing points...")
			pb <- utils::txtProgressBar(min = 0, max = sets, initial = 0, style = 3, width = 30)
		}

		for (set in seq_len(sets)) {

			if ((verbose | faster("verbose")) & sets > 1)  utils::setTxtProgressBar(pb, set)

			index <- (1 + srcsAtATime * (set - 1)) : min(nSrcs, set * srcsAtATime)
			srcIn <- srcs[index]
			input <- paste(srcIn, collapse = ",")
			if (set > 1) input <- paste0(src, ",", input)
		
			src <- .makeSourceName("spatSample_v_patch", "vector")

			rgrass::execGRASS(
				cmd = "v.patch",
				input = input,
				output = src,
				flags = c(.quiet(), "overwrite")
			)
			
		}

		if (faster("clean")) .rm(srcs, type = "vector", warn = FALSE)
		if ((verbose | faster("verbose")) & sets > 1)  close(pb)

	# if strata is not NULL
	} else {
		
		if (size > 200000) .message("spatSample_strata", "Using `strata` when selecting a large number of points can take a long time.")

		src <- .makeSourceName("spatSample_v_random", "vector")

		args <- list(
			cmd = "v.random",
			output = src,
			npoints = size,
			flags = c(.quiet(), "overwrite")
		)

		if (!is.null(seed)) {
			seed <- round(seed)
			args$seed <- seed
		}

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
		args$seed <- round(1E9 * stats::runif(1))

		# args$flags <- c(args$flags, "b") ### do not create topology... problems? YES!
		do.call(rgrass::execGRASS, args = args)

		# return coordinates
		if (xy) {
			out <- .crdsVect(src, z = is.3d(x), gtype = "points")
		}

	} # if strata is not NULL

	# extract values from raster
	if (values | cats) {

		vals <- .extractFromRasterAtPoints(x = x, y = src, cats = cats, verbose = verbose)

		if (exists("out", inherits = FALSE)) {
			out <- cbind(out, vals)
		} else {
			out <- vals
		}

	}

	if (as.points) {
		.vAttachDatabase(src)
		if (exists("out", inherits = FALSE)) {
			# info <- .vectInfo(src)
			# nGeometries <- info$nGeometries
			# n <- nGeometries / size
			# out <- .makeGVector(src, table = out, cats = rep(1:size, each = n))
			out <- .makeGVector(src, table = out)
		} else {
			out <- .makeGVector(src, cats = 1:size)
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

	if (!is.null(seed)) {
		if (!omnibus::is.wholeNumber(seed)) stop("Argument `seed` must be an integer or NULL.")
		seed <- round(seed)
	}
	
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
		restrict = sources(x),
		flags = c(.quiet(), "overwrite")
	)

	if (!is.null(seed)) {
		seed <- round(seed)
		args$seed <- seed
	}

	# if (!is.null(strata) & byStratum) args$flags <- c(args$flags, "a")
	if (byStratum) args$flags <- c(args$flags, "a")

	if (!is.null(zlim)) {
		args$zmin <- zlim[1L]
		args$zmax <- zlim[2L]
		args$flags <- c(args$flags, "z")
	}

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
