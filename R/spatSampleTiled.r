#' Sample random points from a GRaster or GVector
#'
#' @description `spatSample()` randomly locates points across a `GRaster` or `GVector`. It can return a `GVector`, the coordinates, values associated with the points, or all of these. If you want to generate a raster with randomly-sampled cells, see [sampleRast()]. The `tiles` argument can be useful for speeding up creation of random points when the number of points is very large and you wish to extract values from the rasters (`values` or `cats` is `TRUE`).
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
#' @param tiles Numeric/integer vector (only used if `x` is a `GRaster`): Number of tiles into which to divide `x` before randomly placing points. This can be a single number, in which case `x` is divided into `n` × `n` tiles, or two values in which case it is divided into `n[1]` × `n[2]` tiles (rows x columns). Random points are allocated across tiles with probabilities proportionate to the total area of each tile. The default value is 1, meaning the raster is used as-is (no tiling).
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
#' @aliases spatSampleTiled
#' @rdname spatSampleTiled
#' @exportMethod spatSampleTiled
methods::setMethod(
	f = "spatSampleTiled",
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
		tiles = 1,
		zlim = NULL,
		seed = NULL,
		verbose = FALSE
	) {

	if (FALSE) {
	
		size <- 20
		as.points <- FALSE
		values <- TRUE
		cats <- TRUE
		xy <- FALSE
		strata <- NULL
		byStratum <- FALSE
		tiles <- 1
		zlim <- NULL
		seed <- NULL
		verbose <- FALSE
	
	
	}

	if (!is.null(zlim) & (values | cats)) stop("You cannot at present extract values or categories using 3D points.")
	if (!xy & !as.points & !values & !cats) stop("At least one of `xy`, `as.points`, `values`, or `cats` must be TRUE.")

	.locationRestore(x)
	.region(x)
	
	# ### fast point location... use R
	# ### v.random is REALLY slow for even moderately large sample sizes
	# if (is.null(strata)) {

	# 	# for unprojected, we want to adjust for smaller cells near poles, so we oversample, then subsample
	# 	extent <- ext(x, vector = TRUE)
	# 	if (is.lonlat(x)) {

	# 		# sampling scheme for latitude:
	# 		# divide y extent into large number of bins
	# 		# weight each bin by cos(latitude)
	# 		# sample with replacement from bin y values
	# 		# add a uniform random number +- half bin width

	# 		yres <- yres(x)
	# 		yLower <- extent[3L] + 0.5 * yres
	# 		yUpper <- extent[4L] - 0.5 * yres
	# 		ydim <- dim(x)[2L]
	# 		nMarks <- 100 * ydim
	# 		halfDelta <- 0.5 * (yUpper - yLower) / (nMarks - 1)
	# 		yMarks <- seq(yLower, yUpper, length.out = nMarks)
	# 		w <- abs(cos(pi * yMarks / 180))
	# 		ys <- sample(yMarks, size, prob = w, replace = TRUE)
	# 		ys <- ys + stats::runif(size, -halfDelta, halfDelta)
		
	# 	} else {
	# 		# if not long/lat, just sample uniformly
	# 		ys <- stats::runif(size, extent[3L], extent[4L])
	# 	}

	# 	xs <- stats::runif(size, extent[1L], extent[2L])
	# 	if (!is.null(zlim)) zs <- stats::runif(size, zlim[1L], zlim[2L])

	# 	if (xy) {
	# 		if (is.null(zlim)) {
	# 			out <- data.table::data.table(x = xs, y = ys)
	# 		} else {
	# 			out <- data.table::data.table(x = xs, y = ys, z = zs)
	# 		}
	# 	}

	# 	### NB The script below ingests points in chunks bc that has proven faster than doing all at once for larger numbers of points.

	# 	xs <- round(xs, 7L)
	# 	ys <- round(ys, 7L)
	# 	if (!is.null(zlim)) zs <- round(zs, 7L)

	# 	xs[xs < extent[1L]] <- extent[1L]
	# 	xs[xs > extent[2L]] <- extent[2L]
	# 	ys[ys < extent[3L]] <- extent[3L]
	# 	ys[ys > extent[4L]] <- extent[4L]

	# 	### use v.in.ascii to ingest each subset of points
	# 	##################################################

	# 	nAtATime <- 2E5 # optimal-ish size based on manual checks
	# 	sets <- ceiling(size / nAtATime)
	# 	srcs <- .makeSourceName("spatSample_v_in_ascii", "vector", n = sets)

	# 	if ((verbose | faster("verbose")) & sets > 1)  {
	# 		omnibus::say("Ingesting points...")
	# 		pb <- utils::txtProgressBar(min = 0, max = sets, initial = 0, style = 3, width = 30)
	# 	}

	# 	for (set in seq_len(sets)) {

	# 		if ((verbose | faster("verbose")) & sets > 1) utils::setTxtProgressBar(pb, set)

	# 		index <- (nAtATime * (set - 1) + 1):(min(nAtATime * set, size))
	# 		thisXs <- xs[index]
	# 		thisYs <- ys[index]
	# 		if (!is.null(zlim)) thisZs <- zs[index]

	# 		if (is.null(zlim)) {
	# 			coords <- data.table::data.table(cat = index, x = thisXs, y = thisYs)
	# 		} else {
	# 			coords <- data.table::data.table(cat = index, x = thisXs, y = thisYs, z = thisZs)
	# 		}
	# 		coords[ , coords := do.call(paste, c(.SD, sep = "|"))]
	# 		coords <- coords[ , "coords", drop = FALSE]

	# 		tf <- tempfile(fileext = ".txt")
	# 		data.table::fwrite(coords, tf, col.names = FALSE, quote = FALSE, scipen = 20L)

	# 		args <- list(
	# 			"v.in.ascii",
	# 			input = tf,
	# 			output = srcs[set],
	# 			format = "point",
	# 			separator = "pipe",
	# 			cat = 1, x = 2, y = 3,
	# 			flags = c(.quiet(), "overwrite", "t", "n", "b") # "b" --> no topology
	# 			# flags = c(.quiet(), "overwrite", "t", "n") # "b" --> no topology
	# 		)
	# 		if (!is.null(zlim)) {
	# 			args$flags <- c(args$flags, "z")	
	# 			args$z <- 4
	# 		}
	# 		do.call(rgrass::execGRASS, args = args)
	# 		unlink(tf)

	# 		# if (set > 1) {
				
	# 		# 	topCat <- min(nAtATime * set, size)
	# 		# 	srcs[set] <- .vIncrementCats(srcs[set], add = topCat)

	# 		# }

	# 	} # next set

	# 	if ((verbose | faster("verbose")) & sets > 1)  close(pb)

	# 	### use v.patch to combine subsets of points
	# 	############################################

	# 	# seems like we can combine at least 11 vectors at a time, but not too many more
	# 	srcsAtATime <- 10L # number of sources to combine at a time (plus the running `x` source)

	# 	nSrcs <- length(srcs)
	# 	sets <- ceiling(nSrcs / srcsAtATime)

	# 	if ((verbose | faster("verbose")) & sets > 1)  {
	# 		omnibus::say("Collapsing points...")
	# 		pb <- utils::txtProgressBar(min = 0, max = sets, initial = 0, style = 3, width = 30)
	# 	}

	# 	if (nSrcs == 1L) {
	# 		src <- srcs
	# 	} else {
			
	# 		for (set in seq_len(sets)) {

	# 			if ((verbose | faster("verbose")) & sets > 1L)  utils::setTxtProgressBar(pb, set)

	# 			index <- (1 + srcsAtATime * (set - 1)) : min(nSrcs, set * srcsAtATime)
	# 			srcIn <- srcs[index]
	# 			input <- paste(srcIn, collapse = ",")
	# 			if (set > 1) input <- paste0(src, ",", input)
			
	# 			src <- .makeSourceName("spatSample_v_patch", "vector")

	# 			rgrass::execGRASS(
	# 				cmd = "v.patch",
	# 				input = input,
	# 				output = src,
	# 				flags = c(.quiet(), "overwrite", "b", "n") # "n" ==> no topology
	# 				# flags = c(.quiet(), "overwrite") # "n" ==> input has topology, "b" ==> don't build topology
	# 			)
				
	# 			.rm(srcIn, type = "vector", warn = FALSE)

	# 		}

	# 		if ((verbose | faster("verbose")) & sets > 1)  close(pb)

	# 	} # if > 1 set

	# # if strata is not NULL
	# } else {
		
	# 	if (size > 200000) .message("spatSample_strata", "Using `strata` when selecting a large number of points can take a long time.")

	# 	src <- .makeSourceName("spatSample_v_random", "vector")

	# 	args <- list(
	# 		cmd = "v.random",
	# 		output = src,
	# 		npoints = size,
	# 		flags = c(.quiet(), "overwrite")
	# 	)

	# 	if (!is.null(seed)) {
	# 		seed <- round(seed)
	# 		args$seed <- seed
	# 	}

	# 	if (!is.null(strata)) {
	# 		args$restrict <- sources(strata)
	# 		if (byStratum) args$flags <- c(args$flags, "a")
	# 	}

	# 	if (!is.null(zlim)) {
	# 		args$zmin <- zlim[1L]
	# 		args$zmax <- zlim[2L]
	# 		args$flags <- c(args$flags, "z")
	# 	}

	# 	# if (!is.null(seed)) args$seed <- seed
	# 	args$seed <- round(1E9 * stats::runif(1))

	# 	# args$flags <- c(args$flags, "b") ### do not create topology... problems? YES!
	# 	do.call(rgrass::execGRASS, args = args)

	# 	# return coordinates
	# 	if (xy) {
	# 		out <- .crdsVect(src, z = is.3d(x), gtype = "points")
	# 	}

	# } # if strata is not NULL

	### select points by raster tile
	################################
	
	if (length(tiles) == 1L) tiles <- c(tiles, tiles)
	nTiles <- prod(tiles)
	if (nTiles > 1L) {

		# create tiles from 1st raster layer
		dims <- dim(x)
		extent <- ext(x, vector = TRUE)
		res <- res(x)
		tileSrcs <- .tiles(x = x[[1L]], n = tiles, dims = dims, extent = extent, res = res, overlap = 0)
		on.exit(.rm(tileSrcs, type = "rasters", warn = FALSE, verify = FALSE), add = TRUE)

		# calculate each tile's relative area
		tileAreas <- rep(NA_real_, nTiles)
		spatial <- attr(tileSrcs, "spatial")
		if (is.lonlat(x)) {

			for (i in 1L:nTiles) {
			
				lats <- spatial[[i]]$extent[c(3L, 4L)]
				meanLat <- mean(lats)
				cols <- spatial[[i]]$dim[ , 2L]
				rows <- spatial[[i]]$dim[ , 1L]
				tileAreas <- abs(cos(pi * meanLat / 180)) * rows * cols
			
			}

		} else {
			for (i in 1L:nTiles) tileAreas[i] <- prod(spatial[[i]]$dim)
		}

		selectedTile <- sample(1L:nTiles, size, prob = tileAreas, replace = TRUE)
		sizeByTile <- table(selectedTile)

		# select random points
		ptSrcs <- .makeSourceName("spatSample", "vector", n = nTiles)
		for (i in 1L:nTiles) {
		
			extent <- spatial[[i]]$extent
			.regionExt(extent, respect = "dimensions")
					
			args <- list(
				cmd = "v.random",
				output = ptSrcs[i],
				npoints = sizeByTile[i],
				flags = c(.quiet(), "overwrite")
			)
		
			# build topology... need if we want to return the points or extract coordinates
		    if (!as.points & !xy) args$flags <- c(args$flags, "b")

			if (!is.null(seed)) {
				seed <- round(seed)
				args$seed <- seed
			}

			do.call(rgrass::execGRASS, args = args)
			on.exit(.rm(ptSrcs, type = "vectors", warn = FALSE, verify = FALSE), add = TRUE)
		
		}

		# coordinates of points
		if (xy) {
		
			for (i in 1L:nTiles) {
				
				thisCats <- 1:sizeByTile[i]
				thisCoords <- .crdsVect(ptSrcs[i], z = !is.null(zlim), gtype = "points", cats = thisCats)
				if (i == 1L) {
					coords <- thisCoords
				} else {
					coords <- rbind(coords, thisCoords)
				}

			}

		}
	
		### extract by tile
		if (values | cats) {

			# extract for first layer... we already have these as tiles
			xNames <- names(x)[1L]
			dtype <- datatype(x)[1L]
			levs <- levels(x)[[1L]]
			.region(x)
			for (i in 1L:nTiles) {

				thisVals <- .extractFromRasterAtPoints(x = tileSrcs[i], y = ptSrcs[i], xNames = xNames, dtype = dtype, levels = levs, cats = cats, verbose = FALSE)

				if (i == 1L) {
					vals <- thisVals
				} else {
					vals <- rbind(vals, thisVals)
				}

			}

			# extract from subsequent rasters... tile them
			if (nlyr(x) > 1L) {
			
				dims <- dim(x)
				extent <- ext(x, vector = TRUE)
				res <- res(x)

				# for each layer
				for (i in 2L:nlyr(x)) {
				
					tileSrcs <- .tiles(x = x[[i]], n = tiles, dims = dims, extent = extent, res = res, overlap = 0)
				
					xNames <- names(x)[i]
					dtype <- datatype(x, type = "grass")[i]
					levs <- levels(x)[[i]]

					# extract from tile
					for (tile in 1L:nTiles) {

						thisVals <- .extractFromRasterAtPoints(x = tileSrcs[tile], y = ptSrcs[tile], xNames = xNames, dtype = dtype, levels = levs, cats = cats, verbose = FALSE)

						if (tile == 1L) {
							theseVals <- thisVals
						} else {
							theseVals <- rbind(theseVals, thisVals)
						}

					} # next tile

					vals <- cbind(vals, theseVals)
				
				} # next layer
			
			} # if >1 raster layer

		} # if extracting values/cats

		### combine points for output
		if (as.points | xy) {

			# thisCats <- .vCats(ptSrcs[1L], db = FALSE)
			# topCat <- max(thisCats)
			
			# for (i in 2L:nTiles) {

			# 	ptSrcs[i] <- .vIncrementCats(ptSrcs[i], add = topCat)
			# 	thisCats <- .vCats(srcs[i], db = FALSE)
			# 	topCat <- max(thisCats)
			
			# }

			# ### combine vectors
			# # seems like we can combine at least 11 vectors at a time, but not a lot at a time
			# srcsAtATime <- 10L # number of sources to combine at a time (plus the running `x` source)

			# nSrcs <- length(srcs)
			# sets <- ceiling(nSrcs / srcsAtATime)
			
			# for (set in seq_len(sets)) {

			# 	index <- (1L + srcsAtATime * (set - 1L)) : min(nSrcs, set * srcsAtATime)
			# 	srcIn <- srcs[index]
			# 	input <- paste(srcIn, collapse = ",")
			# 	input <- paste0(src, ",", input)
			
			# 	src <- .makeSourceName("v_patch", "vector")

			# 	rgrass::execGRASS(
			# 		cmd = "v.patch",
			# 		input = input,
			# 		output = src,
			# 		flags = c(.quiet(), "overwrite")
			# 	)
				
			# }

			src <- .rbind(ptSrcs)

		} # combine points for output
		
		# combine everything
		if (!xy & !values & !cats) {
			out <- data.table::data.table(NULL)
		} else if (xy & !(values | cats)) {
			out <- coords
		} else if (!xy & (values | cats)) {
			out <- vals
		} else if (xy & (values | cats)) {
			out <- cbind(coords, vals)
		}

	### if NOT tiling
	#################
	} else {

		if (verbose | faster("verbose")) omnibus::say("Placing points...")
		src <- .makeSourceName("spatSample", "vector")
		args <- list(
			cmd = "v.random",
			output = src,
			npoints = size,
			flags = c(.quiet(), "overwrite")
		)

		# build topology... need if we want to return the points or extract coordinates
		if (!as.points & !xy) args$flags <- c(args$flags, "b")

		# # build topology
		# rgrass::execGRASS(
		# 	"v.build",
		# 	map = src,
		# 	option = "build",
		# 	flags = c(.quiet(), "overwrite")
		# )

		if (!is.null(seed)) {
			seed <- round(seed)
			args$seed <- seed
		}

		do.call(rgrass::execGRASS, args = args)

		# point coordinates
		if (xy) out <- .crdsVect(src, z = !is.null(zlim), gtype = "points", cats = 1:size)

		# extract values from raster
		if (values | cats) {

			# rgrass::execGRASS(
			# 	cmd = "v.build",
			# 	map = src,
			# 	flags = c(.quiet(), "overwrite")
			# )

			vals <- .extractFromRasterAtPoints(x = x, y = src, cats = cats, verbose = verbose)

			if (exists("out", inherits = FALSE)) {
				out <- cbind(out, vals)
			} else {
				out <- vals
			}

		}

	} # if not tiling

	if (as.points) {

		.vAttachDatabase(src)
		if (exists("out", inherits = FALSE)) {
			# info <- .vectInfo(src)
			# nGeometries <- info$nGeometries
			# n <- nGeometries / size
			# out <- .makeGVector(src, table = out, cats = rep(1:size, each = n))
			out <- .makeGVector(src, table = out, cats = 1:size)
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
