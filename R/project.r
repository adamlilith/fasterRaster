#' Change the coordinate reference system of a GRaster or GVector
#'
#' @description There are two ways to project rasters and vectors into a different coordinate reference system (CRS) in **fasterRaster**.  Both are different from how they are implemented in other packages, so it is important to understand how it works in **fasterRaster**.
#'
#' First, if you are wanting to change the CRS for a `SpatRaster`, `SpatVector`, or a raster or vector on disk, you can use the [fast()] function. To do this, you need to start a **GRASS** session using [faster()] with the CRS that you want to transform the raster/vector into, then convert the object to a `GRaster` or `GVector` using [fast()]. Note that for rasters the `method` argument in that function may be important.
#'
#' Second, if you are wanting to change the CRS for a `GRaster` or `GVector`, you need to initiate a new **GRASS** [location][tutorial_locations] using [faster()] with the CRS that you want to transform it into. This means you need to keep track of which "location" you are working in, as multiple rasters and/or vectors can only be used in the function if they are in the same location. **This function, `project()`, is for projecting `GRaster`s or `GVector`s.**
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param y A `NULL` (default), or a `GRaster` or `GVector`:
#' * If `NULL`, then `x` is projected to the CRS of the current [location][tutorial_location] (i.e., `x` should not be in the current "location"). `x` will not be resampled.
#' * If `x` is a `GRaster` and `y` is a `GRaster`, then `x` will be projected to the CRS of `y` and resampled to the same resolution as `y`. `x` should not be the same [location][tutorial_location] as `y`.
#' * If `x` is a `GRaster` and `y` is a `GVector`, then `x` will be projected to the CRS of `y`. `x` will not be resampled. `x` should not be the same [location][tutorial_location] as `y`.
#' * If `x` is a `GVector`, then `x` will be projected to the CRS of `y`.
#'
#' @param method Character or `NULL`: Method to use to conduct the transformation (rasters only). Partial matching is used.
#' * `NULL` (default): Automatically choose based on raster properties (`near` for categorical data, `bilinear` for continuous data)
#' * `'near'`: Nearest neighbor. Best for categorical data, and often a poor choice for continuous data.  If [ncat()] is >0, this method will be used by default.
#' * `'bilinear'`: Bilinear interpolation (default for non-categorical data; uses weighted values from 4 cells).
#' * `'bicubic'`: Bicubic interpolation (uses weighted values from 16 cells).
#' * `'lanczos'`: Lanczos interpolation (uses weighted values from 25 cells).
#' Note that if `x` and `y` are `GRaster`s, then the same method is used to resample `x` to `y` before projecting `x`.
#'
#' @param location,mapset Character or `NULL` (default): The name of the [location][tutorial_locations] and [mapset][tutorial_locations] which has the CRS you to which you wish to transform the `GRaster` or `GVector`. You can get this using [location()] and [mapset()]. If left as `NULL`, then it will be assumed that you want to project the object to the current location and mapset.
#'
#' @param trim Logical: When projecting rasters that "wrap around" (i.e., whole-world rasters or rasters that have edges that actually circle around to meet on the globe), `trim` should be `FALSE` to avoid removing rows and columns from the "edge" of the map.
#'
#' @details When projecting a raster, the "fallback" methods in `r.import` are actually used, even though the `method` argument takes the strings for non-fallback methods. See the manual page for the `r.import` **GRASS** module.
#' 
#' @returns A `GRaster` or `GVector`.
#' 
#' @seealso [terra::project], [sf::st_transform()]
#'
#' @example man/examples/ex_project.r
#'
#' @aliases project
#' @rdname project
#' @exportMethod project
methods::setMethod(
	f = 'project',
	signature = c(x = 'GRaster'),
	definition = function(
		x,
		y = NULL,
		method = NULL,
		trim = TRUE,
		location = NULL,
		mapset = NULL
	) {
	
	# start location and mapset
	startLoc <- location()
	startMapset <- mapset()
	
	# target location and mapset
	if (is.null(y)) {
		toLoc <- location()
		toMapset <- mapset()
	} else {
		toLoc <- location(y)
		toMapset <- mapset(y)
	}
	
	if (toLoc == location(x) & toMapset == mapset(x)) {
		warning('Object is already in the desired coordinate reference system.')
		return()
	}
	
	# method
	if (!is.null(method)) method <- pmatchSafe(method, c('nearest', 'bilinear', 'bicubic', 'lanczos'))
	
	### resample 1st
	################
	
	if (inherits(y, 'GRaster')) {

		# set method
		if (is.null(method)) {
		
			resampMethod <- if (all(ncat(x) == 0L)) {
				'bilinear'
			} else if (all(ncat(x) > 0L)) {
				'nearest'
			} else {
				stop('Rasters are a mix of categorical and continuous. Project each type of raster separately.')
			}
			
		} else {
			resampMethod <- method
		}

		.restore(x)
	
		# back-project region from "to" location to "from" location
		targetExt <- ext(y)
		targetDim <- dim(y)
		targetCRS <- crs(y)
		empty <- matrix(NA, nrow=targetDim[1L], ncol=targetDim[2L])
		emptyTo <- terra::rast(empty, crs=targetCRS, extent=targetExt)
		
		emptyFrom <- terra::project(emptyTo, crs(x), method='near', align=TRUE)

		# reshape region in "from" region
		xExt <- as.vector(ext(x))
		n <- as.character(xExt[4L])
		s <- as.character(xExt[3L])
		e <- as.character(xExt[2L])
		w <- as.character(xExt[1L])
		
		yRes <- terra::res(emptyFrom)
		ewres <- as.character(yRes[1L])
		nsres <- as.character(yRes[2L])
		
		args <- list(
			cmd = 'g.region',
			n = n, s = s, e = e, w = w,
			ewres = ewres, nsres = nsres,
			flags = c('o', 'a', 'quiet'),
			intern = TRUE
		)
		
		do.call(rgrass::execGRASS, args=args)

		# resample
		if (resampMethod == 'nearest') {
			
			args <- list(
				cmd ='r.resample',
				input = NA,
				output = NA,
				flags = c('quiet', 'overwrite')
			)
			
		} else {
		
			args <- list(
				cmd = 'r.resamp.interp',
				input = NA,
				output = NA,
				method = resampMethod,
				flags = c('quiet', 'overwrite')
			)
			
		}

		nLayers <- nlyr(x)
		gns <- .makeGname('resampled', 'raster', nLayers)
		for (i in 1L:nLayers) {

			args$input <- gnames(x)[i]
			args$output <- gns[i]
		
			do.call(rgrass::execGRASS, args=args)
			
			thisOut <- if (i == 1L) {
				makeGRaster(gns[i], names(x)[i])
			} else {
				c(thisOut, makeGRaster(gns[i], names(x)[i]))
			}
		
		}
		
		x <- thisOut
	
	}

	### project 2nd
	###############

	# set method
	if (is.null(method)) {
		method <- if (all(ncat(x) == 0L)) {
			'bilinear'
		} else if (all(ncat(x) > 0L)) {
			'nearest'
		} else {
			stop('Rasters are a mix of categorical and continuous. Project each type of raster separately.')
		}
	}
	
	if (!is.null(method)) method <- pmatchSafe(method, c('nearest', 'bilinear_f', 'bicubic_f', 'lanczos_f'))

	# set session to target location/mapset
	fastRestore(location=toLoc, mapset=toMapset)
	
	# set region in focal region to what the raster will be when it's projected so we don't crop things
	focalExt <- ext(x)
	focalDims <- dim(x)
	focalCRS <- crs(x)
	empty <- matrix(NA, nrow=focalDims[1L], ncol=focalDims[2L])
	emptyFrom <- terra::rast(empty, crs=focalCRS, extent=focalExt)
	
	emptyTo <- terra::project(emptyFrom, crs(), method='near', align=TRUE)
	region(emptyTo)

	### project
	flags <- c('quiet', 'overwrite')
	if (!trim) flags <- c(flags, 'n')

	args <- list(
		cmd = 'r.proj',
		input = NA,
		output = NA,
		location = location(x),
		mapset = mapset(x),
		method = method,
		memory = getFastOptions('memory'),
		flags = flags,
		intern = TRUE
	)

	nLayers <- nlyr(x)
	gns <- .makeGname('projected', 'raster', nLayers)
	for (i in 1L:nLayers) {
	
		args$input <- gnames(x)[i]
		args$output <- gns[i]
	
		do.call(rgrass::execGRASS, args=args)
		if (i == 1L) {
			out <- makeGRaster(args$output, names=names(x)[i])
		} else {
			thisOut <- makeGRaster(args$output, names=names(x)[i])
			out <- c(out, thisOut)
		}
	
	}

	if (location() != startLoc | mapset() != startMapset) fastRestore(location=startLoc, mapset=startMapset)

	out
	
	} # EOF
)


#' @aliases project
#' @rdname project
#' @exportMethod project
methods::setMethod(
	f = 'project',
	signature = c(x = 'GVector'),
	definition = function(
		x,
		y = NULL,
		location = NULL,
		mapset = NULL
	) {

	# start location and mapset
	startLoc <- location()
	startMapset <- mapset()
	
	# target location and mapset
	if (is.null(y)) {
		toLoc <- location()
		toMapset <- mapset()
	} else {
		toLoc <- location(y)
		toMapset <- mapset(y)
	}
	
	if (toLoc == location(x) & toMapset == mapset(x)) {
		warning('Object is already in the desired coordinate reference system.')
		return()
	}
	
	if (!is.null(y)) {
		.restore(y)
	} else if (location() != startLoc | mapset() != startMapset) {
		fastStart(location = toLoc, mapset = toMapset)
	}
	
	flags <- c('quiet', 'overwrite')
	# if (geomtype(x) == 'points') flags <- c(flags, 'b') # makes faster but is this OK???
	
	gn <- .makeGname('projected', 'vector')
	
	args <- list(
		cmd = 'v.proj',
		location = location(x),
		mapset = mapset(x),
		input = gnames(x),
		output = gn,
		flags = flags,
		intern = TRUE
	)

	do.call(rgrass::execGRASS, args=args)
	out <- makeGVector(gn)

	if (location() != startLoc | mapset() != startMapset) fastRestore(location=startLoc, mapset=startMapset)
	
	out

	} # EOF
)
