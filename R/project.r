#' Change the coordinate reference system of a GRaster or GVector
#'
#' @description *fasterRaster** offers three ways to project rasters and vectors into a different coordinate reference system (CRS). Each of these ways are different from how they are implemented in **terra**, so it is important to understand how it works in **fasterRaster**.
#'
#' First, if you are wanting to change the CRS for a `SpatRaster`, `SpatVector`, or `sf` object already in `R`, use the [fast()] function. This function will project the object to the CRS of the current **GRASS** [location][tutorial_sessions]. Note that for rasters, the `method` and `fallback` arguments in the `fast()` function may be important to what you want to achieve.
#' 
#' Second, the procedure works the same way for rasters or vectors stored on disk (and not already in **R**). Simply use the [fast()] function with the file name of the object. Again, the `method` and `fallback` arguments may be important .
#'
#' Third, if you are wanting to change the CRS of a `GRaster` or `GVector`, you need to initiate a new **GRASS** [location][tutorial_sessions] using [faster()] with the CRS to which you want to transform the object. This means you need to keep track of which **GRASS** "location" you are working in, as multiple rasters and/or vectors can only be used in the function if they are in the same "location."  *This function, `project()`, is for projecting `GRaster`s or `GVector`s between **GRASS** "locations."* [location()] (with no arguments) will display the name of the currently active "location", and [crs()] (alo no arguments) will display its CRS.
#'
#' @param x A `GRaster` or `GVector` to be projected.
#'
#' @param y `NULL` (default), or a `GSession` object or any that contain it (i.e., a `GRaster`, `GVector`, or `GRegion`):
#' * If `y` is `NULL`, then `x` is projected to the CRS of the current "[location][tutorial_sessions]" (i.e., `x` should not be in the current "location"). If `x` is a `GRaster`, it will not be resampled to a different resolution.
#' * If `x` is a `GRaster` and `y` is a `GRaster` or `GRegion`, then `x` will be projected to the CRS of `y` and resampled to the same resolution as `y`. `x` will be cropped to the extent of `y` if `align` is `TRUE`. `x` should not be the same [location][tutorial_sessions] as `y`.
#' * If `x` is a `GRaster` and `y` is a `GVector`, then `x` will be projected to the CRS of `y`. `x` will not be resampled. `x` should not be the same "[location][tutorial_sessions]" as `y`.
#' * If `x` is a `GVector`, then `x` will be projected to the CRS of `y`.
#' 
#' @param align Logical: If `FALSE` (default), and `x` and `y` are `GRaster`s, then the extent of `x` will be cropped to the extent of `y`.
#'
#' @param method Character or `NULL` (`GRaster`s only): Method to use to conduct the transformation (rasters only). Partial matching is used.
#' * `NULL` (default): Automatically choose based on raster properties (`near` for categorical data, `bilinear` for continuous data).
#' * `'near'`: Nearest neighbor. Best for categorical data, and often a poor choice for continuous data.  If [datatype()] is `CELL`, this method will be used by default.
#' * `'bilinear'`: Bilinear interpolation (default for non-categorical data; uses weighted values from 4 cells).
#' * `'bicubic'`: Bicubic interpolation (uses weighted values from 16 cells).
#' * `'lanczos'`: Lanczos interpolation (uses weighted values from 25 cells).
#'
#' *Note #1*: If `x` and `y` are `GRaster`s, then the same `method` is used to resample `x` to the resolution of `y` before projecting `x`.
#' 
#' *Note #2*: Methods that use multiple cells will cause the focal cell to become `NA` if there is at least one cell with an `NA` in the cells it draws from. These `NA` cells can often be filled using the `fallback` option.
#' 
#' @param fallback Logical (projecting `GRaster`s only): If `TRUE` (default), then use "lower" methods to fill in `NA` cells when a "higher" method is used. For example, if `method = 'bicubic'`, `NA` cells will be filled in using the `bilinear` method, except when that results in `NA`s, in which case the `near` method will be used. Fallback causes fewer cells to revert to `NA` values, so may be better at capturing complex "edges" (e.g., coastlines). Fallback does increase processing time because each "lower" method must be applied, then results merged. Fallback is not used if `method = 'near'`.
#' 
#' @param location,mapset Character or `NULL` (default): The name of the [location and mapset][tutorial_sessions] which have the CRS you to which you want to transform the `GRaster` or `GVector`. You can get the current location and mapset or the location and mapset of an object using [location()] and [mapset()]. If left as `NULL`, then it will be assumed that you want to project the object to the currently active location and mapset.
#'
#' @param wrap Logical (`GRaster`s only): When projecting rasters that "wrap around" (i.e., whole-world rasters or rasters that have edges that actually circle around to meet on the globe), `wrap` should be `TRUE` to avoid removing rows and columns from the "edge" of the map. The default is `FALSE`.
#'
#' @details When projecting a raster, the "fallback" methods in **GRASS** module `r.import` are actually used, even though the `method` argument takes the strings specifying non-fallback methods. See the manual page for the `r.import` **GRASS** module.
#' 
#' @returns A `GRaster` or `GVector`.
#' 
#' @seealso [terra::project()], [sf::st_transform()], modules `r.proj` and `v.proj` in **GRASS**
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
		align = FALSE,
		method = NULL,
		fallback = TRUE,
		wrap = FALSE,
		toLocation = NULL,
		toMapset = NULL
	) {

	# start location and mapset
	startLoc <- location()
	startMapset <- mapset()

	on.exit(fastRestore(location=startLoc, mapset=startMapset), add=TRUE)
	
	# target location and mapset
	if (is.null(y)) {
		toLocation <- location()
		toMapset <- mapset()
	} else {
		toLocation <- location(y)
		toMapset <- mapset(y)
	}
	
	if (toLocation == location(x) & toMapset == mapset(x)) {
		warning('Object is already in the desired coordinate reference system.')
		return()
	}
	
	if (!is.null(y)) {
		toCrs <- crs(y)
	} else {
		fastRestore(location=toLocation, mapset=toMapset)
		toCrs <- crs()
	}

	# method
	if (!is.null(method)) method <- pmatchSafe(method, c('nearest', 'bilinear', 'bicubic', 'lanczos'))

	if (is.null(method)) {
		
		dt <- datatype(x)
		method <- if (all(dt == 'CELL')) {
			'nearest'
		} else if (all(dt %in% c('DCELL', 'FCELL'))) {
			'bilinear'
		} else {
			stop('Rasters are a mix of datatype CELL and non-CELL (categorical and continuous).  \nProject each type of raster separately.')
		}

	}

	### If y is a GRaster, resample x in its native location first, then project.
	if (inherits(y, 'GRaster')) {

		### resample 1st

		# Use a SpatRaster as template for resampling region. We do this so we can set the "region" resolution and extent correctly.
		extent <- ext(x, vector=TRUE)
		xMat <- matrix(NA_real_, nrow=nrow(x), ncol=ncol(x))
		xRast <- terra::rast(xMat, crs=crs(x), extent=extent)

		extent <- ext(y, vector=TRUE)
		yMat <- matrix(NA_real_, nrow=nrow(y), ncol=ncol(y))
		yRast <- terra::rast(yMat, crs=crs(y), extent=extent)

		yRast <- terra::project(yRast, crs(x), method='near')
		xRast <- terra::resample(xRast, yRast, method='near')

		# resample x in its native location to the resolution it will have in the target location
		.restore(x)
		x <- resample(
			x = x,
			y = terra::res(xRast),
			method = method,
			fallback = fallback
		)

		fastRestore(location=toLocation, mapset=toMapset)

	### "y" is not a raster (no resampling)
	} else {

		# make template raster to match raster to be projected
		extent <- ext(x, vector=TRUE)
		xMat <- matrix(NA_real_, nrow=nrow(x), ncol=ncol(x))
		xRast <- terra::rast(xMat, crs=crs(x), extent=extent)

	}

	### reshape region in "to" location
	###################################

	xRast <- terra::project(xRast, toCrs, method='near', align=TRUE)
	region(xRast)

	### project raster
	##################

	if (method != 'nearest' & fallback) method <- paste0(method, '_f')

	gns <- .makeGname(names(x), 'raster', nlyr(x))
	for (i in 1L:nlyr(x)) {
		
		args <- list(
			cmd = 'r.proj',
			location = location(x),
			mapset = mapset(x),
			input = gnames(x)[i],
			output = gns[i],
			method = method,
			memory = getFastOptions('memory'),
			flags = c('quiet', 'overwrite')
		)

		if (wrap) args$flags <- c(args$flags, 'n')

		do.call(rgrass::execGRASS, args=args)
		thisOut <- makeGRaster(gns[i], names(x)[i])
		if (i == 1L) {
			out <- thisOut
		} else {
			out <- c(out, thisOut)
		}

	} # project next raster

	# if using y as extent to which to crop
	if (!align & inherits(y, 'GRaster')) out <- crop(out, y)
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
		if (is.null(location)) {
			toLocation <- location()
		}
		if (is.null(mapset)) {
			toMapset <- mapset()
		}
	} else {
		toLocation <- location(y)
		toMapset <- mapset(y)
	}
	
	if (toLocation == location(x) & toMapset == mapset(x)) {
		warning('Object is already in the desired coordinate reference system.')
		return()
	}

	# go to "to" location	
	on.exit(fastRestore(location=startLocation, mapset=startMapset), add=TRUE)

	if (!is.null(y)) {
		.restore(y)
	} else if (location() != startLoc | mapset() != startMapset) {
		fastRestore(location = toLocation, mapset = toMapset)
	}
	
	gn <- .makeGname('projected', 'vector')
	
	args <- list(
		cmd = 'v.proj',
		location = location(x),
		mapset = mapset(x),
		input = gnames(x),
		output = gn,
		flags = c('quiet', 'overwrite'),
		intern = TRUE
	)

	do.call(rgrass::execGRASS, args=args)
	out <- makeGVector(gn)

	out

	} # EOF
)
