#' Change the coordinate reference system of a GRaster or GVector
#'
#' @description `project()` changes the coordinate reference system (CRS) of a `GRaster` or `GVector`. It has three use cases:
#' * `x` is a `GRaster` and `y` is a `GRaster`: `x` will be projected to the CRS of `y` and resampled to have the same resolution as `y`. If argument `align` is `FALSE`, then it will also be cropped to the extent of `y`.
#' * `x` is a `GRaster` and `y` is a `GVector` or a CRS string (typically in WKT format): `x` will be projected to the CRS specified by `y` but not resampled or cropped.
#' * `x` is a `GVector` and `y` is a `GRaster`, `GVector`, or CRS string: The vector will be projected to the CRS of `y`.
#'
#' @param x A `GRaster` or `GVector` to be projected.
#'
#' @param y A character or `GLocation` object (i.e., typically a `GRaster` or `GVector`):
#'
#' @param align Logical: If `FALSE` (default), and `x` and `y` are `GRaster`s, then the extent of `x` will be cropped to the extent of `y`.
#'
#' @param method Character or `NULL` (`GRaster`s only): Method to use to conduct the transformation (rasters only). Partial matching is used.
#' * `NULL` (default): Automatically choose based on raster properties (`near` for categorical data, `bilinear` for continuous data).
#' * `"near"`: Nearest neighbor. Best for categorical data, and often a poor choice for continuous data.  If [datatype()] is `integer`, this method will be used by default.
#' * `"bilinear"`: Bilinear interpolation (default for non-categorical data; uses weighted values from 4 cells).
#' * `"bicubic"`: Bicubic interpolation (uses weighted values from 16 cells).
#' * `"lanczos"`: Lanczos interpolation (uses weighted values from 25 cells).
#'
#' *Note #1*: If `x` and `y` are `GRaster`s, then the same `method` is used to resample `x` to the resolution of `y` before projecting `x`.
#' 
#' *Note #2*: Methods that use multiple cells will cause the focal cell to become `NA` if there is at least one cell with an `NA` in the cells it draws from. These `NA` cells can often be filled using the `fallback` option.
#' 
#' @param fallback Logical (projecting `GRaster`s only): If `TRUE` (default), then use "lower" methods to fill in `NA` cells when a "higher" method is used. For example, if `method = "bicubic"`, `NA` cells will be filled in using the `bilinear` method, except when that results in `NA`s, in which case the `near` method will be used. Fallback causes fewer cells to revert to `NA` values, so may be better at capturing complex "edges" (e.g., coastlines). Fallback does increase processing time because each "lower" method must be applied, then results merged. Fallback is not used if `method = "near"`.
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
	f = "project",
	signature = c(x = "GRaster"),
	definition = function(
		x,
		y = NULL,
		align = FALSE,
		method = NULL,
		fallback = TRUE,
		wrap = FALSE
	) {

	xLocation <- .location(x)
	yLocation <- .locationFind(y, match = "crs", return = "name")

	if (!is.null(yLocation)) {
	
		if (yLocation == xLocation) {
			warning("Object is already in the desired coordinate reference system.")
			return()
		}
	
	} else if (is.null(yLocation)) {
		yLocation <- .locationCreate(y)
		yLocation <- .location(yLocation)
	}
	
	# method
	if (!is.null(method)) method <- omnibus::pmatchSafe(method, c("nearest", "bilinear", "bicubic", "lanczos"))

	if (is.null(method)) {
		
		dt <- datatype(x)
		method <- if (all(dt %in% c("integer", "factor"))) {
			"nearest"
		} else if (all(dt %in% c("float", "double"))) {
			"bilinear"
		} else {
			stop("Rasters are a mix of datatype integer and non-integer (categorical and continuous).  \nProject each type of raster separately.")
		}

	}

	### If y is a GRaster, resample x in its native location first, then project.
	if (inherits(y, "GRaster")) {

		### resample 1st

		# Use a SpatRaster as template for resampling region. We do this so we can set the "region" resolution and extent correctly.
		extent <- ext(x, vector = TRUE)
		xMat <- matrix(NA_real_, nrow = nrow(x), ncol = ncol(x))
		xRast <- terra::rast(xMat, crs = crs(x), extent = extent)

		extent <- ext(y, vector = TRUE)
		yMat <- matrix(NA_real_, nrow = nrow(y), ncol = ncol(y))
		yRast <- terra::rast(yMat, crs = crs(y), extent = extent)

		yRast <- terra::project(yRast, crs(x), method = "near")
		xRast <- terra::resample(xRast, yRast, method = "near")

		# resample x in its native location to the resolution it will have in the target location
		x <- resample(
			x = x,
			y = terra::res(xRast),
			method = method,
			fallback = fallback
		)

	### "y" is not a raster (no resampling)
	} else {

		# make template raster to match raster to be projected
		extent <- ext(x, vector = TRUE)
		xMat <- matrix(NA_integer_, nrow = nrow(x), ncol = ncol(x))
		xRast <- terra::rast(xMat, crs = crs(x), extent = extent)

	}

	### reshape region in "to" location
	###################################

	if (inherits(y, "GLocation")) {
		yCrs <- crs(y)
	} else {
		yCrs <- y
	}

	xRast <- terra::project(xRast, yCrs, method = "near", align = TRUE)
	.locationRestore(yLocation)
# cat("Slow step!")
	.region(xRast)
# cat("Slow step^^^")

	### project raster
	##################

	if (method != "nearest" & fallback) method <- paste0(method, "_f")

	srcs <- .makeSourceName(names(x), "raster", nlyr(x))
	for (i in seq_len(nlyr(x))) {
		
		args <- list(
			cmd = "r.proj",
			location = .location(x),
			mapset = .mapset(x),
			input = sources(x)[i],
			output = srcs[i],
			method = method,
			memory = faster("memory"),
			flags = c(.quiet(), "overwrite")
		)

		if (wrap) args$flags <- c(args$flags, "n")

		do.call(rgrass::execGRASS, args=args)
		thisOut <- .makeGRaster(srcs[i], names(x)[i])
		if (i == 1L) {
			out <- thisOut
		} else {
			out <- c(out, thisOut)
		}

	} # project next raster

	# if using y as extent to which to crop
	if (!align & inherits(y, "GRaster")) out <- crop(out, y)
	out

	} # EOF
)

#' @aliases project
#' @rdname project
#' @exportMethod project
methods::setMethod(
	f = "project",
	signature = c(x = "GVector"),
	definition = function(
		x,
		y = NULL
	) {

	xLocation <- .location(x)
	yLocation <- .locationFind(y, match = "crs", return = "name")

	if (!is.null(yLocation)) {
	
		if (yLocation == xLocation) {
			warning("Object is already in the desired coordinate reference system.")
			return()
		}
	
	} else if (is.null(yLocation)) {
		yLocation <- .locationCreate(y)
		yLocation <- .location(yLocation)
	}

	.locationRestore(yLocation)
	src <- .makeSourceName("projected", "vector")
	
	args <- list(
		cmd = "v.proj",
		location = .location(x),
		mapset = .mapset(x),
		input = sources(x),
		output = src,
		flags = c(.quiet(), "overwrite"),
		intern = TRUE
	)

	do.call(rgrass::execGRASS, args = args)
	out <- .makeGVector(src, table = x@table)
	out

	} # EOF
)
