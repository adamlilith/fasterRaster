#' Change the coordinate reference system of a GRaster or GVector
#'
#' @description `project()` changes the coordinate reference system (CRS) of a `GRaster` or `GVector`. It has three use cases:
#' * `x` is a `GRaster` and `y` is a `GRaster`: `x` will be projected to the CRS of `y` and resampled to have the same resolution as `y`. If argument `align` is `FALSE`, then it will also be cropped to the extent of `y`.
#' * `x` is a `GRaster` and `y` is a `GVector` or a CRS string (typically in Well-Known Text format): `x` will be projected to the CRS specified by `y` and resampled but not cropped.
#' * `x` is a `GVector` and `y` is a `GRaster`, `GVector`, or CRS string: The vector will be projected to the CRS of `y`.
#'
#' @param x A `GRaster` or `GVector` to be projected.
#'
#' @param y A character or `GLocation` object (i.e., typically a `GRaster` or `GVector`): Used to set the focal `GRaster` or `GVector`'s new CRS (and resolution and possibly extent, for `GRaster`s).
#'
#' @param align Logical: If `FALSE` (default), and `x` and `y` are `GRaster`s, then the extent of `x` will be cropped to the extent of `y`. If `TRUE`, no corpping is performed.
#'
#' @param method Character or `NULL` (for `GRaster`s only): Method to use to conduct the transformation (rasters only). Partial matching is used.
#' * `NULL` (default): Automatically choose based on raster properties (`near` for categorical data, `bilinear` for continuous data).
#' * `"near"`: Nearest neighbor. Best for categorical data, and often a poor choice for continuous data.  If [datatype()] is `integer`, this method will be used by default.
#' * `"bilinear"`: Bilinear interpolation (default for non-categorical data; uses weighted values from 4 cells).
#' * `"bicubic"`: Bicubic interpolation (uses weighted values from 16 cells).
#' * `"lanczos"`: Lanczos interpolation (uses weighted values from 25 cells).
#'
#' *Note #1*: If `x` and `y` are `GRaster`s, and `res = "terra"`, then the same `method` is used to resample `x` to the resolution of `y` before projecting `x`.
#' 
#' *Note #2*: Methods that use multiple cells will cause the focal cell to become `NA` if there is at least one cell with an `NA` in the cells it draws from. These `NA` cells can often be filled using the `fallback` argument.
#' 
#' @param fallback Logical (for projecting `GRaster`s only): If `TRUE` (default), then use "lower" resampling methods to fill in `NA` cells when a "higher" resampling method is used. For example, if `method = "bicubic"`, `NA` cells will be filled in using the `bilinear` method, except when that results in `NA`s, in which case the `near` method will be used. Fallback causes fewer cells to revert to `NA` values, so may be better at capturing complex "edges" (e.g., coastlines). Fallback does increase processing time because each "lower" method must be applied, then results merged. Fallback is not used if `method = "near"`.
#'
#' @param res Character (for projecting `GRaster`s only): Method used to set the resolution of a `GRaster` in the new CRS. This can be one of three options.  Partial matching is used and case ignored:
#' * `"terra"`: This method creates an output raster that is as close as possible in values and resolution to the one that [terra::project()] would create. However, for large rasters (i.e., many cells), this can fail because `terra::project()` encounters memory limits (it is used internally to create a template). This method resamples the focal raster in its starting CRS, then projects it to the destination CRS.
#' * `"template"`: This method can only be used of `y` is a `GRaster`. The output will have the same resolution as `y` and possibly the same extent (depending on the value of `align`). However, unlike the `"terra"` method, cell values will not necessarily be as close as possible to what [terra::project()] would generate (unless `method = "near"`). Unlike the `"terra"` method, this method does not resample the focal raster in its starting CRS before projecting. For large rasters it will be faster than the `"terra"` method (especially if `"method = "neear"`), and it should be less likely to fail because of memory limits.
#' * Two numeric values: Values for the new resolution (x- and y-dimensions).
#' * `"center"`: This method locates the centroid of the raster to be projected (in the same CRS as the original raster). It then creates four points north, south, east, and west of the centroid, each spaced one cell's width from the centroid. This set of points is then projected to the new CRS. The new cell size in the x-dimension will be the average of the distance between the east and west points from the centroid, and in the y-dimension the average from the centroid to the north and south points.
#' * `"fallback"` (default): This applies the `terra` method first, but if that fails, then tries `template`, then `center`. This process can take a long time for large rasters.
#' 
#' @param wrap Logical (for projecting `GRaster`s only): When projecting rasters that "wrap around" (i.e., whole-world rasters or rasters that have edges that actually circle around to meet on the globe), `wrap` should be `TRUE` to avoid removing rows and columns from the "edge" of the map. The default is `FALSE`.
#'
#' @details When projecting a raster, the "fallback" methods in **GRASS** module `r.import` are actually used, even though the `method` argument takes the strings specifying non-fallback methods. See the manual page for the `r.import` **GRASS** module.
#' 
#' @returns A `GRaster` or `GVector`.
#' 
#' @seealso [terra::project()], [sf::st_transform()], modules `r.proj`, `r.import`, and `v.proj` in **GRASS**
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
		y,
		align = FALSE,
		method = NULL,
		fallback = TRUE,
		res = "fallback",
		wrap = FALSE
	) {

	if (inherits(res, "character")) {
		res <- omnibus::pmatchSafe(res, c("fallback", "terra", "template", "center"), nmax = 1L)
		if (!inherits(y, "GRaster") && res == "template") stop("The ", sQuote("template"), " method for setting resolution can only be used if the argument ", sQuote("y"), " is a GRaster.")
	} else if (inherits(res, "numeric")) {
		if (length(res) == 1L) res <- c(res, res)
		if (length(res) != 2L) stop("Argument ", sQuote("res"), " must have one or two numeric values, or be a string.")
	} else {
		stop("Argument ", sQuote("res"), " must be a numeric vector or a string.")
	}
	
	if (inherits(res, "numeric")) {
	
		out <- .projectRaster(x = x, y = y, align = align, method = method, fallback = fallback, res = res, wrap = wrap)

	} else if (res != "fallback") {
	
		out <- .projectRaster(x = x, y = y, align = align, method = method, fallback = fallback, res = res, wrap = wrap)
	
	} else if (res == "fallback") {
	
		out <- FALSE
		j <- 0L
		n <- length(res)
		try <- c("terra", "template", "center")
		while (inherits(out, "logical") & j <= n) {
		
			j <- j + 1L
			thisRes <- try[j]

			out <- tryCatch(
				.projectRaster(x = x, y = y, align = align, method = method, fallback = fallback, res = thisRes, wrap = wrap),
				error = function(cond) FALSE
			)
		
		}
	
	}
	out

	} # EOF
)

#' @noRd
.projectRaster <- function(x, y, align, method, fallback, res, wrap) {

	.message(msg = "project_raster", message = "This function can produce erroneous results if the raster crosses a pole or the international date line.")
	
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

	### If y is a GRaster, reshape region in target location using y's extent/resolution.
	if (inherits(y, "GRaster")) {

		# user sets target cell size
		if (is.numeric(res)) {
		
			.regionNumericRes(x = x, y = y, align = align, res = res)
			
		# resample as per terra::project()
		} else if (res == "terra") {

			# Use a SpatRaster as template for resampling region. We do this so we can set the "region" resolution and extent correctly.
			extent <- ext(x, vector = TRUE)
			xRast <- matrix(NA_real_, nrow = nrow(x), ncol = ncol(x))
			xRast <- terra::rast(xRast, crs = crs(x), extent = extent)
			
			extent <- ext(y, vector = TRUE)
			yRast <- matrix(NA_real_, nrow = nrow(y), ncol = ncol(y))
			yRast <- terra::rast(yRast, crs = crs(y), extent = extent)

			xRast <- terra::project(xRast, yRast, align = TRUE)
			xRast <- terra::project(xRast, crs(x))
			
			xSR <- xRast

			xRast[] <- 1L
			xRast <- fast(xRast)

			# resample x in its native location to the resolution it will have in the target location
			x <- resample(
				x = x,
				y = xRast,
				method = method,
				fallback = fallback
			)

			xRast <- terra::project(xSR, yRast, method = "near", align = align)

			# reshape region
			.locationRestore(yLocation)
			.region(xRast)

		# } else if (res == "dimensions") {
		
			# .regionRespectsDims(x = x, y = y, align = align)
		 
		# same extent and resolution as terra, but without the resampling before projection
		} else if (res == "template") {
		
			if (!align) {
			
				# reshape region
				.locationRestore(y)
				.region(y)
				
			} else {
			
				# extent from x projected to y
				n <- N(x)
				s <- S(x)
				e <- E(x)
				w <- W(x)
				
				corners <- rbind(
					c(w, n),
					c(w, s),
					c(e, s),
					c(e, n)
				)
				
				corners <- terra::vect(corners, type = "points", crs = crs(x))
				corners <- terra::project(corners, crs(y))
				corners <- terra::crds(corners)
				
				n <- max(corners[c(1L, 4L), 2L])
				s <- min(corners[c(2L, 3L), 2L])
				e <- max(corners[c(3L, 4L), 1L])
				w <- min(corners[c(1L, 2L), 1L])
			
				ewres <- xres(y)
				nsres <- yres(y)
				
				yextent <- ext(y, vector = TRUE)
				
				if (n > yextent[4L]) {
				
					diff <- n - yextent[4L]
					nc <- ceiling(diff / nsres)
					n <- yextent[4L] + nc * nsres
					
				}
				
				if (s < yextent[3L]) {
				
					diff <- yextent[3L] - s
					nc <- ceiling(diff / nsres)
					s <- yextent[3L] - nc * nsres
					
				}
				
				if (e > yextent[2L]) {
				
					diff <- e - yextent[2L]
					nc <- ceiling(diff / ewres)
					e <- yextent[2L] + nc * ewres
					
				}
			
				if (w < yextent[1L]) {
				
					diff <- yextent[1L] - w
					nc <- ceiling(diff / ewres)
					w <- yextent[1L] - nc * ewres
					
				}
				
				rows <- round((y - s) / nsres)
				cols <- round((e - w) / ewres)

				n <- as.character(n)
				s <- as.character(s)
				e <- as.character(e)
				w <- as.character(w)
				
				# reshape region
				.locationRestore(y)
				rgrass::execGRASS(
					cmd = "g.region",
					n = n, s = s, e = e, w = w,
					rows = rows, cols = cols,
					flags = c("o", .quiet())
				)
				
			}

		# "center" method of determining cell resolution
		} else if (res == "center") {
		
			.regionResCenterMethod(x = x, y = y, align = align)
		
		}

	### "y" is not a raster (i.e., WKT string or GVector)
	} else {

		# user-supplied resolution
		if (inherits(res, "numeric")) {

			.regionNumericRes(x = x, y = y, align = TRUE, res = res)

		# project as per terra::project()
		} else if (res == "terra") {

			# make template raster to match raster to be projected
			extent <- ext(x, vector = TRUE)
			xRast <- matrix(NA_integer_, nrow = nrow(x), ncol = ncol(x))
			xSR <- terra::rast(xRast, crs = crs(x), extent = extent)
			
			xRast <- terra::project(xSR, crs(y), method = "near", align = align)

			# reshape region
			.locationRestore(yLocation)
			.region(xRast)

		# # respect dimensions
		# } else if (res == "dimensions") {
		
			# .regionRespectsDims(x = x, y = y, align = align)

		# "center" method of determining cell resolution
		} else if (res == "center") {
		
			.regionResCenterMethod(x = x, y = y, align = align)
		
		}

	}

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
		if (is.factor(x)[i] & method == "nearest") {
			levels <- cats(x)[[i]]
		} else {
			levels <- NULL
		}
		thisOut <- .makeGRaster(srcs[i], names(x)[i], levels = levels, ac = activeCat(x, layer = i))
		if (i == 1L) {
			out <- thisOut
		} else {
			out <- c(out, thisOut)
		}

	} # project next raster

	# if using y as extent to which to crop
	if (!align & inherits(y, "GRaster")) out <- crop(out, y)
	out

}

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

#' Set region in target location so it has user-defined resolution
#'
#' @param x A `GRaster`.
#' @param y `GRaster` or `GVector`
#' @param align Logical
#'
#' @noRd
.regionNumericRes <- function(x, y, align, res) {

	if (!align) {
	
		# extent from y
		n <- N(y)
		s <- S(y)
		e <- E(y)
		w <- W(y)
	
	} else {
	
		# extent from x projected to y
		n <- N(x)
		s <- S(x)
		e <- E(x)
		w <- W(x)
		
		corners <- rbind(
			c(w, n),
			c(w, s),
			c(e, s),
			c(e, n)
		)
		
		corners <- terra::vect(corners, type = "points", crs = crs(x))
		corners <- terra::project(corners, crs(y))
		corners <- terra::crds(corners)
		
		n <- max(corners[c(1L, 4L), 2L])
		s <- min(corners[c(2L, 3L), 2L])
		e <- max(corners[c(3L, 4L), 1L])
		w <- min(corners[c(1L, 2L), 1L])
	
	}
	
	cols <- ceiling((e - w) / res[1L])
	rows <- ceiling((n - s) / res[2L])
	
	e <- w + cols * res[1L]
	s <- n - rows * res[2L]

	n <- as.character(n)
	s <- as.character(s)
	e <- as.character(e)
	w <- as.character(w)

	# reshape region
	.locationRestore(y)
	rgrass::execGRASS(
		cmd = "g.region",
		n = n, s = s, e = e, w = w,
		rows = rows, cols = cols,
		flags = c("o", .quiet())
	)
	
}

# #' Set region in target location so it has same dimensions (rows, columns) as focal raster
# #'
# #' @param x `GRaster` to project
# #' @param y `GRaster` or `GVector`.
# #' @param align Logical.
# #'
# #' @noRD
# .regionRespectsDims <- function(x, y, align) {

	# # get new extent in y location
	# if (!align) {
		# extent <- ext(y, vector = TRUE)
	# } else {

		# # get new extent from projected corners of x raster
		# n <- N(x)
		# s <- S(x)
		# e <- E(x)
		# w <- W(x)
		
		# corners <- rbind(
			# c(w, n),
			# c(w, s),
			# c(e, s),
			# c(e, n)
		# )

		# if (inherits(y, "GLocation")) {
			# yCrs <- crs(y)
		# } else {
			# yCrs <- y
		# }

		# corners <- terra::vect(corners, type = "points", crs = crs(x))
		# corners <- terra::project(corners, yCrs)
		# corners <- terra::crds(corners)

		# extent <- c(
			# xmin = min(corners[1L:2L, 1L]),
			# xmax = max(corners[2L:3L, 1L]),
			# ymin = min(corners[2L:3L, 2L]),
			# ymax = max(corners[c(1L, 4L), 2L])
		# )

	# }

	# xdim <- dim(x)
	# rows <- xdim[1L]
	# cols <- xdim[2L]

	# n <- as.character(extent[4L])
	# s <- as.character(extent[3L])
	# e <- as.character(extent[2L])
	# w <- as.character(extent[1L])

	# # reshape region
	# .locationRestore(y)
	# rgrass::execGRASS(
		# cmd = "g.region",
		# n = n, s = s, e = e, w = w,
		# rows = rows, cols = cols,
		# flags = c("o", .quiet())
	# )

# }

#' Set target region resolution using "center" method
#'
#' Set target region resolution using "center" method. Extent will be from y or projected corners of x
#' 
#' @param x `GRaster`.
#' @param x `GRaster` or `GVector`.
#' @param align Logical.
#'
#' @noRd
.regionResCenterMethod <- function(x, y, align) {

	extent <- ext(x, vector = TRUE)

	# get resolution in location of y
	center <- c(mean(extent[1L:2L]), mean(extent[3L:4L]))

	xres <- res(x)
	wp <- c(center[1L] - xres[1L], center[2L])
	ep <- c(center[1L] + xres[1L], center[2L])
	np <- c(center[1L], center[2L] + xres[2L])
	sp <- c(center[1L], center[2L] - xres[2L])

	pts <- rbind(center, wp, ep, np, sp)
	pts <- terra::vect(pts, type = "points", crs = crs(x))
	pts <- terra::project(pts, crs(y))
	pts <- terra::crds(pts)

	centerx <- pts[1L, 1L]
	centery <- pts[1L, 2L]
	ewres <- mean(c(centerx - pts[2L, 1L], pts[3L, 1L] - centerx))
	nsres <- mean(c(pts[4L, 2L] - centery, centery - pts[5L, 2L]))

	# get extent in y location
	if (!align & inherits(y, "GRegion")) {
		
		# extent from y
		yextent <- ext(y, vector = TRUE)
		n <- N(y)
		s <- S(y)
		e <- E(y)
		w <- W(y)
	
	} else {
		
		# extent by projecting corners of x to y location
		corners <- rbind(
			extent[c(1L, 4L)],
			extent[c(1L, 3L)],
			extent[c(2L, 3L)],
			extent[c(2L, 4L)]
		)

		corners <- terra::vect(corners, type = "points", crs = crs(x))
		corners <- project(corners, crs(y))
		corners <- terra::crds(corners)

		n <- max(corners[c(1L, 4L), 2L])
		s <- min(corners[c(2L, 3L), 2L])
		e <- max(corners[c(3L, 4L), 1L])
		w <- min(corners[c(1L, 2L), 1L])

	}

	cols <- round((e - w) / ewres)
	rows <- round((n - s) / nsres)

	n <- as.character(n)
	s <- as.character(s)
	e <- as.character(e)
	w <- as.character(w)

	# reshape region
	if (inherits(y, "GSpatial")) .locationRestore(y)
	rgrass::execGRASS(
		cmd = "g.region",
		n = n, s = s, e = e, w = w,
		rows = rows, cols = cols,
		flags = c("o", .quiet())
	)

}
