#' Change the cell size of a GRaster
#'
#' @description `resample()` changes the cell size (resolution) of a `GRaster` using either another raster as a template or a user-defined resolution. Note that the extent of the output raster may be expanded to accommodate an integer number of cells. The function is not guaranteed to recreate the same output as [terra::resample()], even when the same resampling method is used.
#'
#' @param x The `GRaster` to resample.
#'
#' @param y Either a `GRaster` to serve as a template, or a numeric vector with two or three values. If a numeric vector, the values represent east-west and north-south resolution for 2D rasters, or east-west, north-south, and top-bottom resolution for 3D rasters.
#'
#' @param method Character or `NULL`: Method to use to assign values to cells. Partial matching is used.
#' * `NULL` (default): Automatically choose based on raster properties (`near` for categorical or integer rasters, `bilinear` for continuous data).
#' * `"near"`: Nearest neighbor. Best for categorical data, and often a poor choice for continuous data.  If [nlevels()] is >0, this method will be used regardless of the value of `method`. If you still want to use a different method, coerce the raster to a different type using [as.int()], [as.float()], or [as.doub()].
#' * `"bilinear"`: Bilinear interpolation (default for non-categorical data; uses weighted values from 4 cells).
#' * `"bicubic"`: Bicubic interpolation (uses weighted values from 16 cells).
#' * `"lanczos"`: Lanczos interpolation (uses weighted values from 25 cells).
#' Note that methods that use multiple cells will cause the focal cell to become `NA` if there is at least one cell with an `NA` in the cells it draws from. These `NA` cells can be filled using the `fallback` option.
#' 
#' @param fallback Logical: If `TRUE` (default), then use "lower" methods to fill in `NA` cells when a "higher" method is used. For example, if `method = "bicubic"`, `NA` cells will be filled in using the `bilinear` method, except when that results in `NA`s, in which case the `near` method will be used. Fallback causes fewer cells to revert to `NA` values, so can be better at resampling the edges of rasters. However, fallback does increase processing time because each "lower" method must be applied, then results merged.
#'
#' @returns A `GRaster`.
#' 
#' @seealso [terra::resample()], **GRASS** modules `r.resample` and `r.resamp.interp` (see `grassHelp("`r.resample`") and `grassHelp("`r.resamp.interp`")`)
#'
#' @example man/examples/ex_resample.r
#'
#' @aliases resample 
#' @rdname resample 
#' @exportMethod resample
methods::setMethod(
	f = "resample",
	signature = c(x = "GRaster", y = "GRaster"),
	definition = function(x, y, method = NULL, fallback = TRUE) .resample(x = x, y = y, method = method, fallback = fallback)
)

#' @aliases resample 
#' @rdname resample 
#' @exportMethod resample
methods::setMethod(
	f = "resample",
	signature = c(x = "GRaster", y = "numeric"),
	definition = function(x, y, method = NULL, fallback = TRUE) .resample(x=x, y=y, method=method, fallback=fallback)
)

#' @noRd
.resample <- function(x, y, method, fallback) {

	# method
	if (!is.null(method)) method <- omnibus::pmatchSafe(method, c("nearest", "bilinear", "bicubic", "lanczos"))

	versionNumber <- grassInfo("versionNumber")

	# reshape region
	if (inherits(y, "GRaster")) {

		ewres <- xres(y)
		nsres <- yres(y)
		zres <- zres(y)
		
		if (is.3d(x) & is.3d(y)) {
			t <- top(y)
			b <- bottom(y)
			tbres <- zres(y)
		} else {
			tbres <- t <- b <- NA_real_
		}
### NEED TO EXTENT TO ENCOMPASS x IF ITS EXTENT IS OUTSIDE THIS		
		w <- W(y)
		e <- E(y)
		s <- S(y)
		n <- N(y)

	} else {
	
		ewres <- y[1L]
		nsres <- y[2L]
		tbres <- if (length(y) == 3L) {
			y[3L]
		} else {
			NA_real_
		}

		t <- top(x)
		b <- bottom(x)
	
		w <- W(x)
		e <- E(x)
		s <- S(x)
		n <- N(x)

	}
	
	cols <- (e - w) / ewres
	rows <- (n - s) / nsres

	if (!omnibus::is.wholeNumber(cols)) cols <- ceiling(cols)
	if (!omnibus::is.wholeNumber(rows)) cols <- ceiling(rows)

	e <- w + cols * ewres
	s <- n - rows * nsres

	w <- as.character(w)
	e <- as.character(e)
	s <- as.character(s)
	n <- as.character(n)
	t <- as.character(t)
	b <- as.character(b)

	ewres <- as.character(ewres)
	nsres <- as.character(nsres)
	tbres <- as.character(tbres)

	args <- list(
		cmd = "g.region",
		n = n, s = s, e = e, w = w,
		ewres = ewres, nsres = nsres,
		flags = c("o", .quiet())
	)
	if (!is.na(t)) args <- c(args, t = t)
	if (!is.na(b)) args <- c(args, t = b)
	if (!is.na(tbres)) args <- c(args, tbres = tbres)

	do.call(rgrass::execGRASS, args=args)
	
	# resample each layer
	for (i in 1L:nlyr(x)) {

		# resampling method
		if (is.null(method)) {
			thisMethod <- if (datatype(x, type = "GRASS")[i] == "CELL") {
				"nearest"
			} else {
				"bilinear"
			}
		} else if (is.factor(x)[i]) {
			thisMethod <- "nearest"
		} else {
			if (method == "near") {
				thisMethod <- "nearest"
			} else {
				thisMethod <- method
			}
		}
		
		### resample
		if (thisMethod == "nearest" | fallback) {

			srcNearest <- .makeSourceName("resample", "raster")
			rgrass::execGRASS(
				cmd = "r.resample",
				input = sources(x)[i],
				output = srcNearest,
				flags = c(.quiet(), "overwrite")
			)

		}

		if (thisMethod == "bilinear" | (fallback & thisMethod %in% c("bicubic", "lanczos"))) {

			srcBilinear <- .makeSourceName("resample_bilinear", "raster")
			args <- list(
				cmd = "r.resamp.interp",
				input = sources(x)[i],
				output = srcBilinear,
				method = "bilinear",
				memory = faster("memory"),
				flags = c(.quiet(), "overwrite")
			)
			if (versionNumber >= 8.4) args$nprocs <- faster("cores")
			do.call(rgrass::execGRASS, args = args)

		}

		if (thisMethod == "bicubic" | (fallback & thisMethod == "lanczos")) {

			srcBicubic <- .makeSourceName("resample_bicubic", "raster")
			args <- list(
				cmd = "r.resamp.interp",
				input = sources(x)[i],
				output = srcBicubic,
				method = "bicubic",
				memory = faster("memory"),
				flags = c(.quiet(), "overwrite")
			)
			# nprocs became available with GRASS eight point three
			if (versionNumber >= 8.3) args$nprocs <- faster("cores")
			do.call(rgrass::execGRASS, args = args)

		}

		if (thisMethod == "lanczos") {

			srcLanczos <- .makeSourceName("r_resamp_lanczos", "raster")
			args <- list(
				cmd = "r.resamp.interp",
				input = sources(x)[i],
				output = srcLanczos,
				method = "lanczos",
				memory = faster("memory"),
				flags = c(.quiet(), "overwrite")
			)
			# nprocs became available with GRASS eight point three
			if (versionNumber > 8.3) args$nprocs <- faster("cores")
			do.call(rgrass::execGRASS, args = args)

		}

		### output/fallback
		if (thisMethod == "nearest") {
			if (is.factor(x)[i]) {
				cats <- cats(x)[[i]]
				ac <- activeCat(x, layer = i)
			} else {
				cats <- ac <- NULL
			}
			thisOut <- .makeGRaster(srcNearest, names(x)[i], levels = cats, ac = ac)
		} else if (thisMethod == "bilinear" & !fallback) {
			thisOut <- .makeGRaster(srcBilinear, names(x)[i])
		} else if (thisMethod == "bicubic" & !fallback) {
			thisOut <- .makeGRaster(srcBicubic, names(x)[i])
		} else if (thisMethod == "lanczos" & !fallback) {
			thisOut <- .makeGRaster(srcLanczos, names(x)[i])
		} else if (fallback) {
			
			src <- .makeSourceName("resample_fallback", "raster")
			if (thisMethod == "bilinear") {

				# merge bilinear and nearest
				ex <- paste0(src, " = if(!isnull(", srcBilinear, "), ", srcBilinear, ", ", srcNearest, ")")
				on.exit(.rm(c(srcBilinear, srcNearest), type = "raster"), add = TRUE)

			} else if (thisMethod == "bicubic") {
			
				# merge bicubic, bilinear, and nearest
				ex <- paste0(src, " = if(!isnull(", srcBicubic, "), ", srcBicubic, ", if(!isnull(", srcBilinear, "), ", srcBilinear, ", ", srcNearest, "))")
				on.exit(.rm(c(srcBicubic, srcBilinear, srcNearest), type = "raster"), add = TRUE)
				
			} else if (thisMethod == "lanczos") {

				# merge bicubic, bilinear, and nearest
				ex = paste0(src, " = if(!isnull(", srcLanczos, "), ", srcLanczos, ", if(!isnull(", srcBicubic, "), ", srcBicubic, ", if(!isnull(", srcBilinear, "), ", srcBilinear, ", ", srcNearest, ")))")
				on.exit(.rm(c(srcLanczos, srcBicubic, srcBilinear, srcNearest), type = "raster"), add = TRUE)

			}

			args <- list(
				cmd = "r.mapcalc",
				expression = ex,
				flags = c(.quiet(), "overwrite")
			)
			do.call(rgrass::execGRASS, args=args)
			thisOut <- .makeGRaster(src, names = names(x)[i])

		} # next layer

		if (i == 1L) {
			out <- thisOut
		} else {
			out <- c(out, thisOut)
		}

	} # next layer
	out

}
