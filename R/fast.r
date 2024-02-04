#' Create a GRaster or GVector
#'
#' @description `fast()` creates a `GRaster` or `GVector` from a file, or from a `SpatRaster`, `SpatVector`, or `sf` vector. This function will also create a connection to **GRASS** the given coordinate reference system of the raster or vector if none has been made yet.
#'
#' **GRASS** supports loading from disk a variety of raster formats (see the **GRASS** manual page for [`r.in.gdal`](https://grass.osgeo.org/grass84/manuals/r.in.gdal.html)) and vector formats (see the **GRASS** manual page for [`v.in.ogr`](https://grass.osgeo.org/grass84/manuals/v.in.ogr.html)), though not all of them will work with this function.
#'
#' @param x Any one of:
#' * A `SpatRaster` raster. Rasters can have one or more layers.
#' * A `SpatVector` or `sf` spatial vector.
#' * A character string or a vector of strings with the path(s) and filename(s) of one or more rasters or one vector to be loaded directly into **GRASS**. The function will attempt to ascertain the type of object from the file extension (raster or vector), but it can help to indicate which it is using the `rastOrVect` argument if it is unclear.
#'
#' @param rastOrVect Either `NULL` (default), or `"raster"` or `"vector"`: If `x` is a filename, then the function will try to ascertain whether it represents a raster or a vector, but sometimes this will fail. In that case, it can help to specify if the file holds a raster or vector. Partial matching is used.
#'
#' @param ... Other arguments:
#' * `snap` (`GVector`s): Either `NULL` or a positive numeric value: The value of `snap` indicates how close vertices need to be for them to be shifted to to the same location. If the purpose of using `snap` is to fix topology (see **Details), small values are recommended, as values that are too large can dramatically change the shape of polygons. Units are in map units (usually meters). A value of `NULL` will use an iterative procedure starting with `snap = 0` (i.e., no snapping) to find the minimum value that produces a topologically vector. This is not guaranteed to find the best value of `snap`, it can produce undesirable changes in shapes, and for large vectors can take a long time. You must also set the `iter` argument if `snap` is NULL.  Vectors that have been snapped may need to be cleaned using [cleanGeom()] with the `break`, `duplicated`, and `smallAngles` tools.
#'
#' * `area` (polygon `GVector`s): Either `NULL` or a positive numeric value: Remove polygons with an area smaller than this value. This argument can be used to correct topologically incorrect polygons with borders that cross one another (see *Details*). If `NULL`, then an iterative procedure is applied that increases the size of `area` starting from 0 (i.e., no removal of polygons). This procedure is not guaranteed to find the optimal value of `area`, and it may remove polygons that are legitimate. You must also set the `iter` argument if `area` is NULL.
#'
#' * `iter` (`GVector`s only): Positive integer: Number of times to increment values of `snap` and/or `area` when using automated topology correction (either `snap` or `area` is `NULL`).
#'
#' * `levels` (`GRaster`s--useful mainly to developers, not most users): A `data.frame`, `data.table`, or list of `data.frame`s or `data.table`s with categories for categorical rasters: The first column of a table corresponds to raster values and must be of type `integer`. A subsequent column corresponds to category labels. By default, the second column is assumed to represent labels, but this can be changed with \code{\link[fasterRaster]{activeCat<-}}. Level tables can also be `NULL` (e.g., `data.fame(NULL)`).
#'
#' * `table` (`GVector`s--useful mainly to developers, not most users): A `data.frame` or `data.table` with one row per geometry in a `GVector`: Serves as an attribute table.
#'
#' @details Sometimes, polygons created in other software are topologically incorrect--the borders of adjacent polygons may cross one another, or there may be small gaps between them. These errors can be corrected by slightly shifting vertices and/or removing small polygons that result from intersections of larger ones that border one another.
#' 
#' You can attempt to correct vectors that have invalid topology using the optional arguments `snap` and `area`. These arguments take numeric values or `NULL` values. If you set one or both of these to `NULL`, the function will attempt to correct for invalid topology automatically by incrementally increasing their values until a topologically valid vector is created or a maximum number of iterations is surpassed.
#'
#' When implementing automatic topology correction, for both `snap` and `area`, the first increment attempted is 0 (no snapping, no area removal). If this does not yield a valid topology, then the value of `snap` is set to 0.000001 * *d*, and `area` to 0.000001 * `d^2`, where *d* is the minimum extent of the vector in the x- and y-directions. For each iteration `i`, the value of `snap` is set to *d* * 0.000001 * 2^(`i` - 1), and of `area` to  *d*^2 * 0.000001 * 2^(`i` - 1).
#'
#' @seealso [rgrass::read_RAST()] and [rgrass::read_VECT()], [cleanGeom()], plus modules [`v.in.ogr`](https://grass.osgeo.org/grass84/manuals/r.in.gdal.html), [`v.in.ogr`](https://grass.osgeo.org/grass84/manuals/v.in.ogr.html), and [`r.import`](https://grass.osgeo.org/grass84/manuals/r.import.html) in **GRASS**.
#'
#' @return A `GRaster` or `GVector`.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases fast
#' @rdname fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "character"),
	function(
		x,
		rastOrVect = NULL,
		...
	) {

	dots <- list(...)

	### raster or vector?
	#####################
	if (is.null(rastOrVect)) {

		### attempt to get type from extension
		# 3-letter extensions
		rastExtensions <- c("asc", "grd", "img", "mem", "tif", "saga")
		vectExtensions <- c("shp", "gpkg", "kml")

		ext <- .fileExt(x)
		ext <- tolower(ext)

		if (all(ext %in% rastExtensions)) {
			rastOrVect <- "raster"
		} else if (all(ext %in% vectExtensions)) {
			rastOrVect <- "vector"
		} else {
			stop("Cannot determine data if raster or vector from file name. Please use argument ", sQuote("rastOrVect"), ".")
		}

	} else {
		### user supplied rastOrVect
		rastOrVect <- omnibus::pmatchSafe(rastOrVect, c("raster", "vector"))
	}

	if (rastOrVect == "vector" & length(x) > 1L) stop("Cannot load more than one vector at a time.")

	### raster from disk
	####################
	
	if (rastOrVect == "raster") {
		
		# multiple rasters
		if (length(x) > 1L) {

			for (i in seq_along(x)) {
			
				thisOut <- fast(x[i], rastOrVect = "raster", ...)
			
				if (i == 1L) {
					out <- thisOut
				} else {
					out <- c(out, thisOut)
				}
			
			}

		# load just one raster
		} else {

			xRast <- terra::rast(x)
			nLayers <- terra::nlyr(xRast)
			xNames <- names(xRast)
			
			location <- .locationFind(xRast, match = "crs")
			
			if (is.null(location) | !grassStarted()) {

				.locationCreate(x = xRast)
				location <- .location()

			}

			.locationRestore(x = location)

			src <- .makeSourceName("r_external", type = "raster", n = 1L)
			rgrass::execGRASS(
				cmd = "r.external",
				input = x,
				output = src,
				flags = c(.quiet(), "overwrite")
			)
			
			if (nLayers > 1L) src <- paste0(src, ".", seq_len(nLayers))

			# raster levels
			if (any(names(dots) == "levels")) {
				if (!is.null(levels)) levels <- dots$levels
			} else {
				levels <- NULL
			}

			out <- .makeGRaster(src, names = xNames, levels = levels)

		}

	### vector from disk
	####################
	
	} else if (rastOrVect == "vector") {

		xVect <- terra::vect(x)
		
		# table from ...
		if (any(names(dots) == "table")) {
			
			table <- dots$table
		
		# if loading from file, get attribute table and strip it from the vector
		} else if (dim(xVect)[2L] > 0L) {

			table <- terra::as.data.frame(xVect)
			table <- data.table::as.data.table(table)
		
		} else {
			table <- NULL
		}
		
		location <- .locationFind(xVect, return = "name", match = "crs")
		
		if (is.null(location) | !grassStarted()) {

			.locationCreate(x = xVect)
			location <- .location()

		}

		.locationRestore(x = location)

		### load vector while using use snap and area to fix broken topology
		ndots <- names(dots)
		hasSnap <- any(ndots == "snap")
		hasArea <- any(ndots == "area")
		hasBoth <- hasSnap & hasArea
		
		if ((hasSnap && is.null(dots$snap)) | (hasArea && is.null(dots$area))) {

			if (!("iter" %in% ndots)) stop("If you use the ", sQuote("snap"), " or ", sQuote("area"), " arguments\n  and set at least one of them to NULL, you must also supply the ", sQuote("iter"), " argument.")

			iter <- dots$iter

		}

		# base value by which to increase snap and area when automated
		# actual value will be increment[prior value] + increment^iter
		increment <- 0.000001

		msg <- paste0("Vector has an invalid topology.\n  Try using (or increasing) the values of the ", sQuote("snap"), " and/or ", sQuote("area"), " arguments,\n  or increasing the value of ", sQuote("iter"), " if either ", sQuote("snap"), " or ", sQuote("area"), " is NULL.")

		# snap and area are both NULL
		src <- .makeSourceName("v_in_ogr", type = "vector")
		if (hasBoth && is.null(dots$snap) && is.null(dots$area)) {

			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				flags = c(.quiet(), "overwrite")
			)

			i <- 1L
			while (!.vValidCats(src) & i <= iter) {

				info <- .vectInfo(src)
				
				# geomtype
				gtype <- info$geometry
				if (gtype != "polygons") {
					stop("Argument ", sQuote("area"), " can only be used with polygon vectors.")
				}

				# get threshold values of snap and area
				diffs <- c(info$east - info$west, info$north - info$south)
				minDiff <- min(diffs)
				snap <- minDiff * increment * 2^(i - 1L)
				area <- minDiff^2 * increment * 2^(i - 1L)

				srcIn <- src
				src <- .makeSourceName("v_clean", type = "vector")

				rgrass::execGRASS(
					cmd = "v.clean",
					input = srcIn,
					output = src,
					tool = c("snap", "rmarea"),
					threshold = c(snap, area),
					type = "area",
					flags = c(.quiet(), "overwrite")
				)

				i <- i + 1L

			}

		# snap is NUMERIC, area is NUMERIC
		} else if (hasBoth && (!is.null(dots$snap) & !is.null(dots$area))) {

			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				snap = dots$snap,
				flags = c(.quiet(), "overwrite")
			)

			info <- .vectInfo(src)

			# geomtype
			gtype <- info$geometry
			if (gtype != "polygons") {
				stop("Argument ", sQuote("area"), " can only be used with polygon vectors.")
			}

			area <- dots$area

			srcIn <- src
			src <- .makeSourceName("v_clean", type = "vector")

			rgrass::execGRASS(
				cmd = "v.clean",
				input = srcIn,
				output = src,
				tool = "rmarea",
				threshold = area,
				type = "area",
				flags = c(.quiet(), "overwrite")
			)

		# snap is NUMERIC, area is NULL
		} else if (hasBoth && !is.null(dots$snap) && is.null(dots$area)) {

			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				snap = dots$snap,
				flags = c(.quiet(), "overwrite")
			)

			i <- 1L
			while (!.vValidCats(src) & i <= iter) {

				info <- .vectInfo(src)
				
				# geomtype
				gtype <- info$geometry
				if (gtype != "polygons") {
					stop("Argument ", sQuote("area"), " can only be used with polygon vectors.")
				}

				# get threshold values of snap and area
				diffs <- c(info$east - info$west, info$north - info$south)
				minDiff <- min(diffs)
				area <- minDiff^2 * increment * 2^(i - 1L)

				srcIn <- src
				src <- .makeSourceName("v_clean", type = "vector")

				rgrass::execGRASS(
					cmd = "v.clean",
					input = srcIn,
					output = src,
					tool = "rmarea",
					threshold = area,
					type = "area",
					flags = c(.quiet(), "overwrite")
				)

				i <- i + 1L

			}

		# snap is NULL, area is NUMERIC
		} else if ((hasSnap & hasArea) && is.null(dots$snap) && !is.null(dots$area)) {

			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				flags = c(.quiet(), "overwrite")
			)

			i <- 1L
			while (!.vValidCats(src) & i <= iter) {

				info <- .vectInfo(src)
				
				# geomtype
				gtype <- info$geometry
				if (gtype != "polygons") {
					stop("Argument ", sQuote("area"), " can only be used with polygon vectors.")
				}

				# get threshold values of snap and area
				diffs <- c(info$east - info$west, info$north - info$south)
				minDiff <- min(diffs)
				snap <- minDiff * increment * 2^(i - 1L)
				area <- dots$area

				srcIn <- src
				src <- .makeSourceName("v_clean", type = "vector")

				rgrass::execGRASS(
					cmd = "v.clean",
					input = srcIn,
					output = src,
					tool = c("snap", "rmarea"),
					threshold = c(snap, area),
					type = "area",
					flags = c(.quiet(), "overwrite")
				)

				i <- i + 1L

			}

		# snap is NULL, area is MISSING
		} else if ((hasSnap & !hasArea) && is.null(dots$snap)) {

			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				flags = c(.quiet(), "overwrite")
			)

			i <- 1L
			while (!.vValidCats(src) & i <= iter) {

				info <- .vectInfo(src)
				
				# geomtype
				gtype <- info$geometry
				gtype <- c("point", "line", "area")[gtype == c("points", "lines", "polygons")]

				# get threshold values of snap and area
				diffs <- c(info$east - info$west, info$north - info$south)
				minDiff <- min(diffs)
				snap <- minDiff * increment * 2^(i - 1L)

				srcIn <- src
				src <- .makeSourceName("v_clean", type = "vector")

				rgrass::execGRASS(
					cmd = "v.clean",
					input = srcIn,
					output = src,
					tool = "snap",
					threshold = snap,
					type = gtype,
					flags = c(.quiet(), "overwrite")
				)

				i <- i + 1L

			}

		# snap is NUMERIC, area is MISSING
		} else if ((hasSnap & !hasArea) && !is.null(dots$snap)) {

			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				snap = dots$snap,
				flags = c(.quiet(), "overwrite")
			)

		# snap is MISSING, area is NUMERIC
		} else if ((!hasSnap & hasArea) && !is.null(dots$area)) {

			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				flags = c(.quiet(), "overwrite")
			)

			info <- .vectInfo(src)
			
			# geomtype
			gtype <- info$geometry
			if (gtype != "polygons") {
				stop("Argument ", sQuote("area"), " can only be used with polygon vectors.")
			}

			area <- dots$area

			srcIn <- src
			src <- .makeSourceName("v_clean", type = "vector")

			rgrass::execGRASS(
				cmd = "v.clean",
				input = srcIn,
				output = src,
				tool = "rmarea",
				threshold = area,
				type = "area",
				flags = c(.quiet(), "overwrite")
			)

		# snap is MISSING, area is NULL
		} else if ((!hasSnap & hasArea) && is.null(dots$area)) {

			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				flags = c(.quiet(), "overwrite")
			)

			i <- 1L
			while (!.vValidCats(src) & i <= iter) {

				info <- .vectInfo(src)
				
				# geomtype
				gtype <- info$geometry
				if (gtype != "polygons") {
					stop("Argument ", sQuote("area"), " can only be used with polygon vectors.")
				}

				# get threshold values of snap and area
				diffs <- c(info$east - info$west, info$north - info$south)
				minDiff <- min(diffs)
				area <- minDiff^2 * increment * 2^(i - 1L)

				srcIn <- src
				src <- .makeSourceName("v_clean", type = "vector")

				rgrass::execGRASS(
					cmd = "v.clean",
					input = srcIn,
					output = src,
					tool = "rmarea",
					threshold = area,
					type = "area",
					flags = c(.quiet(), "overwrite")
				)

				i <- i + 1L

			}

		# snap is MISSING and area is MISSING
		} else if (!hasSnap & !hasArea) {

			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				flags = c(.quiet(), "overwrite")
			)

		}
			
		if (!.vValidCats(src)) stop(msg)

		out <- .makeGVector(src, table = table)

	}
	out

	} # EOF
)

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "SpatRaster"),
	function(x) {

	rastFile <- terra::sources(x)
	levels <- .getLevels(x)

	if (any(rastFile == "")) {
		tempFile <- tempfile(fileext = ".tif")
		terra::writeRaster(x, filename = tempFile, overwrite = TRUE, datatype = datatype(x, "terra", forceDouble = TRUE))
		rastFile <- tempFile
	} else {
		rastFile <- terra::sources(x)
	}

	fast(x = rastFile, rastOrVect = "raster", levels = levels)

	} # EOF
)

#' Get levels from a SpatRaster or a file
#'
#' @param x A `SpatRaster`, `data.frame`, `data.table`, empty string, `NULL`, or a file name. CSV, TAB, RDS, and RDA are allowed file types.
#'
#' @noRd
.getLevels <- function(x) {

	if (is.null(x)) {
		levels <- NULL
	} else if (inherits(x, "SpatRaster")) {
		levels <- terra::cats(x)
	} else if (is.character(x)) {
		if (x == "") {
			levels <- list(x)
		} else {
		
			# load from file
		    nc <- nchar(x)
			suffix <- substr(x, nc - 3L, nc)
			
			suffix <- tolower(suffix)
			
			if (suffix %in% c(".csv", ".tab")) {
				levels <- data.table::fread(x, nThread = faster("cores"), na.strings = "<not defined>", showProgress = FALSE)
			} else if (suffix == ".rds") {
				levels <- readRDS(x)
			} else if (suffix == ".rda") {
				levelsObj <- load(x)
				levels <- get(levelsObj)
			} else {

				suffix <- substr(x, nc - 5L, nc)

				suffix <- tolower(suffix)

				if (suffix == ".rdata") {
					levelsObj <- load(levels)
					levels <- get(levelsObj)
				} else {
					stop("Cannot open a file with level data of this type.\n  Supported types include .csv, .tab, .rds, .rda/.rdata (case is ignored).")
				}

			}
		}
	
	} else if (!inherits(x, c("list", "data.frame", "data.table"))) {
		stop("Argument ", sQuote("levels"), " must be a data.frame, data.table, an empty string, a list of these, OR NULL or a file name.")
	}
	levels

}

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "SpatVector"),
	function(x, ...) .fastVector(x, ...)
)

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "sf"),
	function(x, ...) .fastVector(x, ...)
)

# 1. Write vector to disk (if needed)
# 2. Send to fast(signature = "character")
#' @noRd
.fastVector <- function(
	x,		# SpatVector or sf
	...
) {

	dots <- list(...)
	ndots <- names(dots)
	
	if (any('snap' %in% ndots) && (is.null(dots$snap) & !any('iter' %in% ndots))) stop("If you use the ", sQuote("snap"), " argument and set it to NULL,\n  you must also provide an ", sQuote("iter"), " argument.")
	if (any('area' %in% ndots) && (is.null(dots$area) & !any('iter' %in% ndots))) stop("If you use the ", sQuote("area"), " argument and set it to NULL,\n  you must also provide an ", sQuote("iter"), " argument.")

	if (!inherits(x, "SpatVector")) x <- terra::vect(x)
	if (any('area' %in% ndots) && terra::geomtype(x) != "polygons") stop("The ", sQuote("area"), " argument can only be used with a polygons vector.")

	# remove data frame
	table <- data.table::as.data.table(x)
	if (nrow(table) != 0L) {
		nc <- ncol(x)
		x[ , seq_len(nc)] <- NULL 
	}

	if (terra::sources(x) == "") {
		vectFile <- tempfile(fileext = ".gpkg")
		terra::writeVector(x, filename = vectFile, filetype = "GPKG", overwrite = TRUE)
	} else {
		vectFile <- terra::sources(x)
	}

	args <- list(
		x = vectFile,
		rastOrVect = "vector",
		table = table
	)
	args <- c(args, list(...))
	do.call(fast, args = args)
	
}
