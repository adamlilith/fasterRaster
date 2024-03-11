#' Create a GRaster or GVector
#'
#' @description `fast()` creates a `GRaster` or `GVector` from a file, or from a `SpatRaster`, `SpatVector`, or `sf` vector. Behind the scenes, this function will also create a connection to **GRASS** if none has yet been made yet.
#'
#' **GRASS** supports loading from disk a variety of raster formats (see the **GRASS** manual page for [`r.in.gdal`](https://grass.osgeo.org/grass84/manuals/r.in.gdal.html)) and vector formats (see the **GRASS** manual page for [`v.in.ogr`](https://grass.osgeo.org/grass84/manuals/v.in.ogr.html)), though not all of them will work with this function.
#'
#' Note that there are a several methods for fixing issues with vectors. If the vector has already been imported as a `GVector`, [other tools][breakPolys] are also available.
#'
#' @param x Any one of:
#' * A `SpatRaster` raster. Rasters can have one or more layers.
#' * A `SpatVector` or `sf` spatial vector.
#' * A character string or a vector of strings with the path(s) and filename(s) of one or more rasters or one vector to be loaded directly into **GRASS**. The function will attempt to ascertain the type of object from the file extension (raster or vector), but it can help to indicate which it is using the `rastOrVect` argument if it is unclear.
#'
#' @param ... Other arguments:
#' * `rastOrVect`: Either `NULL` (default), or `"raster"` or `"vector"`: If `x` is a filename, then the function will try to ascertain whether it represents a raster or a vector, but sometimes this will fail. In that case, it can help to specify if the file holds a raster or vector. Partial matching is used.
#' * `levels` (`GRaster`s--useful mainly to developers, not most users): A `data.frame`, `data.table`, or list of `data.frame`s or `data.table`s with categories for categorical rasters: The first column of a table corresponds to raster values and must be of type `integer`. A subsequent column corresponds to category labels. By default, the second column is assumed to represent labels, but this can be changed with \code{\link[fasterRaster]{activeCat<-}}. Level tables can also be `NULL` (e.g., `data.fame(NULL)`).
#' * `area` (polygon `GVector`s): Either `NULL` or a positive numeric value: Remove polygons with an area smaller than this value. This argument can be used to correct topologically incorrect polygons with borders that cross one another (see *Details*). Units of `area` are in square meters (regardless of the CRS). If `NULL`, then an iterative procedure is used to identify a valid value for `area`.
#' * `correct` (`GVector`s only): Logical. If `TRUE` (default), then [topology][tutorial_vector_topology_and_troubleshooting] of the vector will be corrected. Please see *Details* and the [tutorial][tutorial_vector_topology_and_troubleshooting] on vector topology and troubleshooting.
#' * `dropTable` (`GVector`s only): Drop any data table associated with the geometries. This can be useful for importing a topologically incorrect vector for which the table is mot needed The general idea is that the vector can have imperfections which appear as "slivers" between polygons or dangling lines. These are treated as legitimate geometries (but can be removed using the `snap` and `area` arguments or after loading with [geometry cleaning][breakPolys])), but they can cause errors because it is usually not possible to match a sliver to a single row in a data table since it is comprised of at least two polygons.
#' * `increment` (`GVector`s only): Numeric: Multiplier for increasing `snap` and/or `area` each iteration (only when `snap` and/or `area` are `NULL`).
#' * `iter` (`GVector`s only): Positive integer: Number of times to increment values of `snap` and/or `area` when using automated topology correction (either `snap` or `area` is `NULL`).
#' * `snap` (`GVector`s only): Either `NULL` or a positive numeric value: The value of `snap` indicates how close vertices need to be for them to be shifted to to the same location. Units of `snap` are map units (usually meters), or degrees for unprojected CRSs. A value of `NULL` will use an iterative procedure to find the best, smallest value of `snap`. You must also set the `iter` argument if `snap` is NULL.  Please see *Details* and the [tutorial][tutorial_vector_topology_and_troubleshooting] on vector topology and troubleshooting.
#' * `table` (`GVector`s--useful mainly to developers, not most users): A `data.frame` or `data.table` with one row per geometry in a `GVector`: Serves as an attribute table.
#' * `verbose`: Displays progress for iterative topological correction (i.e., `snap` and/or `area` are `NULL`).
#'
#' @details **GRASS** uses a [topological][tutorial_vector_topology_and_troubleshooting] model for vectors. Sometimes, polygons created in other software are topologically incorrect--the borders of adjacent polygons may cross one another, or there may be small gaps between them. These errors can be corrected by slightly shifting vertices and/or removing small polygons that result from intersections of larger ones that border one another. A topological system also recognizes that boundaries to adjacent polygons are shared by the areas, so should not be ascribed attributes that belong to both areas (e.g., the shared border between two countries "belongs" to both countries).
#'
#' By default, `fast()` will correct topological errors in vectors. This will not necessarily give you a vector that is commensurate with what you get from [terra::vect()], but it will be topologically correct. You can force `fast()` not to correct topology by setting `correct = FALSE`, which is more likely to yield what you would get with **terra**. However, even if you do this, you can still use the `snap` and `area` options to force further corrections. `fast()` has two levels of topology correction that can be used simultaneously: a) correction on loading the vector (`correct = TRUE`), b) and correction using the `snap` and/or `area` arguments, plus also maybe the `iter` and `increment` arguments.
#' 
#' Topological issues can often be corrected by snapping vertices that are close to one another to the same location, and/or by removing very small polygons (which can be produced when `correct = TRUE` and slight overlaps in polygons are converted to their own polygons). Snapping is performed by using the `snap` argument, and removal of small polygons through the `area` argument. Snapping and area removal can be performed regardless of the value of `correct`. The `snap` and `area` arguments take numeric values or can be assigned to `NULL`. If you set one or both of these to `NULL`, the function will attempt to snap and/or remove small areas by incrementally increasing their values until a topologically valid vector is created or a maximum number of iterations is surpassed. If you use the automatic option, you must set the number of iterations using `iter`. You can also set the rate of increase using `increment` (which is 1E-6 by default).
#'
#' When implementing automatic topology correction (i.e., either `snap` and/or `area` are `NULL`), the first iteration attempted does no snapping or no area removal. If this does not yield a valid topology, then *d*, the minimum of the extent of the vector in the x- and y-directions, is calculated. For the remaining iterations, the value of `snap` is set to *d* * `increment` * 2^(`i` - 1) and `area` to  *d*^2 * `increment` * 2^(`i` - 1).
#'
#' @seealso [tutorial on vector topology and troubleshooting][tutorial_vector_topology_and_troubleshooting], [rgrass::read_RAST()] and [rgrass::read_VECT()], [vector cleaning][breakPolys], [removeHoles()], plus modules [`v.in.ogr`](https://grass.osgeo.org/grass84/manuals/r.in.gdal.html), [`v.in.ogr`](https://grass.osgeo.org/grass84/manuals/v.in.ogr.html), and [`r.import`](https://grass.osgeo.org/grass84/manuals/r.import.html) in **GRASS**.
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
		...
	) {

	### dots
	########
	dots <- list(...)
	dotNames <- names(dots)

	rastOrVect <- if (any(dotNames == "rastOrVect")) {
		dots$rastOrVect
	} else {
		NULL
	}

	### raster or vector?
	#####################
	if (is.null(rastOrVect)) {

		### attempt to get type from extension
		# 3-letter extensions
		rastExtensions <- c("asc", "asci", "ascii", "grd", "hgt", "img", "mem", "tif", "tifg", "saga")
		vectExtensions <- c("shp", "gpkg", "kml")

		ext <- .fileExt(x)
		ext <- tolower(ext)

		if (any(ext %in% rastExtensions)) {
			rastOrVect <- "raster"
		} else if (any(ext %in% vectExtensions)) {
			rastOrVect <- "vector"
		} else {
			stop("Cannot determine data if raster or vector from file name. Please use argument `rastOrVect`.")
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
			if (any(dotNames == "levels")) {
				levels <- dots$levels
			} else {
				levels <- NULL
			}

			out <- .makeGRaster(src, names = xNames, levels = levels)

		}

	### vector from disk
	####################
	
	} else if (rastOrVect == "vector") {

		digits <- 9 # number of digits to display for "verbose" messages

		xVect <- terra::vect(x)

		# values of snap, area, iter, verbose, correct topology flag, and increment
		hasSnap <- any(dotNames == "snap")
		hasArea <- any(dotNames == "area")

		if (hasSnap) {
			snap <- dots$snap
			snapNull <- is.null(snap)
		} else {
			snap <- -1
			snapNull <- FALSE
		}

		if (hasArea) {
			area <- dots$area
			areaNull <- is.null(area)
		} else {
			area <- 0
			areaNull <- FALSE
		}
		
		if (any(dotNames == "iter")) {
			iter <- dots$iter
		} else {
			iter <- 1L
		}

		if (faster("verbose")) {
			verbose <- TRUE
		} else if (any(dotNames == "verbose")) {
			verbose <- dots$verbose
		} else {
			verbose <- FALSE
		}
		
		if (any(dotNames == "correct")) {
			correct <- dots$correct
			if (!correct) {
				correctTopoFlag <- "c"
			} else {
				correctTopoFlag <- NULL
			}
		} else {
			correct <- faster("correct")
			correctTopoFlag <- if (correct) { NULL } else { "c" }
		}

		# base value by which to increase snap and area when automated
		# actual value will be increment[prior value] + increment^iter
		if (!any(dotNames == "increment")) {
			increment <- 1E-6
		} else {
			increment <- dots$increment
		}

		# table from ...
		if ((any(dotNames == "dropTable") && dots$dropTable) & (any(dotNames == "table") && !is.null(dots$table))) {
			stop("Cannot specify a table and drop the table at the same time.")
		} else if (any(dotNames == "dropTable") && dots$dropTable) {
			table <- NULL
		} else if (any(dotNames == "table")) {
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

		# snap and area are both NULL
		if ((hasSnap & hasArea) && snapNull && areaNull) {

			i <- 1L
			valid <- FALSE
			while (!valid & i <= iter) {

				src <- .makeSourceName("v_in_ogr", type = "vector")

				# try import with no snap and no area
				if (i == 1L) {

					if (verbose) omnibus::say("Iteration 1: No snapping or polygon removal...")

					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = -1,
						min_area = 0,
						flags = c(.quiet(), "overwrite", correctTopoFlag)
					)

				} else if (i > 1L) {

					snapArea <- .snapArea(info = info, i = i, increment = increment, snap = NULL, area = NULL)
					snap <- snapArea[["snap"]]
					area <- snapArea[["area"]]

					if (verbose) omnibus::say("Iteration ", i, ": Snapping by ", round(snap, digits), " map units and removing polygons <", round(area, digits), " m2...")

					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = snap,
						min_area = area,
						flags = c(.quiet(), "overwrite", correctTopoFlag)
					)

				}

				valid <- .validVector(src, table)
				i <- i + 1L

			}

		# snap is NUMERIC, area is NUMERIC
		} else if ((hasSnap & hasArea) && (!snapNull & !areaNull)) {

			if (verbose) omnibus::say("Snapping by ", round(snap, digits), " map units and removing polygons <", round(area, digits), " m2...")

			src <- .makeSourceName("v_in_ogr", type = "vector")
			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				snap = snap,
				min_area = area,
				flags = c(.quiet(), "overwrite", correctTopoFlag)
			)
			valid <- .validVector(src, table)
		
		# snap is NUMERIC, area is NULL
		} else if ((hasSnap & hasArea) && !snapNull && araNull) {

			i <- 1L
			valid <- FALSE
			while (!valid & i <= iter) {

				if (verbose) omnibus::say("Iteration 1: Snapping by ", round(snap, digits), " map units snap with no polygon removal...")

				src <- .makeSourceName("v_in_ogr", "vector")
				if (i == 1L) {

					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = snap,
						min_area = 0,
						flags = c(.quiet(), "overwrite", correctTopoFlag)
					)

				} else {

					snapArea <- .snapArea(info = info, i = i, increment = increment, snap = snap, area = NULL)
					area <- snapArea[["area"]]

					if (verbose) omnibus::say("Iteration ", i, ": Snapping by ", round(snap, digits), " map units snap and removing polygons ", round(area, digits), " m2...")

					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = snap,
						min_area = area,
						flags = c(.quiet(), "overwrite", correctTopoFlag)
					)

				}

				valid <- .validVector(src, table)
				i <- i + 1L

			} # next iteration

		# snap is NULL, area is NUMERIC
		} else if ((hasSnap & hasArea) && snapNull && !areaNull) {

			i <- 1L
			valid <- FALSE
			while (!valid & i <= iter) {

				if (verbose) omnibus::say("Iteration 1: No snapping but removing polygons <", round(area, digits), " m2...")

				src <- .makeSourceName("v_in_ogr", "vector")
				if (i == 1L) {

					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = -1,
						min_area = area,
						flags = c(.quiet(), "overwrite", correctTopoFlag)
					)

				} else {

					snapArea <- .snapArea(info = info, i = i, increment = increment, snap = NULL, area = area)
					snap <- snapArea[["snap"]]

					if (verbose) omnibus::say("Iteration ", i, ": Snapping by ", round(snap, digits), " map units snapping and removing polygons <", round(area, digits), " m2...")

					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = snap,
						min_area = area,
						flags = c(.quiet(), "overwrite", correctTopoFlag)
					)

				}

				valid <- .validVector(src, table)
				i <- i + 1L

			} # next iteration
		
		# snap is NULL, area is MISSING
		} else if ((hasSnap & !hasArea) && snapNull) {

			i <- 1L
			valid <- FALSE
			while (!valid & i <= iter) {

				if (verbose) omnibus::say("Iteration 1: No snapping or polygons removal...")

				src <- .makeSourceName("v_in_ogr", "vector")
				if (i == 1L) {

					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = -1,
						min_area = 0,
						flags = c(.quiet(), "overwrite", correctTopoFlag)
					)

				} else {

					snapArea <- .snapArea(info, i = i, increment = increment, snap = NULL, area = NULL)
					snap <- snapArea[["snap"]]

					if (verbose) omnibus::say("Iteration ", i, ": Snapping by ", round(snap, digits), " map units snapping with no polygon removal...")

					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = snap,
						min_area = 0,
						flags = c(.quiet(), "overwrite", correctTopoFlag)
					)

				}

				valid <- .validVector(src, table)
				i <- i + 1L

			} # next iteration
		
		# snap is NUMERIC, area is MISSING
		} else if ((hasSnap & !hasArea) && !snapNull) {
			
			if (verbose) say("Snapping by ", round(snap, digits), " map units...")
			src <- .makeSourceName("v_in_ogr", type = "vector")
			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				snap = snap,
				min_area = 0,
				flags = c(.quiet(), "overwrite", correctTopoFlag)
			)
			valid <- .validVector(src, table)

		# snap is MISSING, area is NUMERIC
		} else if ((!hasSnap & hasArea) && !areaNull) {

			if (verbose) say("Removing polygons <", round(area, digits), " m2...")
			src <- .makeSourceName("v_in_ogr", type = "vector")
			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				snap = -1,
				min_area = area,
				flags = c(.quiet(), "overwrite", correctTopoFlag)
			)
			valid <- .validVector(src, table)

		# snap is MISSING, area is NULL
		} else if ((!hasSnap & hasArea) && areaNull) {

			i <- 1L
			valid <- FALSE
			while (!valid & i <= iter) {

				if (verbose) omnibus::say("Iteration 1: No snapping or polygon removal...")

				src <- .makeSourceName("v_in_ogr", "vector")
				if (i == 1L) {

					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = -1,
						min_area = area,
						flags = c(.quiet(), "overwrite", correctTopoFlag)
					)

				} else {

					snapArea <- .snapArea(info = info, i = i, increment = increment, snap = NULL, area = NULL)
					area <- snapArea[["area"]]

					if (verbose) omnibus::say("Iteration ", i, ": No snapping but removing polygons <", round(area, digits), " m2...")

					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = -1,
						min_area = area,
						flags = c(.quiet(), "overwrite", correctTopoFlag)
					)

				}

				valid <- .validVector(src, table)
				i <- i + 1L

			} # next iteration
		
		# snap is MISSING and area is MISSING
		} else if (!hasSnap & !hasArea) {

			if (verbose) say("No snapping or polygon removal...")
			src <- .makeSourceName("v_in_ogr", type = "vector")
			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				snap = -1,
				min_area = 0,
				flags = c(.quiet(), "overwrite", correctTopoFlag)
			)
			valid <- .validVector(src, table)

		}

		# detach GRASS vector table
		# GRASS tables are very slow to work with and confound subsetting
		# .vDetachDatabase(src)

		if (!is.null(table)) {
		
			info <- .vectInfo(src)
			if (nrow(table) > info$nGeometries) {

				msg <- paste0("Vector has more geometries than rows in its data table. Try:\n  * Setting the `correct` argument to `TRUE`;\n  * Using (or increasing) the value(s) of the `snap` and/or `area` arguments;\n  * Increasing the value of `iter` and/or `increment` if either `snap` or `area` is NULL;\n  * Dropping the data table associated with the vector using `dropTable = FALSE`; or\n  * Correcting the vector outside of fasterRaster with `terra::makeValid()` or `sf::st_make_valid()` before using fast().")
				stop(msg)

			} else if (nrow(table) < info$nGeometries) {
			
				msg <- paste0("Vector has more rows in its data table than geometries. Try:\n  * Setting the `correct` argument to `TRUE`;\n  * Decreasing the value(s) of the `snap` and/or `area` arguments;\n  * If either `snap` or `area` is NULL, decreasing the value of the `increment` argument;\n  * Dropping the data table associated with the vector using `dropTable = FALSE`; or\n  * Correcting the vector outside of fasterRaster with `terra::makeValid()` or `sf::st_make_valid()` before using fast().")
				stop(msg)
			
			}
		
		}

		msg <- paste0("Vector has geometries that overlap. Try:\n  * Setting the `correct` argument to `TRUE`;\n  * Increasing the value(s) of the `snap` and/or `area` arguments;\n  * If either `snap` or `area` is NULL, increasing the value of the `iter` or `increment` arguments;\n  * Dropping the data table associated with the vector using `dropTable = FALSE`; or\n  * Correcting the vector outside of fasterRaster with `terra::makeValid()` or `sf::st_make_valid()` before using fast().")
		out <- .makeGVector(src, table = table)

		# detach GRASS vector table
		# GRASS tables are very slow to work with and confound subsetting
		# .vDetachDatabase(src)

		out <- .makeGVector(src, table)

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
		stop("Argument `levels` must be a data.frame, data.table, an empty string, a list of these, OR NULL or a file name.")
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
	dotNames <- names(dots)
	
	if (any('snap' %in% dotNames) && (is.null(dots$snap) & !any("iter" %in% dotNames))) stop("If you use the `snap` argument and set it to NULL,\n  you must also provide an `iter` argument.")
	if (any('area' %in% dotNames) && (is.null(dots$area) & !any("iter" %in% dotNames))) stop("If you use the `area` argument and set it to NULL,\n  you must also provide an `iter` argument.")

	if (!inherits(x, "SpatVector")) x <- terra::vect(x)
	if (any('area' %in% dotNames) && terra::geomtype(x) != "polygons") stop("The `area` argument can only be used with a polygons vector.")

	# remove data frame
	if (any(dotNames == "dropTable") && dots$dropTable) {
	
		table <- NULL

	} else {

		table <- data.table::as.data.table(x)
		# if (nrow(table) != 0L) {
		# 	nc <- ncol(x)
		# 	x[ , seq_len(nc)] <- NULL 
		# }

	}

	if (terra::sources(x) == "") {
		vectFile <- tempfile(fileext = ".gpkg")
		terra::writeVector(x, filename = vectFile, filetype = "GPKG", overwrite = TRUE)
	} else {
		vectFile <- terra::sources(x)
	}

	args <- list(x = vectFile, rastOrVect = "vector", table = table)
	args <- c(args, list(...))
	do.call(fast, args = args)
	
}

#' Auto-calculate snap and area
#' 
#' @param info A list: The output of `.vectInfo()`.
#' @param i Integer: Loop counter.
#' @param increment Numeric: Proportion of minimum of extent by which to increase snap, times 2^(i - 1).
#'
#' @returns A named numeric vector.
#'
#' @noRd
.snapArea <- function(info, i, increment, snap = NULL, area = NULL) {

	diffs <- c(info$east - info$west, info$north - info$south)
	minDiff <- min(diffs)
	
	if (is.null(snap) & is.null(area)) {
		snap <- minDiff * increment * 2^(i - 1L)
		area <- minDiff^2 * increment * 2^(i - 1L)
	} else if (is.null(snap) & !is.null(area)) {
		snap <- minDiff * increment * 2^(i - 1L)
	} else if (!is.null(snap) & is.null(area)) {
		area <- minDiff^2 * increment * 2^(i - 1L)
	} else {
		stop("Both snap and area cannot be NULL.")
	}
	
	c("snap" = snap, "area" = area)

}

#' Test if a vector is valid
#'
#' @param src The [sources()] name of a **GRASS** vector
#' @param table Either `NULL` (no table) or a `data.frame`, `data.table`, or `matrix`
#'
#' @returns Logical.
#'
#' @noRd
.validVector <- function(src, table) {

	if (!is.null(table)) {

		info <- .vectInfo(src)
		nGeoms <- info$nGeometries
		tableValid <- nGeoms == nrow(table)

	} else {
		tableValid <- TRUE
	}

	if (tableValid) {
		catsValid <- .vValidCats(src)
	} else {
		catsValid <- FALSE
	}

	tableValid & catsValid

}


