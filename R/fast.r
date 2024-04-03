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
#' @param rastOrVect: Either `NULL` (default), or `"raster"` or `"vector"`: If `x` is a filename, then the function will try to ascertain whether it represents a raster or a vector, but sometimes this will fail. In that case, it can help to specify if the file holds a raster or vector. Partial matching is used.
#'
#' @param levels (`GRaster`s only): A `data.frame`, `data.table`, or list of `data.frame`s or `data.table`s with categories for categorical rasters. The first column of a table corresponds to raster values and must be of type `integer`. A subsequent column corresponds to category labels. By default, the second column is assumed to represent labels, but this can be changed with \code{\link[fasterRaster]{activeCat<-}}. Level tables can also be `NULL` (e.g., `data.fame(NULL)`). You can also assign levels after loading a raster using \code{\link[fasterRaster]{levels<-}}.
#'
#' @param correct Logical (`GVector`s only): Correct topological issues. See *Details* for more details! By default, this is `TRUE`.
#'
#' @param snap `GVector`s only`: Numeric or `NULL` (default). The value of `snap` indicates how close vertices need to be for them to be shifted to to the same location. Units of `snap` are map units (usually meters), or degrees for unprojected CRSs. For lines and polygons vectors, a value of `NULL` will invoke an iterative procedure to find an optimal, smallest value of `snap`. To turn snapping off, set `snap = 0`. See *Details* for more details!
#'
#' @param area Polygon `GVector`s only: Either a positive numeric value or `NULL` (default). Remove polygons with an area smaller than this value. Units of `area` are in square meters (regardless of the CRS). If `NULL`, then an iterative procedure is used to identify a value of `area` that results in a topologically correct polygon vector. For point and lines vectors, this argument is ignored. To turn area removal off, set `area = 0`. See *Details* for more details! 
#'
#' @param steps `GVector`s only: A positive integer > 1 (default is 10). When using automatic vector correction (i.e., either `snap = NULL` and/or `area = NULL`), this is the number of values of `snap` and/or `area` to try to generate a correct topology, including no snapping or polygon removal (i.e, `snap = 0` and `area = 0`).
#'
#' @param dropTable `GVector`s only: Logical. If `TRUE`, then drop the data table associated with a vector. By default, this is `FALSE`. See *Details* for more details!
#'
#' @param verbose `GVector`s only: Logical. Displays progress when using automatic topology correction.
#'
#' @param ... Other arguments::
#' * `table` (`GVector`s--useful mainly to developers, not most users): A `data.frame` or `data.table` with one row per geometry in a `GVector`. Serves as an attribute table.
#' * `xVect` (`GVector`s--useful mainly to developers, not most users): The `SpatVector` that corresponds to the file named by `x`.
#'
#' @details **GRASS** uses a [topological][tutorial_vector_topology_and_troubleshooting] model for vectors. Topological issues generally arise only with polygon vectors, not point or line vectors. Sometimes, polygons created in other software are topologically incorrect--the borders of adjacent polygons may cross one another, or there may be small gaps between them. These errors can be corrected by slightly shifting vertices and/or removing small polygons that result from intersections of larger ones that border one another. A topological system also recognizes that boundaries to adjacent polygons are shared by the areas, so should not be ascribed attributes that belong to both areas (e.g., the shared border between two countries "belongs" to both countries).
#'
#' By default, `fast()` will try to correct topological errors in vectors. There are three levels of correction, and they are not necessarily mutually exclusive:
#' 1. Automatic correction: By default, `fast()` will apply automatic topology correction. You can turn this off using the `correct = FALSE` argument, though in most cases this is not recommended.
#' 2. Manual correction: In addition to correction from step 1, you can cause vertices of polygons close to one another to be "snapped" to same place and/or polygons that are smaller than some threshold to be removed. Problems with mis-aligned vertices arise when adjacent polygons are meant to share borders, but slight differences in the locations of the vertices cause them to  mis-align. This mis-alignment can also produce small "slivers" of polygons that are the areas where they overlap. You can snap vertices within a given distance of one another using the `snap` argument followed by a numeric value, like `snap = 0.000001`. Units of `snap` are in map units (usually meters) for projected coordinate reference systems and degrees for unprojected systems (e.g., WGS84, NAD83, NAD27). You can also remove polygons that are smaller than a particular area using the `area` argument followed by a numeric value (e.g., `area = 1`). The units of `area` are in m2, regardless of the coordinate reference system. Note that using `snap` and `area` entails some risk, as it is possible for nearby vertices to actually be distinct and for small areas to be legitimate.
#' 3. Automatic snapping and/or area removal: In addition to the correction from step 1, you can use automatic `snap` and/or `area` correction on polygons vectors by setting `snap` and/or `area` to `NULL` (i.e., their default values). If just `snap` is `NULL`, only automatic snapping will be performed, and if just `area` is `NULL`, then only automatic area removal will be performed. Regardless, you will also need to set an integer value for `steps`, which is the number of steps to take between the smallest value of `snap` and/or `area` and the maximum value attempted. The function will then proceed by first attempting `snap = 0` and/or `area = 0`. If this does not produce a topologically correct vector, **GRASS** will (internally) suggest a range for `snap`. The `fast()` function then creates `steps` values from the lowest to the highest values of this range evenly-spaced along the log values of this range, then proceed to repeat the loading process until either the vector is imported correctly or the maximum value suggested is reached and results in a failed topology. Smaller values of `step` will result in more fine-grained attempts so are less likely to yield overcorrection, but can also take more time. The value of `area` in automatic correction is set to `snap^2`. **NB**: Automated snapping and area removal are only able performed on polygons vectors, even if `snap` or `area` is `NULL`. To snap lines or points, you must set `snap` equal to a numeric value.
#' 4. Data table-vector mismatching: If your vector has a data table ("attribute table") associated with it, errors can occur if there are more/fewer geometries (or multi-geometries) per row in the table. If you do not really need the data table to do your analysis, you can remove it (and thus obviate this error) using `dropTable = TRUE` when using `fast()`.
#' 5. Pre-load correction: Before you convert the vector into **fasterRaster**'s `GVector` format, you can also try using the [terra::makeValid()] or [sf::st_make_valid()] tools to fix issues, then use `fast()`.
#' 6. If you do get a vector loaded into `GVector` format, you can also use [tools][breakPolys] or [fillHoles()] to fix issues.
#'
#' @seealso [rgrass::read_RAST()] and [rgrass::read_VECT()], [vector cleaning][breakPolys], [removeHoles()], plus modules [`v.in.ogr`](https://grass.osgeo.org/grass84/manuals/r.in.gdal.html), [`v.in.ogr`](https://grass.osgeo.org/grass84/manuals/v.in.ogr.html), and [`r.import`](https://grass.osgeo.org/grass84/manuals/r.import.html) in **GRASS**.
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
		levels = NULL,
		correct = TRUE,
		snap = NULL,
		area = NULL,
		steps = 10,
		dropTable = FALSE,
		verbose = FALSE,
		...
	) {

	### dots
	########
	dots <- list(...)
	dotNames <- names(dots)

	x <- normalizePath(x, mustWork = FALSE)

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
				flags = c(.quiet(), "overwrite", "o") # overriding projection check!
			)
			
			if (!.exists(src)) stop("Raster not loaded. You may need to use an absolute (not relative) file path.")
			if (nLayers > 1L) src <- paste0(src, ".", seq_len(nLayers))

			# raster levels
			if (is.list(levels)) {
				if (length(levels) == 1L & length(xNames) > 1L & is.null(levels[[1L]])) levels <- NULL
			}
			out <- .makeGRaster(src, names = xNames, levels = levels)

		}

	### vector from disk
	####################
	
	} else if (rastOrVect == "vector") {

		if (!any(dotNames == "xVect")) {
			xVect <- terra::vect(x, what = "geom")
		} else {
			xVect <- dots$xVect
		}
		gtype <- terra::geomtype(xVect)

		# table from ...
		if (dropTable) {
			table <- NULL
			if (any(dotNames == "table")) warning("Argument `dropTable` is TRUE, so the data table will be ignored.")
		} else if (any(dotNames == "table")) {
			table <- dots$table
		} else {
			if (any(dotNames == "table")) {
				table <- table
			} else {
				table <- terra::vect(x, what = "attributes")
			}
			table <- data.table::as.data.table(table)
		}

		# location, location, location...
		location <- .locationFind(xVect, return = "name", match = "crs")
		if (is.null(location) | !grassStarted()) {

			.locationCreate(x = xVect)
			location <- .location()

		}
		.locationRestore(x = location)

		# correct topology?
		if (correct) {
			correctTopoFlag <- NULL
		} else {
			correctTopoFlag <- "c" # no correction
		}

		# no snapping/area removal for particular geometry types
		if (is.null(snap) & gtype == "points") snap <- -1
		if (is.null(area) & gtype %in% c("lines", "points")) area <- 0

		### first try (no snapping or area removal)
		if (is.null(snap) || snap <= 0) {
			thisSnap <- -1
			thisSnapNice <- "no"
		} else {
			thisSnap <- snap
			thisSnapNice <- paste0(thisSnap, " map-units")
		}

		if (is.null(area) || area == 0) {
			thisArea <- 0
			thisAreaNice <- "no polygon removal"
		} else {
			thisArea <- area
			thisAreaNice <- paste0("removal of polygons of <", thisArea, " m2")
		}

		if (verbose & gtype == "polygons") {
			omnibus::say("Creating GVector with ", thisSnapNice, " snapping and ", thisAreaNice, "...")
		} else if (verbose) {
			omnibus::say("Creating GVector with ", thisSnapNice, " snapping of vertices/points...")
		}

		src <- .makeSourceName("v_in_ogr", "vector")
		if (is.null(snap) | is.null(area)) {
			
			# slower if we need to record messages
			suppressMessages(
				run <- rgrass::execGRASS(
					cmd = "v.in.ogr",
					input = x,
					output = src,
					snap = thisSnap,
					min_area = thisArea,
					flags = c(.quiet(), "verbose", "overwrite", "t", correctTopoFlag),
					ignore.stderr = FALSE,
					Sys_show.output.on.console = FALSE,
					echoCmd = FALSE, # displays GRASS command
					intern = TRUE
				)
			)

		} else {

			# faster if we remove fluff
			rgrass::execGRASS(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				snap = -1,
				min_area = 0,
				flags = c(.quiet(), "overwrite", "t", correctTopoFlag)
			)
			
		}
		
		info <- .vectInfo(src)
		valid <- .validVector(info, table)

		### automated vector topology correction
		if (!valid & (is.null(snap) | is.null(area))) {
		
			stepsMinus1 <- steps - 1L
			snapRange <- run[grepl(run, pattern = "Estimated range of snapping threshold:")]

			# generic snap range
			if (length(snapRange) == 0L) {

				snapRange <- c(1E-08, 1)

			# GRASS-suggested snap range
			} else {

				snapRange <- substr(snapRange, 41L, nchar(snapRange))
				snapRange <- sub(snapRange, pattern = "\\]", replacement = "")
				snapRange <- strsplit(snapRange, split = ", ")[[1L]]
				# snapRange <- c(snapRange[[1L]], snapRange[[2L]])
				snapRange <- as.numeric(snapRange)

			}

			# create sequence of snap values
			# evenly-spaced in log space bc suggested min/max values are usually several OOMs apart
			snapRange <- log(snapRange)
			snaps <- seq(snapRange[1L], snapRange[2L], length.out = steps - 1L)
			snaps <- exp(snaps)

			digits <- abs(floor(log10(c(snaps[1L]^2))))

			# snap AUTO and area AUTO
			if (is.null(snap) & is.null(area)) {
			
				step <- 1L
				while (!valid & step <= stepsMinus1) {
				
					thisSnap <- snaps[step]
					thisArea <- snaps[step]^2

					if (verbose) {
						
						thisSnapNice <- round(thisSnap, digits)
						thisAreaNice <- round(thisArea, digits)

						omnibus::say("Iteration ", step, ": Snapping at ", thisSnapNice, " map-units and removing polygons of <", thisAreaNice, " m2...")

					}

					src <- .makeSourceName("v_in_ogr", "vector")
					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = thisSnap,
						min_area = thisArea,
						flags = c(.quiet(), "overwrite", "t", correctTopoFlag)
					)
					
					info <- .vectInfo(src)
					valid <- .validVector(info, table)
					step <- step + 1L

				}
				
			# snap AUTO and area NUMERIC
			} else if (is.null(snap) & is.numeric(area)) {
				
				step <- 1L
				while (!valid & step <= stepsMinus1) {
				
					thisSnap <- snaps[step]
					thisArea <- area

					if (verbose) {
						
						thisSnapNice <- round(thisSnap, digits)
						omnibus::say("Iteration ", step, ": Snapping at ", thisSnapNice, " map-units...")

					}

					src <- .makeSourceName("v_in_ogr", "vector")
					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = thisSnap,
						min_area = thisArea,
						flags = c(.quiet(), "overwrite", "t", correctTopoFlag)
					)
					
					info <- .vectInfo(src)
					valid <- .validVector(info, table)
					step <- step + 1L

				}
				
			# snap NUMERIC and area AUTO
			} else if (is.numeric(snap) & is.null(area)) {
				
				step <- 1L
				while (!valid & step <= stepsMinus1) {
				
					if (snap <= 0) {
						thisSnap <- -1
					} else {
						thisSnap <- snap
					}
					thisArea <- snaps[step]^2
					
					if (verbose) {
					
						thisAreaNice <- round(thisArea, digits)
						omnibus::say("Iteration ", step, ": Removing polygons of <", thisAreaNice, " m2...")

					}

					src <- .makeSourceName("v_in_ogr", "vector")
					rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = thisSnap,
						min_area = thisArea,
						flags = c(.quiet(), "overwrite", "t", correctTopoFlag)
					)
					
					info <- .vectInfo(src)
					valid <- .validVector(info, table)
					step <- step + 1L

				}
			
			} # next type of topology correction while loading
	
		} # if not valid and doing automated correction

		if (!valid) {

			if (!is.null(table)) {
			
				if (nrow(table) > info$nGeometries) {

					msg <- paste0("Vector has more geometries than rows in its data table. Try:\n  * Setting the `correct` argument to `TRUE`;\n  * Increasing the value(s) of the `snap` and/or `area` arguments;\n  * Using automated `snap` and/or `area` correction;\n  * Dropping the data table associated with the vector using `dropTable = FALSE`; or\n  * Correcting the vector outside of fasterRaster with `terra::makeValid()` or `sf::st_make_valid()` before using fast().")
					stop(msg)

				} else if (nrow(table) < info$nGeometries) {
				
					msg <- paste0("Vector has more rows in its data table than geometries. Try:\n  * Setting the `correct` argument to `TRUE`;\n  * Decreasing the value(s) of the `snap` and/or `area` arguments;\n  * Using automated `snap` and/or `area` correction;\n  * Dropping the data table associated with the vector using `dropTable = FALSE`; or\n  * Correcting the vector outside of fasterRaster with `terra::makeValid()` or `sf::st_make_valid()` before using fast().")
					stop(msg)
				
				}
			
			}
		} # not valid, so correct
		out <- .makeGVector(src = src, table = table)

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
	function(x, ...) {

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
	function(x, correct = TRUE, snap = NULL, area = NULL, steps = 10, dropTable = FALSE, verbose = FALSE, ...) .fastVector(x, correct = correct, snap = snap, area = area, steps = steps, dropTable = dropTable, verbose = verbose, ...)
)

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "sf"),
	function(x, correct = TRUE, snap = NULL, area = NULL, steps = 10, dropTable = FALSE, verbose = FALSE, ...) .fastVector(x, correct = correct, snap = snap, area = area, steps = steps, dropTable = dropTable, verbose = verbose, ...)
)

# 1. Write vector to disk (if needed)
# 2. Send to fast(signature = "character")
#' @noRd
.fastVector <- function(
	x,		# SpatVector or sf
	correct,
	snap,
	area,
	steps,
	dropTable,
	verbose,
	...
) {

	dots <- list(...)
	dotNames <- names(dots)
	
	if (!inherits(x, "SpatVector")) x <- terra::vect(x)
	xVect <- x

	# remove data frame
	if (dropTable) {
		table <- NULL
	} else {
		table <- data.table::as.data.table(x)
	}

	if (terra::sources(x) == "") {

		# remove table
		if (!is.null(table) && ncol(table) > 0L) {
			nc <- ncol(xVect)
			xVect[ , seq_len(nc)] <- NULL 
		}

		vectFile <- tempfile(fileext = ".gpkg")
		terra::writeVector(xVect, filename = vectFile, filetype = "GPKG", overwrite = TRUE)

	} else {
		vectFile <- terra::sources(x)
	}

	args <- list(x = vectFile, rastOrVect = "vector", correct = correct, snap = snap, area = area, steps = steps, dropTable = dropTable, table = table, xVect = xVect, verbose = verbose)
	args <- c(args, list(...))
	do.call(fast, args = args)
	
}

#' Test if a vector is valid
#'
#' @param x Either the [sources()] name of a **GRASS** vector or a `vectInfo` object created by [.vectInfo()].
#' @param table Either `NULL` (no table) or a `data.frame`, `data.table`, or `matrix`
#'
#' @returns Logical.
#'
#' @noRd
.validVector <- function(x, table) {

	# does table have same number of rows as vector geometries?
	if (!is.null(table) && nrow(table) > 0L) {

		if (!inherits(x, "vectInfo")) x <- .vectInfo(x)
		nGeoms <- x$nGeometries
		tableValid <- nGeoms == nrow(table)

	} else {
		tableValid <- TRUE
	}

	# if the table is valid, are its category numbers valid?
	if (tableValid) {
		if (!inherits(x, "vectInfo")) x <- .vectInfo(x)
		catsValid <- x$catsValid
	} else {
		catsValid <- FALSE
	}

	tableValid & catsValid

}


