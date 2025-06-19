#' @title Create a GRaster or GVector
#'
#' @description `fast()` creates a `GRaster` or `GVector` from 1) a file; 2) from a `SpatRaster`, `SpatVector`, or `sf` vector; or 3) from a numeric vector, `matrix`, `data.frame`, or `data.table`. Behind the scenes, this function will also create a connection to **GRASS** if none has yet been made yet.
#'
#' **GRASS** supports loading from disk a variety of raster formats (see the **GRASS** manual page for `r.in.gdal` (see `grassHelp("r.in.gdal")`) and vector formats `v.in.ogr` (see grassHelp("v.in.ogr")`), though not all of them will work with this function.
#'
#' Note that `GVectors` may fail to be created if they contain issues that do not coincide with the topological data model used by **GRASS**. The most common of these is overlapping polygons. See *Details* on how to fix these kinds of issues.
#'
#' Note also that **GRASS** (and thus, **fasterRaster**) is *not* very fast when loading vectors. So, if the vector is large and you only want a portion of it, consider using the `extent` argument to load the spatial subset you need.
#'
#' @param x Any one of:
#' * A `SpatRaster` raster. Rasters can have one or more layers.
#' * A `SpatVector` or `sf` spatial vector. See especially arguments `correct`, `area`, `snap`, `steps`, and `verbose`.
#' * A character string or a vector of strings with the path(s) and filename(s) of one or more rasters or one vector to be loaded directly into **GRASS**. The function will attempt to ascertain the type of object from the file extension (raster or vector), but it can help to indicate which it is using the `rastOrVect` argument if it is unclear. For rasters, see especially argument `levels`. For vectors, see especially arguments `correct`, `resolve`, `area`, `snap`, `steps`, and `verbose`.
#' * A vector with an even number of numeric values representing longitude/latitude pairs. See arguments `geom`, `keepgeom`, and `crs`.
#' * A `data.frame`, `data.table`, or `matrix`: Create a `points` `GVector`. Two of the columns must represent longitude and latitude. See arguments `geom`, `keepgeom`, and `crs`.
#' * Missing: Creates a generic `GRaster` or `GVector`. You must specify `rastOrVect`; for example, `fast(rastOrVect = "raster")`. Also see argument `crs`.
#'
#' @param rastOrVect Either `NULL` (default), or `"raster"` or `"vector"`: If `x` is a filename, then the function will try to ascertain whether it represents a raster or a vector, but sometimes this will fail. In that case, it can help to specify if the file holds a raster or vector. Partial matching is used.
#'
#' @param levels (`GRaster`s only): Any of:
#' * Logical: If `TRUE` (default) and at least one layer of a raster is of type `integer`, search for a "levels" file, load it, and attach levels. A levels file will have the same name as the raster file, but end with any of "rdata", "rdat", "rda", "rds", "csv", or "tab" (case will generally not matter). If such a file is not found, no levels will be assigned. The levels file must contain either a `data.frame`, `data.table`, or `list` of `data.frame`s or `data.table`s, or `NULL`.
#' * A `data.frame`, `data.table`, or list of `data.frame`s or `data.table`s with categories for categorical rasters. The first column of a table corresponds to raster values and must be of type `integer`. A subsequent column corresponds to category labels. By default, the second column is assumed to represent labels, but this can be changed with \code{\link[fasterRaster]{activeCat<-}}. Level tables can also be `NULL` (e.g., `data.fame(NULL)`). You can also assign levels after loading a raster using \code{\link[fasterRaster]{levels<-}}.
#' * `NULL`: Do not attach a levels table.
#'#'
#' @param geom Character or integer vector: If `x` is a `data.frame`, `data.table`, or `matrix`, this specifies which columns of `x` represent longitude and latitude. Columns can be given by name (a character vector) or index (a numeric or integer vector). The default is to use the first two columns of `x`.
#'
#' @param crs String: Coordinate reference system (CRS) WKT2 string. This argument is used for creating a `GVector` from a `numeric` vector or a `data.frame` or similar, or from `fast(rastOrVect = "vector")` or `fast(rastOrVect = "raster")`. By default, the function will use the value of [crs()] (no arguments), which is the CRS of the current **GRASS** "project/location" (see `vignette("projects_mapsets", package = "fasterRaster")`).
#'
#' @param keepgeom Logical: If `x` is a set of `numeric` coordinates, or a `data.frame` or similar, then they can be coerced into a `points` `GVector`. If `keepgeom` is `TRUE`, then the coordinates will be included in the data table of the `GVector`. The default is `FALSE`.
#'
#' @param extent (`GVector`s only): Either a `NULL` (default), or a `GVector`, a `SpatVector`, a `SpatExtent` object, an `sf` vector, a `bbox` object, or a numeric vector of 4 values providing a bounding box. If provided, only vector features within this bounding box are imported. If `extent` is a numeric vector, the values *must* be in the order west, east, south, north. If `NULL`, the entire vector is imported.
#'
#' @param correct Logical (`GVector`s only): Correct topological issues. See *Details* for more details! By default, this is `TRUE`.
#'
#' @param snap `GVector`s only: Numeric or `NULL` (default). The value of `snap` indicates how close vertices need to be for them to be shifted to to the same location. Units of `snap` are map units (usually meters), or degrees for unprojected CRSs. For lines and polygons vectors, a value of `NULL` will invoke an iterative procedure to find an optimal, smallest value of `snap`. To turn snapping off, set `snap = 0`. See *Details* for more details!
#'
#' @param area Polygon `GVector`s only: Either a positive numeric value or `NULL` (default). Remove polygons with an area smaller than this value. Units of `area` are in square meters (regardless of the CRS). If `NULL`, then an iterative procedure is used to identify a value of `area` that results in a topologically correct polygon vector. For point and lines vectors, this argument is ignored. To turn area removal off, set `area = 0`. See *Details* for more details! 
#'
#' @param steps `GVector`s only: A positive integer > 1 (default is 10). When using automatic vector correction (i.e., either `snap = NULL` and/or `area = NULL`), this is the number of values of `snap` and/or `area` to try to generate a correct topology, including no snapping or polygon removal (i.e, `snap = 0` and `area = 0`).
#'
#' @param dropTable `GVector`s only: Logical. If `TRUE`, then drop the data table associated with a vector. By default, this is `FALSE`. See *Details* for more details!
#'
#' @param resolve `GVector`s only: Character. If a `GVector` would be topologically invalid after a first attempt at creating it, then this method will be used to resolve the issue and create a valid `GVector`. Partial matching is used.
#' * `"disaggregate"`: Coerce each area of overlap between polygons into its own geometry. The output will not have a data table associated with it.
#' * `"aggregate"`: Coerce all geometries into a "multipart" geometry so it acts like a single geometry. The output will not have a data table associated with it.
#' * `NA` (default): Do neither of the above and if either `snap` or `area` is `NULL`, keep trying to create the `GVector`. Upon success, the `GVector` will retain any data table associated with it unless `dropTable` is `FALSE`.
#'
#' @param verbose `GVector`s only: Logical. Displays progress when using automatic topology correction.
#'
#' @param ... Other arguments::
#' * `table` (`GVector`s--useful mainly to developers, not most users): A `data.frame` or `data.table` with one row per geometry in a `GVector`. Serves as an attribute table.
#' * `xVect` (`GVector`s--useful mainly to developers, not most users): The `SpatVector` that corresponds to the file named by `x`.
#'
#' @details **GRASS** uses a "topological" model for vectors. Topological issues generally arise only with polygon vectors, not point or line vectors. Sometimes, polygons created in other software are topologically incorrect--the borders of adjacent polygons may cross one another, or there may be small gaps between them. These errors can be corrected by slightly shifting vertices and/or removing small polygons that result from intersections of larger ones that border one another. A topological system also recognizes that boundaries to adjacent polygons are shared by the areas, so should not be ascribed attributes that belong to both areas (e.g., the shared border between two countries "belongs" to both countries).
#'
#' By default, `fast()` will try to correct topological errors in vectors. There are three levels of correction, and they are not necessarily mutually exclusive:
#' 1. **Automatic correction**: By default, `fast()` will apply automatic topology correction. You can turn this off using the `correct = FALSE` argument, though in most cases this is not recommended.
#' 2. ***Manual* snapping and/or area removal**: In addition to correction from step 1, you can cause vertices of polygons close to one another to be "snapped" to same place and/or polygons that are smaller than some threshold to be removed. Problems with mis-aligned vertices arise when adjacent polygons are meant to share borders, but slight differences in the locations of the vertices cause them to  mis-align. This mis-alignment can also produce small "slivers" of polygons that are the areas where they overlap. You can snap vertices within a given distance of one another using the `snap` argument followed by a numeric value, like `snap = 0.000001`. Units of `snap` are in map units (usually meters) for projected coordinate reference systems and degrees for unprojected systems (e.g., WGS84, NAD83, NAD27). You can also remove polygons that are smaller than a particular area using the `area` argument followed by a numeric value (e.g., `area = 1`). The units of `area` are in meters-squared, regardless of the coordinate reference system. Note that using `snap` and `area` entails some risk, as it is possible for nearby vertices to actually be distinct and for small areas to be legitimate.
#' 3. **Automatic snapping and/or area removal**: In addition to the correction from step 1, you can use automatic `snap` and/or `area` correction on polygons vectors by setting `snap` and/or `area` to `NULL` (i.e., their default values). If just `snap` is `NULL`, only automatic snapping will be performed, and if just `area` is `NULL`, then only automatic area removal will be performed. Regardless, you will also need to set an integer value for `steps`, which is the number of steps to take between the smallest value of `snap` and/or `area` and the maximum value attempted. The function will then proceed by first attempting `snap = 0` and/or `area = 0` (i.e., no snapping or area removal). If this does not produce a topologically correct vector, **GRASS** will (internally) suggest a range for `snap`. The `fast()` function then creates `steps` values from the lowest to the highest values of this range evenly-spaced along the log values of this range, then proceed to repeat the importing process until either the vector is imported correctly or the maximum value of `snap` is reached and results in a failed topology. Smaller values of `step` will result in more fine-grained attempts so are less likely to yield overcorrection, but can also take more time. The value of `area` in automatic correction is set to `snap^2`. **NB**: Automated snapping and area removal are only performed on polygons vectors, even if `snap` or `area` is `NULL`. To snap lines or points, you must set `snap` equal to a numeric value. The `area` correction is ignored for points and lines.
#'
#' Issues can also arise due to:
#'
#' * **Data table-vector mismatching**: If your vector has a data table ("attribute table") associated with it, errors can occur if there are more/fewer geometries (or multi-geometries) per row in the table. If you do not really need the data table to do your analysis, you can remove it (and thus obviate this error) using `dropTable = TRUE`.
#' * **Dissolving or aggregating "invalid" geometries**: Using the `resolve` argument, you can create a topologically valid vector by either coercing all overlapping portions of polygons into their own geometries (`resolve = "disaggregate"`), or by coercing them into a single, combined geometry (`resolve = "aggregate"`). Aggregation/disaggregation will be implemented after loading the vector into **GRASS** using the settings given by `snap` and `area`. Aggregation/disaggregation will cause any associated data table to be dropped (it forces `dropTable` to be `TRUE`). The default action is to do neither aggregation nor disaggregation (`resolve = NA`). You can also do this outside **fasterRaster** using [terra::aggregate()] or [terra::disagg()].
#'
#' If none of these fixes work, you can try:
#'
#' * **Correction outside of *fasterRaster***: Before you convert the vector into **fasterRaster**'s `GVector` format, you can also try using the [terra::makeValid()] or [sf::st_make_valid()] tools to fix issues, then use `fast()`. You can also use [terra::aggregate()] or [terra::disagg()] to combine/split problematic geometries.
#' * **Post-load correction**: If you do get a vector loaded into `GVector` format, you can also use a set of **fasterRaster** vector-manipulation [tools][breakPolys] or [fillHoles()] to fix issues.
#'
#' @seealso \code{\link[rgrass]{read_RAST}} and \code{\link[rgrass]{read_VECT}}, [vector cleaning][breakPolys], [fillHoles()], plus **GRASS** modules `v.in.ogr` (see `grassHelp("v.in.ogr")`) and `r.import` (`grassHelp("r.import")`)
#'
#' @returns A `GRaster` or `GVector`.
#'
#' @example man/examples/ex_fast.r
#'
#' @aliases fast,character-method
#' @rdname fast
#' @name fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "character"),
	function(
		x,
		rastOrVect = NULL,
		levels = TRUE,
		extent = NULL,
		correct = TRUE,
		snap = NULL,
		area = NULL,
		steps = 10,
		dropTable = FALSE,
		resolve = NA,
		verbose = TRUE,
		...
	) {

	### debugging
	if (FALSE) {
	
		rastOrVect <- NULL
		levels <- TRUE
		extent <- NULL
		correct <- TRUE
		snap <- NULL
		area <- NULL
		steps <- 10
		dropTable <- FALSE
		resolve <- NA
		verbose <- TRUE
		dots <- list()
	
	}

	if (is.na(faster("grassDir"))) stop("You must specify the folder in which GRASS is installed using faster().")

	# Error for cases where there are fewer geometries than rows in the data table. Topology correction using `snap` or `area` can only the number of geometries, so if the number of geometries is larger than the number of table rows, then iteratively trying larger values of `snap` or `area` will continue to create a mismatch between the number of geometries and table rows. In this case, just throw an error to obviate wasting time.
	nGeometriesVsTableRowsError <- "Since the data table has more rows than there are vector geometries,\n  increasing `snap` and/or `area` can only decrease the number of geometries.\n  Try smaller values of `snap` and/or `area`, or remove the table using\n  `dropTable = TRUE`."

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

		ext <- .fileExt(x[1L])
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

			.region(xRast)
			src <- .makeSourceName("fast_r_external", type = "raster", n = 1L)
			rgrass::execGRASS(
				cmd = "r.external",
				input = x,
				output = src,
				flags = c(.quiet(), "overwrite", "o") # overriding projection check!
			)
			
			if (!.exists(src)) stop("Raster not loaded. Check the file name. You may need to use an absolute (not relative) file path.")
			if (nLayers > 1L) src <- paste0(src, ".", seq_len(nLayers))

			### raster levels
			# if (is.list(levels)) {
			# 	if (length(levels) == 1L & length(xNames) > 1L & is.null(levels[[1L]])) levels <- NULL
			# } else if (inherits(levels, c("data.frame", "data.table"))) {
				# levels <- list(levels)
			if (is.logical(levels) && levels) {
				
				info <- .rastInfo(src)
				
				# search for and add levels
				if (any(info$grassDataType == "CELL")) {

					levelsFileName <- x
					levelsFileExt <- .fileExt(x)
					levelsFileName <- substr(levelsFileName, 1L, nchar(x) - nchar(levelsFileExt))

					# extensions <- c("rds", "RDS", "rdata", "RData", "rda", "RDa", "Rda", "Rdat", "rdat", "RDat", "csv", "CSV", "tab", "TAB")
					extensions <- c("rds", "rdata", "rda", "rdat", "csv", "tab")
					levelsFileNames <- paste0(levelsFileName, extensions)

					fileExists <- file.exists(levelsFileNames)
					if (any(fileExists)) {
					
						# if (sum(fileExists) > 1L) warning("More than one `levels` file found. Only the first will be used.")
						levelsFileName <- levelsFileNames[fileExists]
						extension <- extensions[fileExists]

						if (tolower(extension) == "rds") {
							levels <- readRDS(levelsFileName)
						} else if (tolower(extension) %in% c("rdata", "rda", "rdat")) {
							load(levelsFileName)
						} else if (tolower(extension) == "tab") {
							levels <- utils::read.delim(levelsFileName)
						} else if (tolower(extension) == "csv") {
							levels <- utils::read.csv(levelsFileName)
						}

					} else {
						levels <- NULL
					}

				} else {
					levels <- NULL
				}
			
			}

			if (exists("info", inherits = FALSE)) {
				out <- .makeGRaster(info, names = xNames, levels = levels)
			} else {
				out <- .makeGRaster(src, names = xNames, levels = levels)
			}
			
		}

	### vector from disk
	####################
	
	} else if (rastOrVect == "vector") {

		# x is a filename and xVect is missing: we have NOT come through methods for SpatVectors or sf objects
		if (!any(dotNames == "xVect")) {
			
			x <- terra::vect(x)
			out <- fast(x, extent = extent, correct = correct, snap = snap, area = area, steps = steps, resolve = resolve, dropTable = dropTable, verbose = verbose, ...)

		# x is a filename and xVect is present: we HAVE come here through a method for SpatVectors or sf objects
		} else {

			if (!is.na(resolve)) resolve <- omnibus::pmatchSafe(resolve, c("aggregate", "disaggregate"), n = 1L)

			xVect <- dots$xVect
			if (any(dotNames == "table")) {
				table <- dots$table
			} else {
				table <- NULL
			}

			gtype <- terra::geomtype(xVect)
			if (gtype == "points") {
				gtype <- "point"
			} else if (gtype == "lines") {
				gtype <- "line"
			} else if (gtype == "polygons") {
				gtype <- "area"
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
			if (is.null(snap) & gtype == "point") snap <- -1
			if (is.null(area) & gtype %in% c("line", "point")) area <- 0

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

			if ((verbose | faster("verbose")) & gtype == "area") {
				omnibus::say("Creating GVector with ", thisSnapNice, " snapping and ", thisAreaNice, "...")
			}

			src <- .makeSourceName("fast_v_in_ogr", "vector")
			if (is.null(snap) & (is.null(area) || area == 0)) {

				# slower if we need to record messages
				suppressMessages(
					assessment <- rgrass::execGRASS(
						cmd = "v.in.ogr",
						input = x,
						output = src,
						snap = thisSnap,
						min_area = thisArea,
						flags = c("verbose", "overwrite", "t", "o", correctTopoFlag),
						ignore.stderr = FALSE,
						Sys_show.output.on.console = FALSE,
						echoCmd = FALSE, # displays GRASS command
						intern = TRUE
					)
				)

				valid <- !any(c(
					grepl(assessment, pattern = "WARNING: The output contains topological errors"),
					grepl(assessment, pattern = "Invalid argument"),
					assessment == "1"
				))

				if (valid) { # more thorough test... slower

					info <- .vectInfo(src)
					valid <- .validVector(info, table)
				
					if (!valid & !is.na(resolve)) {

						table <- NULL
						src <- .aggDisaggVect(src, resolve = resolve, verbose = verbose)

						# info <- .vectInfo(src)
						# valid <- .validVector(info, table)
						valid <- TRUE

					}

					if (!valid & verbose & !dropTable) {
						omnibus::say("   Vector has ", info$nGeometries, " valid geometries, ", sum(is.na(info$cats)), " invalid geometries, and ", nrow(table), " rows in its data table.")
						if (info$nGeometries < nrow(table)) {
							stop(nGeometriesVsTableRowsError)
						}
					} else if (!valid & verbose) {
						omnibus::say("   Vector has ", info$nGeometries, " valid geometries and ", sum(is.na(info$cats)), " invalid geometries.")
					}

				}

			} else {

				# faster if we remove fluff
				rgrass::execGRASS(
					cmd = "v.in.ogr",
					input = x,
					output = src,
					snap = thisSnap,
					min_area = thisArea,
					flags = c(.quiet(), "overwrite", "t", correctTopoFlag)
				)

				# if (!exists("table", inherits = TRUE, mode = "numeric")) table <- .vAsDataTable(src)
				info <- .vectInfo(src)
				valid <- .validVector(info, table)

				if (!valid & !is.na(resolve)) {

					table <- NULL
					src <- .aggDisaggVect(src, resolve = resolve, verbose = verbose)

					# info <- .vectInfo(src)
					# valid <- .validVector(info, table)
					valid <- TRUE

				}

				if (!valid & verbose & !dropTable) {
					omnibus::say("   Vector has ", info$nGeometries, " valid geometries, ", sum(is.na(info$cats)), " invalid geometries, and ", nrow(table), " rows in its data table.")
						if (info$nGeometries < nrow(table)) {
							stop(nGeometriesVsTableRowsError)
						}
				} else if (!valid & verbose) {
					omnibus::say("   Vector has ", info$nGeometries, " valid geometries and ", sum(is.na(info$cats)), " invalid geometries.")
				}

			}
			
			### automated vector topology correction
			if (!valid & (is.null(snap) | is.null(area))) {
			
				if (!exists("assessment", inherits = FALSE)) {
				
					suppressMessages(
						assessment <- rgrass::execGRASS(
							cmd = "v.in.ogr",
							input = x,
							output = src,
							snap = thisSnap,
							min_area = thisArea,
							flags = c("verbose", "overwrite", "t", "o", correctTopoFlag),
							ignore.stderr = FALSE,
							Sys_show.output.on.console = FALSE,
							echoCmd = FALSE, # displays GRASS command
							intern = TRUE
						)
					)
				
				}

				stepsMinus1 <- steps - 1L
				snapRange <- assessment[grepl(assessment, pattern = "Estimated range of snapping threshold:")]

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

						if ((verbose | faster("verbose")) & gtype == "area") {
							
							thisSnapNice <- round(thisSnap, digits)
							thisAreaNice <- round(thisArea, digits)

							omnibus::say("Iteration ", step, ": Snapping at ", thisSnapNice, " map-units and removing polygons of <", thisAreaNice, " m2...")

						}

						src <- .makeSourceName("fast_v_in_ogr", "vector")
						rgrass::execGRASS(
							cmd = "v.in.ogr",
							input = x,
							output = src,
							snap = thisSnap,
							min_area = thisArea,
							flags = c(.quiet(), "overwrite", "t", correctTopoFlag)
						)
						
						made <- .exists(src)
						if (made) {
							info <- .vectInfo(src)
							valid <- .validVector(info, table)
						} else {
							valid <- FALSE
						}

						if (made & !valid & !is.na(resolve)) {

							table <- NULL
							src <- .aggDisaggVect(src, resolve = resolve, verbose = verbose)

							# info <- .vectInfo(src)
							# valid <- .validVector(info, table)
							valid <- TRUE

						}

						if (made & !valid & verbose & !dropTable) {
							omnibus::say("   Vector has ", info$nGeometries, " valid geometries, ", sum(is.na(info$cats)), " invalid geometries, and ", nrow(table), " rows in its data table.")
							if (info$nGeometries < nrow(table)) {
								stop(nGeometriesVsTableRowsError)
							}
						} else if (made & !valid & verbose) {
							omnibus::say("   Vector has ", info$nGeometries, " valid geometries and ", sum(is.na(info$cats)), " invalid geometries.")
						} else if (!made) {
							omnibus::say("   Failed to create vector.")
						}

						step <- step + 1L

					}
					
				# snap AUTO and area NUMERIC
				} else if (is.null(snap) & is.numeric(area)) {
					
					step <- 1L
					while (!valid & step <= stepsMinus1) {
					
						thisSnap <- snaps[step]
						thisArea <- area

						if ((verbose | faster("verbose")) & gtype == "area") {
							
							thisSnapNice <- round(thisSnap, digits)
							omnibus::say("Iteration ", step, ": Snapping at ", thisSnapNice, " map-units...")

						}

						src <- .makeSourceName("fast_v_in_ogr", "vector")
						rgrass::execGRASS(
							cmd = "v.in.ogr",
							input = x,
							output = src,
							snap = thisSnap,
							min_area = thisArea,
							flags = c(.quiet(), "overwrite", "t", correctTopoFlag)
						)
						
						made <- .exists(src)

						if (made) {
							info <- .vectInfo(src)
							valid <- .validVector(info, table)
						} else {
							valid <- FALSE
						}

						if (made & !valid & !is.na(resolve)) {

							table <- NULL
							src <- .aggDisaggVect(src, resolve = resolve, verbose = verbose)

							# info <- .vectInfo(src)
							# valid <- .validVector(info, table)
							valid <- TRUE

						}

						if (made & !valid & verbose & !dropTable) {
							omnibus::say("   Vector has ", info$nGeometries, " valid geometries, ", sum(is.na(info$cats)), " invalid geometries, and ", nrow(table), " rows in its data table.")
							if (info$nGeometries < nrow(table)) {
								stop(nGeometriesVsTableRowsError)
							}
						} else if (made & !valid & verbose) {
							omnibus::say("   Vector has ", info$nGeometries, " valid geometries and ", sum(is.na(info$cats)), " invalid geometries.")
						} else if (!made) {
							omnibus::say("   Failed to create vector.")
						}
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
						
						if ((verbose | faster("verbose")) & gtype == "area") {
						
							thisAreaNice <- round(thisArea, digits)
							omnibus::say("Iteration ", step, ": Removing polygons of <", thisAreaNice, " m2...")

						}

						src <- .makeSourceName("fast_v_in_ogr", "vector")
						rgrass::execGRASS(
							cmd = "v.in.ogr",
							input = x,
							output = src,
							snap = thisSnap,
							min_area = thisArea,
							flags = c(.quiet(), "overwrite", "t", correctTopoFlag)
						)
						
						made <- .exists(src)
						if (made) {
							info <- .vectInfo(src)
							valid <- .validVector(info, table)
							if (!exists("table", inherits = TRUE)) table <- .vAsDataTable(src)
						} else {
							valid <- FALSE
						}

						if (made & !valid & !is.na(resolve)) {

							table <- NULL
							src <- .aggDisaggVect(src, resolve = resolve, verbose = verbose)

							# info <- .vectInfo(src)
							# valid <- .validVector(info, table)
							valid <- TRUE

						}

						if (made & !valid & verbose & !dropTable) {
							omnibus::say("   Vector has ", info$nGeometries, " valid geometries, ", sum(is.na(info$cats)), " invalid geometries, and ", nrow(table), " rows in its data table.")
							if (info$nGeometries < nrow(table)) {
								stop(nGeometriesVsTableRowsError)
							}
						} else if (made & !valid & verbose) {
							omnibus::say("   Vector has ", info$nGeometries, " valid geometries and ", sum(is.na(info$cats)), " invalid geometries.")
						} else if (!made) {
							omnibus::say("   Failed to create vector.")
						}
						step <- step + 1L

					}
				
				} # next type of topology correction while loading
		
			} # if not valid and doing automated correction

			if (!valid) {

				msg <- paste0("Vector has an invalid topology. Try:\n  * Setting the `correct` argument to `TRUE`;\n  * Increasing the value(s) of the `snap` and/or `area` arguments;\n  * Using automated `snap` and/or `area` correction (set `snap` and/or `area` to NULL);\n  * Dropping the data table associated with the vector using `dropTable = TRUE`;\n  * Aggregating or disaggregating polygons with the `resolve` argument; or\n  * Attempting correction of the vector outside of fasterRaster with `terra::makeValid()` or `sf::st_make_valid()` before using fast().")

				stop(msg)

			}

			out <- .makeGVector(src = src, table = table)
			
			# # save/reload vector... seems to fix errors with subset_single_bracket on polygon vectors later on
			# if (reload & geomtype(out) == "polygons") {

			# 	if (verbose | faster("verbose")) omnibus::say('Reloading GVector...')
			# 	tempVect <- tempfile(fileext = ".gpkg")

			# 	args <- list(
			# 		cmd = "v.out.ogr",
			# 		input = sources(out),
			# 		output = tempVect,
			# 		format = "GPKG",
			# 		flags = c(.quiet(), "s", "overwrite")
			# 		# flags = c(.quiet(), "s", "c") # "c" ==> save geometries lacking a cat number
			# 	)
			# 	do.call(rgrass::execGRASS, args)

			# 	Sys.sleep(0.5)
			# 	src <- .makeSourceName("v_in_ogr", "vector")
			# 	rgrass::execGRASS(
			# 		cmd = "v.in.ogr",
			# 		input = tempVect,
			# 		output = src,
			# 		flags = c(.quiet(), "overwrite", "t", "c")
			# 	)
			# 	out <- .makeGVector(src, table = table)

			# 	# writeVector(out, filename = tempVect, overwrite = TRUE)
			# 	# out <- fast(tempVect, reload = FALSE, table = table) # cannot use vect() on this... weird!
			
			# }

			if ((verbose | faster("verbose")) & geomtype(out) == "polygons") omnibus::say("Topologically valid vector created.")

		} # x is a filename and xVect supplied

	}
	out

	} # EOF
)

#' @aliases fast,SpatRaster-method
#' @rdname fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "SpatRaster"),
	function(x, ...) {

	if (is.na(faster("grassDir"))) stop("Before using fasterRaster functions, you must specify the folder in which GRASS is installed using faster().")

	rastFile <- terra::sources(x)
	levels <- .getLevels(x)

	if (any(rastFile == "")) {

		tempFile <- tempfile(fileext = ".tif")
		terra::writeRaster(x, filename = tempFile, overwrite = TRUE)
		rastFile <- tempFile

	}

	# if this SpatRaster is a subset of rasters in a file, save the subset to disk... otherwise, fast(signature = 'character') will load the entire set of layers and fail to match the levels() from the subset
	thisRast <- terra::rast(rastFile)
	nlay <- terra::nlyr(thisRast)
	nlev <- length(levels)

	if (nlay != nlev) {

		tempFile <- tempfile(fileext = ".tif")
		terra::writeRaster(x, filename = tempFile, overwrite = TRUE)
		rastFile <- tempFile

	}

	fast(x = rastFile, rastOrVect = "raster", levels = levels)

	} # EOF
)

#' @aliases fast,SpatVector-method
#' @rdname fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "SpatVector"),
	function(x, extent = NULL, correct = TRUE, snap = NULL, area = NULL, steps = 10, dropTable = FALSE, resolve = NA, verbose = TRUE) .fastVector(x, extent = extent, correct = correct, snap = snap, area = area, steps = steps, dropTable = dropTable, resolve = resolve, verbose = verbose)
)

#' @aliases fast,sf-method
#' @rdname fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "sf"),
	function(x, extent = NULL, correct = TRUE, snap = NULL, area = NULL, steps = 10, resolve = NA, dropTable = FALSE, verbose = TRUE) .fastVector(x, correct = correct, snap = snap, area = area, steps = steps, extent = extent, dropTable = dropTable, resolve = resolve, verbose = verbose)
)

#' @rdname fast
#' @aliases fast,missing-method
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "missing"),
	function(x, rastOrVect, crs = "") {
	
	if (crs == "") { 
	
		crs <- tryCatch(crs(), error = function(cond) FALSE)

		if (is.logical(crs)) stop("You must provide a coordinate reference system with `crs` or\n  have created or loaded at least one GRaster or GVector.")

	}

	rastOrVect <- omnibus::pmatchSafe(rastOrVect, c("raster", "vector"), nmax = 1L)
	if (rastOrVect == "raster") {

		x <- terra::rast()
		x <- terra::project(x, crs)
		x[] <- 1L
		tf <- tempfile(fileext = ".tif")
		terra::writeRaster(x, tf, overwrite = TRUE)
		out <- fast(tf)

	} else {

		x <- c(-180, 180, -90, 90)
		x <- terra::ext(x)
		x <- terra::as.polygons(x, crs = "epsg:4326")
		x <- terra::project(x, crs)
		out <- .fastVector(x, correct = TRUE, snap = FALSE, area = FALSE, steps = 10, extent = NULL, dropTable = FALSE, resolve = NA, verbose = FALSE)

	}
	out

	} # EOF
)

#' @rdname fast
#' @aliases fast,numeric-method
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "numeric"),
	function(x, crs = "", keepgeom = FALSE) {
	
	if (length(x) %% 2L != 0L) stop("You must supply an even number of numeric values, each representing longitude/latitude pairs.")
	x <- matrix(x, ncol = 2L, byrow = TRUE)
	.fastDF(x = x, geom = 1:2, crs = crs, keepgeom = keepgeom)

	} # EOF
)

#' @rdname fast
#' @aliases fast,data.frame-method
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "data.frame"),
	function(x, geom = 1:2, crs = "", keepgeom = FALSE) .fastDF(x = x, geom = geom, crs = crs, keepgeom = keepgeom)
)

#' @rdname fast
#' @aliases fast,data.table-method
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "data.table"),
	function(x, geom = 1:2, crs = "", keepgeom = FALSE) .fastDF(x = x, geom = geom, crs = crs, keepgeom = keepgeom)
)

#' @rdname fast
#' @aliases fast,matrix-method
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "matrix"),
	function(x, geom = 1:2, crs = "", keepgeom = FALSE) .fastDF(x = x, geom = geom, crs = crs, keepgeom = keepgeom)
)

#' Create a points GVector from a data.frame or similar
#'
#' @param x Two numeric values, or a `data.frame`, `data.table`, or `matrix`.
#' @param geom 2-element 
#' @param crs WKT2 string
#' @param keepgeom Logical
#' @noRd
.fastDF <- function(x, geom, crs, keepgeom) {

	if (crs == "") {
	
		crs <- tryCatch(crs(), error=function(cond) FALSE)

		if (is.logical(crs)) stop("You must provide a coordinate reference system with `crs` or\n  have created or loaded at least one GRaster or GVector.")

	}

	if (!is.data.frame(x)) x <- as.data.frame(x)

	if (is.numeric(geom) | is.integer(geom)) {
		xcol <- names(x)[geom[1L]]
		ycol <- names(x)[geom[2L]]
	} else {
		xcol <- geom[1L]
		ycol <- geom[2L]
	}

	xVect <- terra::vect(x, geom = c(xcol, ycol), crs = crs, keepgeom = keepgeom)
	fast(xVect, correct = TRUE)

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

# 1. Write vector to disk (if needed)
# 2. Send to fast(signature = "character")
#' @noRd
.fastVector <- function(
	x,
	extent,
	correct,
	snap,
	area,
	steps,
	dropTable,
	resolve,
	verbose,
	...
) {

	if (is.na(faster("grassDir"))) stop("You must specify the folder in which GRASS is installed using faster().")

	dots <- list(...)
	dotNames <- names(dots)
	
	if (!inherits(x, "SpatVector")) x <- terra::vect(x)
	
	# crop
	if (!is.null(extent)) {
		
		if (inherits(extent, "GSpatial")) {
			extent <- ext(extent)
		} else if (inherits(extent, c("SpatVector", "SpatRaster", "SpatExtent"))) {
			extent <- terra::ext(extent)
		} else if (inherits(extent, "sf")) {
			extent <- sf::st_bbox
			extent <- as.vector(extent)
			extent <- extent[c(1L, 3L, 2L, 4L)]
			extent <- terra::ext(extent)
		} else if (is.numeric(extent)) {
			if (length(extent) != 4L) stop("The `extent` argument must be a spatial object or a vector of four numeric values.")
			extent <- terra::ext(extent)
		} else {
			stop("The `extent` argument must be a spatial object or a vector of four numeric values.")
		}

		x <- terra::crop(x, extent)

		if (nrow(x) == 0L) {
			warning("No geometries occur within the given extent. Returning `NULL`.")
			return(NULL)
		}
		
	}
	
	xVect <- x

	# remove data frame
	if (dropTable) {
		table <- NULL
	} else {
		table <- data.table::as.data.table(xVect)
	}

	if (terra::sources(x) == "") {

		# remove all but one column of data table
		# NB we ***need** a table with the GVector--otherwise, subset_single_bracket does not work as expected
		if (!is.null(table) && ncol(table) > 1L) {
			nc <- 2L:ncol(xVect)
			xVect[ , nc] <- NULL 
		}
		
		# NB we ***need** a table with the GVector--otherwise, subset_single_bracket does not work as expected
		if (is.null(table)) xVect$DUMMYDUMMY_ <- 1L:nrow(xVect)

		vectFile <- paste0(faster("workDir"), "/", omnibus::rstring(1L), ".gpkg")
		terra::writeVector(xVect, filename = vectFile, filetype = "GPKG", overwrite = TRUE)
	
	} else {
		vectFile <- terra::sources(x)
	}

	# sometimes, terra::sources() adds the layer name after "::" to the filename if the vector was saved by v.out.ogr
	if (grepl(vectFile, pattern = "::")) {

		colons <- regexpr(vectFile, pattern = "::", fixed = TRUE)
		vectFile <- substr(vectFile, 1L, colons - 1L)

	}

	# NB not passing extent bc already cropped if we wanted to do that
	args <- list(
		x = vectFile,
		rastOrVect = "vector",
		extent = NULL,
		correct = correct,
		snap = snap,
		area = area,
		steps = steps,
		dropTable = dropTable,
		resolve = resolve,
		table = table,
		xVect = xVect,
		verbose = verbose
	)
	args <- c(args, list(...))
	do.call(fast, args = args)
	
}

