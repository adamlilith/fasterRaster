#' Create a GRaster or GVector
#'
#' @description `fast()` imports a raster or vector into the active **GRASS** session and from it creates a `GRaster` or `GVector`.
#'
#' Rasters and vectors can be imported from `SpatRaster`s, `SpatVector`s, or `sf` objects, or from files on disk. **GRASS** supports loading from disk a variety of raster formats (see the **GRASS** manual page for `r.in.gdal`) and vector formats (see the **GRASS** manual page for `v.in.ogr`), though not all of them will work with this function. 
#'
#' Rasters can have "levels" (i.e., categories associated with values). This can be specified using the `levels` argument. If a `SpatRaster` already has levels associated with it, these will be attached to the `GRaster` automatically. Vectors can also have attribute tables, though they do not need to. If they do, there must be one row per geometry (point, line, or polygon).
#'
#' The `fast()` function will project a raster or vector to the current "[location's][tutorial_sessions]" coordinate reference system. Users can specify how to do this using the `method`, `fallback`, and `wrap` arguments.
#'
#' @param x Any one of:
#' * A `SpatRaster` raster. Rasters can have one or more layers. They will retain their "layerdness" in most **fasterRaster** functions.
#' * A `SpatVector` or `sf` spatial vector.
#' * A character string with the path and filename of a raster or vector to be loaded directly into **GRASS**. The function will attempt to ascertain the type of object from the file extension (raster or vector), but it can help to indicate which it is using the `rastOrVect` argument if it is unclear.
#'
#' @param checkCRS Logical: If `TRUE` (default), compare the coordinate reference system (CRS) of the raster or vector to that of the current location. If it is different, then project the raster or vector while importing. If `FALSE`, ignore differences in CRS (if any), and import the raster or vector.
#' 
#' @param method Character or `NULL` (rasters only): If `x` does not have the same coordinate reference system as the currently active **GRASS** "[location][tutorial_sessions]", then it will be projected when it is imported. You may need to specify which method is used to conduction the transformation. Partial matching is used.
#' * `NULL` (default): Automatically choose based on raster properties (`near` for categorical data, `bilinear` for continuous data)
#' * `"near"`: Nearest neighbor. Best for categorical data, and often a poor choice for continuous data.
#' * `"bilinear"`: Bilinear interpolation (default for non-categorical data; uses weighted values from 4 cells).
#' * `"bicubic"`: Bicubic interpolation (uses weighted values from 16 cells).
#' * `"lanczos"`: Lanczos interpolation (uses weighted values from 25 cells).
#'
#' @param fallback Logical (rasters only): If `TRUE` (default), then use "lower" resampling methods to fill in `NA` cells when a "higher" method is used. For example, if `method = "bicubic"`, `NA` cells will be filled in using the `bilinear` method, except when that results in `NA`s, in which case the `near` method will be used. Fallback causes fewer cells to revert to `NA` values, so may be better at capturing complex "edges" (e.g., coastlines). Fallback does increase processing time because each "lower" method must be applied, then results merged.
#'
#' @param wrap Logical (rasters only): When projecting rasters that "wrap around" (i.e., whole-world rasters or rasters that have edges that actually circle around to meet on the globe), `wrap` should be `TRUE` to avoid removing rows and columns from the "edge" of the map. The default is `FALSE`.
#'
#' @param snap `NULL` (default) or a positive numeric value (vectors only): Sometimes, polygons created in other software are topologically incorrect--the borders of adjacent polygons may cross one another, or there may be small gaps between them. These errors can be corrected by slightly moving vertices. The value of `snap` indicates how close vertices need to be for them to be shifted to to the same location for a correct topology. Small values are recommended, and units are in map units (usually meters). By default, this is `NULL`, meaning no snapping is done. Vectors that have been snapped may need to be cleaned using [cleanGeom()] with the `break`, `duplicated`, and `smallAngles` tools.
#'
#' @param warn Logical: If `TRUE`, display a warning when projecting the vector or raster.
#' 
#' @param ... Other arguments. These are typically used internally so not of use to most users. They can include:
#' * `rastOrVect` Character (`"raster"` or `"vector"`). If `x` is a filename, then the function will try to ascertain whether it represents a raster or a vector, but sometimes this will fail. In that case, it can help to specify if the file holds a raster or vector. Partial matching is used.
#'
#' * `levels` (`GRaster`s): A `data.frame`, `data.table`, or list of `data.frame`s or `data.table`s with categories for categorical rasters: The first column of a table corresponds to raster values. A subsequent column corresponds to category labels. By default, the second column is assumed to represent labels, but his can be changed with `[activeCat<-]`. Tables can also be `NULL` (e.g., `data.fame(NULL)`).
#'
#' * `table` (`GVector`s): A `data.frame` or `data.table` with one row per geometry in a `GVector`: Serves as an attribute table.
#'
#' @details When projecting a raster, the "fallback" methods in `r.import` are actually used, even though the `method` argument takes the strings for non-fallback methods. See the manual page for the `r.import` **GRASS** module.
#' 
#' @seealso [rgrass::read_RAST()] and [rgrass::read_VECT()], plus **GRASS** modules `r.in.gdal`, `r.import`, and `v.in.ogr`.
#'
#' @return A `GRaster` or `GVector`.
#'
#' @example man/examples/ex_faster.r
#'
#' @aliases fast
#' @rdname fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "character"),
	function(
		x,
		checkCRS = TRUE,
		method = NULL,
		fallback = TRUE,
		wrap = FALSE,
		snap = NULL,
		warn = TRUE,
		...
	) {

	dots <- list(...)

	if (any(names(dots) == "rastOrVect")) {
		rastOrVect <- dots$rastOrVect
	} else {
		rastOrVect <- NULL
	}

	### raster or vector?
	#####################
	if (is.null(rastOrVect)) {

		### attempt to get type from extension
		# 3-letter extensions
		rastExtensions <- c("asc", "grd", "img", "mem", "tif", "saga")
		vectExtensions <- c("shp", "gpkg")

		ext <- .fileExt(x)
		ext <- tolower(ext)

		if (ext %in% rastExtensions) {
			rastOrVect <- "raster"
		} else if (ext %in% vectExtensions) {
			rastOrVect <- "vector"
		} else {
			stop("Cannot determine data if raster or vector from file name. Please use argument ", sQuote("rastOrVect"), ".")
		}

	} else {
		### user supplied rastOrVect
		rastOrVect <- omnibus::pmatchSafe(rastOrVect, c("raster", "vector"))
	}

	### raster from disk
	####################
	
	if (rastOrVect == "raster") {
		
		xRast <- terra::rast(x)
		nLayers <- terra::nlyr(xRast)
		xNames <- names(xRast)

		### load raster (no projecting needed)
		# if (!checkCRS || terra::crs(xRast) == crs()) {
		if (!checkCRS || terra::same.crs(xRast, crs())) {
			
			region(xRast)
			
			src <- .makeSourceName("r_in_gdal", type = "raster")
			args <- list(
				cmd = "r.in.gdal",
				input = x,
				output = src,
				flags = .quiet()
			)

			if (!checkCRS) args$flags <- c(args$flags, "o")
		
		### load and project raster
		} else {
		
			if (warn) warning("Raster has a different coordinate reference system than this GRASS ", dQuote("location"), ".\n  Raster will be projected to the current location\'s coordinate reference system.")

			# method
			if (!is.null(method)) method <- omnibus::pmatchSafe(method, c("nearest", "bilinear", "bicubic", "lanczos"))

			if (is.null(method)) {

				method <- if (!is.null(levels)) {
					"nearest"
				} else {
					"bilinear"
				}

			}

			if (!is.null(levels) & method != "nearest") warning("At least one raster is categorical but non-nearest resampling method has been selected. Values may not correspond to categories.")

			if (method != "nearest" & fallback) method <- paste0(method, "_f")

			# define region settings using a projected SpatRaster that matches x
			xRast <- xRast[[1L]]
cat("Time-consuming step here for large rasters:")
			xRast <- xRast * NA
			xRast <- terra::project(xRast, crs(), align = TRUE)
cat("Time-consuming step here for large rasters^^^")
			region(xRast)

   			src <- .makeSourceName("r_import", type = "raster")
			args <- list(
				cmd = "r.import",
				input = x,
				output = src,
				resample = method,
				memory = getFastOptions("memory"),
				extent = "region",
				resolution = "region",
				flags = c(.quiet(), "overwrite")
			)

			if (wrap) args$flags <- c(args$flags, "n")
		
		} # projected raster from disk

		do.call(rgrass::execGRASS, args = args)
		if (nLayers > 1L) src <- paste0(src, ".", seq_len(nLayers))

		# raster levels
		if (any(names(dots) == "levels")) {
			if (!is.null(levels)) levels <- dots$levels
		} else {
			levels <- NULL
		}
		out <- .makeGRaster(src, names = xNames, levels = levels)

	### vector from disk
	### (and project on the fly if needed)
	######################################
	
	} else if (rastOrVect == "vector") {

		xVect <- terra::vect(x)
		
		# table from ...
		if (any(names(dots) == "table")) {
			
			table <- dots$table
		
		# if loading from file, get attribute table and strip it from the vector
		} else if (dim(xVect)[2L] > 0L) {

			# xVect$frKEY <- 1L:dim(xVect)[1L]
			# xVect <- terra::disagg(xVext)
			table <- terra::as.data.frame(xVect)
			table <- data.table::as.data.table(table)
			# table[ , names(table) := lapply(.SD, function(x) gsub("\\|", "-", x))]
			# nc <- ncol(xVect)
			# xVect[ , 1L:nc] <- NULL
		
		} else {
			table <- NULL
		}
		
		xCrs <- terra::crs(xVect)
		currentCrs <- crs()

		# vector is in same CRS as current location
		# if (!checkCRS || xCrs !=currentCrs)) {
		if (!checkCRS || terra::same.crs(xCrs, currentCrs)) {

			src <- .makeSourceName("v_in_ogr", type = "vector")
			args <- list(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				flags = c(.quiet(), "overwrite")
			)

			if (!checkCRS) args$flags <- c(args$flags, "o")

		# vector is in different CRS from current location
		# ... need to import to a temporary location, then project to the current location
		} else {

			if (warn) warning("Vector has a different coordinate reference system than this GRASS ", dQuote("location"), ".\n  Vector will be projected to the current location\'s coordinate reference system.")

			src <- .makeSourceName("v_import", type = "vector")
			args <- list(
				cmd = "v.import",
				input = x,
				output = src,
				extent = "input",
				flags = c(.quiet(), "overwrite")
			)
			
			if (!is.null(snap)) args$snap <- snap

		} # vector on disk needs projected

		do.call(rgrass::execGRASS, args = args)
		out <- .makeGVector(src, table = table)

	} # is vector on disk
	out

	} # EOF
)

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "SpatRaster"),
	function(
		x,
		checkCRS = TRUE,
		method = NULL,
		fallback = TRUE,
		wrap = FALSE,
		warn = TRUE
	) {

	rastFile <- terra::sources(x)
	levels <- .getLevels(x)

	if (any(rastFile == "")) {
		tempFile <- tempfile(fileext = ".tif")
		terra::writeRaster(x, filename = tempFile, overwrite = TRUE, datatype = datatype(x, "terra", forceDouble = TRUE))
		x <- tempFile
	} else {
		x <- terra::sources(x)
	}

	fast(x = x, method = method, fallback = fallback, wrap = wrap, warn = warn, levels = levels, rastOrVect = "raster")

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
				levels <- data.table::fread(x, nThread = getFastOptions("cores"), na.strings = "<not defined>", showProgress = FALSE)
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
	function(x, checkCRS = TRUE, snap = NULL, warn = TRUE) .fastVector(x, checkCRS = checkCRS, snap = snap, warn = warn)
)

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "sf"),
	function(x, checkCRS = TRUE, snap = NULL, warn = TRUE) .fastVector(x, checkCRS = checkCRS, snap = snap, warn = warn)
)

# 1. Write vector to disk (if needed)
# 2. Send to fast(signature = "character")
.fastVector <- function(
	x,		# SpatVector or sf
	checkCRS, # Logical
	snap,	# NULL or numeric
	warn	# logical
) {
    
	if (!inherits(x, "SpatVector")) x <- terra::vect(x)
	# x$frKEY <- 1L:dim(x)[1L]

	table <- data.table::as.data.table(x)
	
	# remove data frame
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
    
  fast(x = vectFile, checkCRS = checkCRS, snap = snap, warn = warn, table = table, rastOrVect = "vector")
	
}
