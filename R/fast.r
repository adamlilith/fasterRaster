#' Create a GRaster or GVector
#'
#' @description `fast()` imports a raster or vector into the active **GRASS** session and from it creates a `GRaster` or `GVector`.
#'
#' Rasters can vectors can be imported from `SpatRaster`s, `SpatVector`s, or `sf` objects, or from files on disk. **GRASS** supports loading from disk a variety of raster formats (see the **GRASS** manual page for `r.in.gdal`) and vector formats (see the **GRASS** manual page for `v.in.ogr`), though not all of them will work with this function. 
#'
#' Rasters can have "levels" (i. have categories associated with values). This can be specified using the `levels` argument. If a `SpatRaster` already has levels associated with it, these should be attached to the `GRaster` automatically. Vectors can also have attribute tables, though they do not need to.
#'
#' The `fast()` function will project a raster or vector to the current "[location's][tutorial_sessions]" coordinate reference system. Users can specify how to do this using the `method`, `fallback`, and `wrap` arguments.
#'
#' @param x Any one of:
#' * A `SpatRaster` raster. Rasters can have one or more layers. They will retain their "layerdness" in most **fasterRaster** functions.
#' * A `SpatVector` or `sf` spatial vector.
#' * A character string with the path and filename of a raster or vector to be loaded directly into **GRASS**. The function will attempt to ascertain the type of object from the file extension (raster or vector), but it can help to indicate which it is using the `rastOrVect` argument if it is unclear.
#'
#' @param levels Specifies the [levels()] of a categorical raster:
#'  * `NULL` (default): If `x` is a `SpatRaster` and it already has levels, these will be attached to the `GRaster`. Otherwise, the raster is assumed to have no levels.
#' * An empty string: Same as `NULL` (no levels).
#'  * `data.frame` or `data.table`: Must have at least two columns. The first column must be integers indicating which values in the raster correspond to what categories. The second column is assumed to have category labels. This can be changed using [activeCat<-][activeCat].
#' * A list with one element per raster layer: Each element can be a `data.frame`, `data.table`, or a empty string. A list can have a mix of any of these.
#' * A non-empty string: Specifies name of a file containing a `data.frame`, `data.table`, or `list` with levels. Files can be ".csv", ".tab", ".rds", or ".rda" files.
#'
#' @param table Either `NULL` (default), or a `data.frame` or `data.table`: Metadata associated with each feature in a vector. This is usually not useful for most users, as the attribute table of a `SpatVector` will be automatically copied over from the file, `SpatVector`, or `sf` vector to the `GVector`. Leaving this as `NULL` will copy this table automatically.
#' 
#' @param rastOrVect Either `NULL` (default) or character (`"raster"` or `"vector"`). If `x` is a raster or vector already in **R**, this does not need to be specified. However, if `x` is a filename, then the function will try to ascertain whether it represents a raster or a vector, but sometimes this will fail. In that case, it can help to specify if the file holds a raster or vector. Partial matching is used.
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
#' @param warn Logical: If `TRUE`, display a warning when projecting the vector or raster.
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
		rastOrVect = NULL,
		levels = NULL,
		table = NULL,
		method = NULL,
		fallback = TRUE,
		wrap = FALSE,
		warn = TRUE
	) {

	### raster or vector?
	#####################
	if (is.null(rastOrVect)) {

		### attempt to get type from extension
		nc <- nchar(x)

		# 3-letter extensions
		rastExtensions <- c(".asc", ".grd", ".img", ".mem", ".tif")
		vectExtensions <- c(".shp")

		ext <- substr(x, nc - 3L, nc)
		ext <- tolower(ext)

		if (ext %in% rastExtensions) {
			rastOrVect <- "raster"
		} else if (ext %in% vectExtensions) {
			rastOrVect <- "vector"
		}

		# 4-letter extensions
		rastExtensions <- c(".saga")
		vectExtensions <- c(".gpkg")

		ext <- substr(x, nc - 4L, nc)
		ext <- tolower(ext)

		if (ext %in% rastExtensions) {
			rastOrVect <- "raster"
		} else if (ext %in% vectExtensions) {
			rastOrVect <- "vector"
		}

		if (is.null(rastOrVect)) stop("Cannot determine data if raster or vector from file name. Please use argument ", sQuote("rastOrVect"), ".")

	} else {
	### user supplied rastOrVect
		rastOrVect <- pmatchSafe(rastOrVect, c("raster", "vector"))
	}

	### raster from disk
	####################
	
	if (rastOrVect == "raster") {

		xRast <- terra::rast(x)
		nLayers <- terra::nlyr(xRast)
		xNames <- names(xRast)

		### load raster (no projecting needed)
		if (terra::crs(xRast) == crs()) {

			region(xRast)

			src <- .makeSourceName("r.in.gdal", rastOrVect = "raster")
			args <- list(
				cmd = "r.in.gdal",
				input = x,
				output = src,
				flags = "quiet",
				intern = TRUE
			)

		### load and project raster
		} else {
		
			if (warn) warning("Raster has a different coordinate reference system than this GRASS ", dQuote("location"), ".\n  Raster will be projected to the current location\"s coordinate reference system.")

			# method
			if (!is.null(method)) method <- pmatchSafe(method, c("nearest", "bilinear", "bicubic", "lanczos"))

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
			xRast <- xRast * NA_real_
			xRast <- terra::project(xRast, crs(), align = TRUE)
cat("Time-consuming step here for large rasters^^^")
			region(xRast)

   			src <- .makeSourceName("r.import", rastOrVect = "raster")
			args <- list(
				cmd = "r.import",
				input = x,
				output = src,
				resample = method,
				memory = getFastOptions("memory"),
				extent = "region",
				resolution = "region",
				flags = c("quiet", "overwrite"),
				intern = TRUE
			)

			if (wrap) args$flags <- c(args$flags, "n")
		
		} # projected raster from disk

		do.call(rgrass::execGRASS, args = args)
		if (nLayers > 1L) src <- paste0(src, ".", seq_len(nLayers))

		# raster levels
		if (!is.null(levels)) levels <- .getLevels(levels)
		out <- .makeGRaster(src, names = xNames, levels = levels)

	### vector from disk (and project on the fly if needed)
	#######################################################
	
	} else if (rastOrVect == "vector") {

		xVect <- terra::vect(x)
		
		# if loading from file, get attribute table and strip it from the vector
		if (is.null(table)) {
		
			table <- terra::as.data.frame(xVect)
			table <- data.table::as.data.table(table)
			xVect <- tidyterra::transmute(xVect)
		
		}
		
		xCrs <- terra::crs(xVect)
		currentCrs <- crs()
		src <- .makeSourceName(xVect, rastOrVect = "vector")

		# vector is in same CRS as current location
		if (xCrs == currentCrs) {

			args <- list(
				cmd = "v.in.ogr",
				input = x,
				output = src,
				flags = c("quiet", "overwrite"),
				intern=TRUE
			)

		# vector is in different CRS from current location
		# ... need to import to a temporary location, then project to the current location
		} else {

			if (warn) warning("Vector has a different coordinate reference system than this GRASS ", dQuote("location"), ".\n  Vector will be projected to the current location\'s coordinate reference system.")

			args <- list(
				cmd = "v.import",
				input = x,
				output = src,
				extent = "input",
				flags = c("quiet", "overwrite"),
				intern = TRUE
			)

		} # vector on disk needs projected

		do.call(rgrass::execGRASS, args=args)
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
		method = NULL,
		fallback = TRUE,
		wrap = FALSE,
		warn = TRUE
	) {

	rastFile <- terra::sources(x)
	levs <- .getLevels(x)

	if (!is.null(levs) && !any(sapply(levs, is.null)) && any(.nlevels(levs) > 0L)) {

		levelsFile <- tempfile(fileext = ".rds")
		levelsFile <- forwardSlash(levelsFile)
  		saveRDS(levs, file = levelsFile)

	} else {
		levelsFile <- NULL
	}

	if (any(rastFile == "")) {
		tempFile <- tempfile(fileext = ".tif")
		terra::writeRaster(x, tempFile, overwrite = TRUE, datatype = datatype(x, "terra"))
		x <- tempFile
	} else {
		x <- terra::sources(x)
	}

	fast(x = x, levels = levelsFile, rastOrVect = "raster", method = method, fallback = fallback, wrap = wrap, warn = warn)

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
	function(x, warn = TRUE) .fastVector(x, warn = warn)
)

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "sf"),
	function(x, warn = TRUE) .fastVector(x, warn = warn)
)

# 1. Write vector to disk (if needed)
# 2. Send to fast(signature = "character")
.fastVector <- function(
	x,		# SpatVector or sf
	warn	# logical
) {
    
	if (!inherits(x, "SpatVector")) x <- terra::vect(x)

	table <- terra::as.data.frame(x)
	if (nrow(table) != 0L) x <- tidyterra::transmute(x) # remove data frame

	if (terra::sources(x) == "") {
		vectFile <- tempfile(fileext = ".gpkg")
		terra::writeVector(x, filename = vectFile, filetype = "GPKG", overwrite = TRUE)
	} else {
		vectFile <- terra::sources(x)
	}
    
	fast(x = vectFile, rastOrVect = "vector", table = table, warn = warn)
	
}
