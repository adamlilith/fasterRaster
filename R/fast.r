#' Create a GRaster or GVector
#'
#' @description `fast()` creates a `GRaster` or `GVector` from a file, or from a `SpatRaster`, `SpatVector`, or `sf` vector. This function will also create a connection to **GRASS** the given coordinate reference system of the raster or vector if none has been made yet.
#'
#' **GRASS** supports loading from disk a variety of raster formats (see the **GRASS** manual page for [`r.in.gdal`](https://grass.osgeo.org/grass84/manuals/r.in.gdal.html)) and vector formats (see the **GRASS** manual page for [`v.in.ogr`](https://grass.osgeo.org/grass84/manuals/v.in.ogr.html)), though not all of them will work with this function.
#'
#' @param x Any one of:
#' * A `SpatRaster` raster. Rasters can have one or more layers.
#' * A `SpatVector` or `sf` spatial vector.
#' * A character string with the path and filename of a raster or vector to be loaded directly into **GRASS**. The function will attempt to ascertain the type of object from the file extension (raster or vector), but it can help to indicate which it is using the `rastOrVect` argument if it is unclear.
#'
#' @param rastOrVect Either `NULL` (default), or `"raster"` or `"vector"`: If `x` is a filename, then the function will try to ascertain whether it represents a raster or a vector, but sometimes this will fail. In that case, it can help to specify if the file holds a raster or vector. Partial matching is used.
#'
#' @param snap For polygon `GVector`s only: Either `NULL` (default) or a positive numeric value: Sometimes, polygons created in other software are topologically incorrect--the borders of adjacent polygons may cross one another, or there may be small gaps between them. These errors can be corrected by slightly moving vertices. The value of `snap` indicates how close vertices need to be for them to be shifted to to the same location for a correct topology. Small values are recommended, and units are in map units (usually meters). By default, this is `NULL`, meaning no snapping is done. Vectors that have been snapped may need to be cleaned using [cleanGeom()] with the `break`, `duplicated`, and `smallAngles` tools.
#'
#' @param ... Other arguments. These are typically used internally so not of use to most users. They can include:
#' * `levels` (`GRaster`s): A `data.frame`, `data.table`, or list of `data.frame`s or `data.table`s with categories for categorical rasters: The first column of a table corresponds to raster values and must be of type `integer`. A subsequent column corresponds to category labels. By default, the second column is assumed to represent labels, but this can be changed with \code{\link[fasterRaster]{activeCat<-}}. Level tables can also be `NULL` (e.g., `data.fame(NULL)`).
#'
#' * `table` (`GVector`s): A `data.frame` or `data.table` with one row per geometry in a `GVector`: Serves as an attribute table.
#'
#' @seealso [rgrass::read_RAST()] and [rgrass::read_VECT()], plus **GRASS** modules [`v.in.ogr`](https://grass.osgeo.org/grass84/manuals/r.in.gdal.html), [`v.in.ogr`](https://grass.osgeo.org/grass84/manuals/v.in.ogr.html), and [`r.import`](https://grass.osgeo.org/grass84/manuals/r.import.html).
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
		snap = NULL,
		...
	) {

	dots <- list(...)

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
		
		location <- .locationFind(xRast, match = "crs")
		
		if (is.null(location) | !grassStarted()) {

			.locationCreate(x = xRast)
			location <- .location()

		}

		.locationRestore(x = location)

		# src <- .makeSourceName("r_in_gdal", type = "raster", n = 1L)
		src <- .makeSourceName("r_external", type = "raster", n = 1L)
		rgrass::execGRASS(
			# cmd = "r.in.gdal",
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

		src <- .makeSourceName("v_in_ogr", type = "vector")
		rgrass::execGRASS(
			cmd = "v.in.ogr",
			input = x,
			output = src,
			flags = c(.quiet(), "overwrite")
		)

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
	function(x, snap = NULL) .fastVector(x, snap = snap)
)

#' @rdname fast
#' @aliases fast
#' @exportMethod fast
methods::setMethod(
	"fast",
	signature(x = "sf"),
	function(x, snap = NULL) .fastVector(x, snap = snap)
)

# 1. Write vector to disk (if needed)
# 2. Send to fast(signature = "character")
#' @noRd
.fastVector <- function(
	x,		# SpatVector or sf
	snap	# NULL or numeric
) {
    
	if (!inherits(x, "SpatVector")) x <- terra::vect(x)
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
    
  fast(x = vectFile, snap = snap, rastOrVect = "vector", table = table)
	
}
