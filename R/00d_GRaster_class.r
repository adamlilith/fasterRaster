#' @title Classes for "fasterRaster" locations, rasters, and vectors
#'
#' @aliases GRaster
#' @rdname GSession
#' @exportClass GRaster
GRaster <- methods::setClass(
	"GRaster",
	contains = "GRegion",
	slots = list(
		projection = "character",
		datatypeGRASS = "character",
		nLayers = "integer",
		names = "character",
		minVal = "numeric",
		maxVal = "numeric",
		activeCat = "integer",
		levels = "list"
	),
	prototype = prototype(
		projection = NA_character_,
		datatypeGRASS = NA_character_,
		nLayers = NA_integer_,
		names = NA_character_,
		minVal = NA_real_,
		maxVal = NA_real_,
		activeCat = NA_integer_,
		levels = list(data.table::data.table(NULL))
	)
)

#' Evaluate the @levels column slot of a `GRaster`
#'
#' @description Test whether a list of "levels" tables is valid. First column must be integer. Must have >= 2 columns.
#' 
#' @param object A `GRaster`.
#' @returns `TRUE` if invalid, `FALSE` otherwise.
#'
#' @noRd
.validLevelTable <- function(object) {

	levels <- object@levels

	for (i in seq_along(levels)) {
	
		nr <- nrow(levels[[i]])
	
		if (nr != 0) {
	
			if (object@datatypeGRASS[i] != "CELL") {
				return(TRUE)
			} else if (nr == 1L) {
				return(TRUE)
			} else if (!inherits(levels[[i]][[1L]], "integer")) {
				return(TRUE)
			}
		
		}
	
	}
	FALSE

}

#' Evaluate the @activeCat slot of a GRaster
#'
#' @description Test whether `@activeCat` values are valid.
#' 
#' @param object A `GRaster`.
#' @returns `TRUE` if invalid, `FALSE` otherwise.
#'
#' @noRd
.validActiveCat <- function(object) {

	bad <- FALSE
	
	numCols <- sapply(object@levels, ncol)
	numLevels <- nlevels(object)
	if (
		any(numLevels > 0L & is.na(object@activeCat)) ||
		any(numLevels > 0L & (object@activeCat < 1L | object@activeCat > numCols))) bad <- TRUE
	
	bad

}

#' Evaluate whether a raster's datatype can accommodate levels
#'
#' @description A `GRaster` must have the `CELL` datatype to have levels.
#' 
#' @param object A `GRaster`
#' @returns `TRUE` if invalid, `FALSE` otherwise.
#'
#' @noRd
.validFactor <- function(object) {

	dt <- object@datatypeGRASS
	numLevels <- nlevels(object)
	any(numLevels > 0L & dt != "CELL")

}

#' Test if GRaster is valid
#'
#' @noRd
setValidity("GRaster",
	function(object) {
		if (!all(object@datatypeGRASS %in% c("CELL", "FCELL", "DCELL"))) {
			paste0("@datatypeGRASS can only be NA, ", sQuote("CELL"), ", ", sQuote("FCELL"), ", or ", sQuote("DCELL"), ".")
		} else if (!is.na(object@dimensions[3L]) && object@dimensions[3L] <= 0L) {
			"Third value in @dimensions must be NA or a positive integer."
		} else if (!is.na(object@resolution[3L]) && object@resolution[3L] <= 0) {
			"Third value in @resolution must be NA or a positive real value."
		} else if (object@nLayers < 1) {
			"@nLayers must be a positive integer."
		} else if (object@nLayers != length(object@sources)) {
			"@nLayers is different from the number of @sources."
		} else if (object@nLayers != length(object@names)) {
			"@names must be @nLayers in length."
		} else if (object@nLayers != length(object@minVal)) {
			"@minVal must be @nLayers in length."
		} else if (object@nLayers != length(object@maxVal)) {
			"@maxVal must be @nLayers in length."
		} else if (length(object@activeCat) != object@nLayers) {
			"The number of @activeCat values must be the same as the number of @nLayers."
		} else if (length(object@levels) != object@nLayers) {
			"The number of tables in @levels must be the same as the number of @nLayers."
		} else if (.validFactor(object)) {
			"Only GRASS datatype of a rasters with levels must be CELL."
		} else if (.validLevelTable(object)) {
			"Each table must be a NULL data.table, or if not, the first column must be an integer, and there must be >1 columns."
		} else if (.validActiveCat(object)) {
			"@activeCat must be an integer between 1 and the number of columns in each data.table in @levels, not counting the first (value) column."
		} else {
			TRUE
		}
	} # EOF
)

#' Create a GRaster
#'
#' @description Create a `GRaster` from a raster existing in the current **GRASS** session.
#'
#' @param src Character: The name of the raster in **GRASS**.
#' @param names Character: Name of the raster.
#' @param levels `NULL` (default), a `data.frame`, `data.table`, an empty string (`""`), or a list of `data.frame`s, `data.table`s, and/or empty strings: These become the raster's [levels()]. If `""`, then no levels are defined.
#'
#' @returns A `GRaster`.
#'
#' @seealso [.makeGVector()]
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @noRd
.makeGRaster <- function(src, names = "raster", levels = "") {

	# levels: convert empty strings to NULL data.tables and data.frames to data.tables
	if (is.null(levels)) levels <- ""
	if (!is.list(levels)) levels <- list(levels)
	if (length(levels) == 1L & length(src) > 1L) {
		for (i in 2L:length(src)) levels[[i]] <- levels[[1L]]
	}

	for (i in seq_along(levels)) {
		if (is.character(levels[[i]]) && levels[[i]] == "") {
			levels[[i]] <- data.table::data.table(NULL)
		} else if (!inherits(levels[[i]], "data.table")) {
   			levels[[i]] <- data.table::data.table(levels[[i]])
		}
	}
	
	info <- .rastInfo(src)
	out <- new(
		"GRaster",
		location = getFastOptions("location"),
		mapset = getFastOptions("mapset"),
		crs = crs(),
		projection = info[["projection"]][1L],
		topology = info[["topology"]][1L],
		extent = c(info[["west"]][1L], info[["east"]][1L], info[["south"]][1L], info[["north"]][1L]),
		zextent = c(info[["zbottom"]][1L], info[["ztop"]][1L]),
		nLayers = length(info$sources),
		dimensions = c(info[["rows"]][1L], info[["cols"]][1L], info[["depths"]][1L]),
		resolution = c(info[["ewres"]][1L], info[["nsres"]][1L], info[["tbres"]][1L]),
		sources = src,
		names = names,
		datatypeGRASS = info[["grassDataType"]],
		minVal = info[["minVal"]],
		maxVal = info[["maxVal"]],
		activeCat = rep(2L, length(src)),
		levels = levels
	)
	out <- .makeUniqueNames(out)
	out

}
