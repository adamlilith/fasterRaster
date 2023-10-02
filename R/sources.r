#' Name of a raster or vector in a GRASS session
#'
#' @description `sources()` retrieves the name of a raster or vector in **GRASS**. `GRaster`s and `GVector`s are actually pointers to objects stored in a **GRASS* database. When using **fasterRaster** functions on rasters and vectors, the commands are translated into **GRASS** commands and executed on the objects named in the pointers. These objects use a "source" (which is really a filename) to refer to the **GRASS** objects. In select cases, it can help to get the "sources" of a `GRaster` or `GVector`. This function is not of use to most users.
#'
#' @param x Either a `GSpatial` object or one that inherits from it (i.e., a `GRaster` or `GVector`), *or* a character. If a character, then the character itself is returned.
#'
#' @returns Character.
#'
#' @example man/examples/ex_GRaster.r
#' 
#' @aliases sources
#' @rdname sources
#' @exportMethod sources
methods::setMethod(
	f = "sources",
	signature = "GRaster",
	definition = function(x) x@sources
)

#' @aliases sources
#' @rdname sources
#' @exportMethod sources
methods::setMethod(
	f = "sources",
	signature = "GVector",
	definition = function(x) x@sources
)

#' @aliases sources
#' @rdname sources
#' @exportMethod sources
methods::setMethod(
	f = "sources",
	signature = "character",
	definition = function(x) x
)

#' Make sources
#'
#' @param x Character or `NULL`: Descriptive string. **Developers, please note**: To assist with debugging, **GRASS** objects created by a **GRASS** module have the module named in this argument (with underscores). Example: "v_in_ogr" or "r_resample".
#' @param type Character: `raster`, `raster3D`, `vector`, or `table`.
#' @param n Numeric integer: Number of names to make
#' @noRd
.makeSourceName <- function(x = NULL, type = NULL, n = 1L) {

	if (is.null(x) & is.null(type)) stop("Both ", sQuote("x"), " and ", sQuote("type"), " cannot be ", dQuote("NULL"), " at the same time.")

 	type <- tolower(type)

	names <- ""
	if (!is.null(x)) {
		if (inherits(x, "SpatRaster")) {
			
			type <- "raster"
			names <- names(x)
			names <- .fixNames(names)
			n <- terra::nlyr(x)
			
		} else if (inherits(x, "GRaster")) {

			type <- "raster"
			names <- names(x)
			names <- .fixNames(names)
			n <- nlyr(x)

		} else if (inherits(x, c("SpatVector", "sf"))) {
			type <- "vector"
		} else if (inherits(x, "character")) {
			type <- x
		}
	} else {
		type <- pmatchSafe(type, c("raster", "raster3d", "vector", "group", "region", "table"))
	}

	if (type == "raster3d") type <- "rast3d"
	if (type %in% c("GRaster", "raster")) rastOrVect <- "rast"
	if (type %in% c("GVector", "vector")) type <- "vect"
	if (type == "group") type <- "group"
	if (type == "region") type <- "region"
	if (type == "table") type <- "table"

	src <- rstring(1L)
	if (n > 1L) src <- paste0(src, "_", 1L:n)
	src <- if (names[1L] != "") {
  		paste0(type, "_", names, "_", src)
	} else {
  		paste0(type, "_", src)
	}
	src

}

.fixNames <- function(names) {

	names <- gsub(names, pattern = " ", replacement="_")
	names <- gsub(names, pattern = "\\.", replacement="_")
	names <- gsub(names, pattern = "\\+", replacement="_")
	names <- gsub(names, pattern = "-", replacement="_")
	names <- gsub(names, pattern = "\\?", replacement="_")
	names <- gsub(names, pattern = "!", replacement="_")
	names <- gsub(names, pattern = "\\*", replacement="_")
	names <- gsub(names, pattern = "\\(", replacement="_")
	names <- gsub(names, pattern = "\\)", replacement="_")
	names <- gsub(names, pattern = "\\[", replacement="_")
	names <- gsub(names, pattern = "\\]", replacement="_")
	names <- gsub(names, pattern = "\\|", replacement="_")
	names <- gsub(names, pattern = "@", replacement="_")
	names <- gsub(names, pattern = "\\$", replacement="_")
	names <- gsub(names, pattern = "#", replacement="_")
	names <- gsub(names, pattern = "%", replacement="_")
	names <- gsub(names, pattern = "\\^", replacement="_")
	names <- gsub(names, pattern = "&", replacement="_")
	names <- gsub(names, pattern = "=", replacement="_")
	names <- gsub(names, pattern = "\\'", replacement="_")
	names <- gsub(names, pattern = "\\{", replacement="_")
	names <- gsub(names, pattern = "}", replacement="_")
	names <- gsub(names, pattern = ";", replacement="_")
	names <- gsub(names, pattern = ":", replacement="_")
	names <- gsub(names, pattern = "<", replacement="_")
	names <- gsub(names, pattern = ">", replacement="_")
	names <- gsub(names, pattern = "/", replacement="_")
	names <- gsub(names, pattern = "`", replacement="_")
	names <- gsub(names, pattern = "~", replacement="_")
	names

}
