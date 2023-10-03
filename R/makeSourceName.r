#' Make unique GRASS name for rasters, vectors, etc.
#'
#' @param x Character or `NULL`: Descriptive string. **Developers, please note**: To assist with debugging, **GRASS** objects created by a **GRASS** module have the module named in this argument (with underscores). Example: "v_in_ogr" or "r_resample".
#' 
#' @param type Character: `raster`, `raster3D`, `vector`, or `table`.
#' 
#' @param n Numeric integer: Number of names to make
#' 
#' @returns Character vector.
#' 
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
	if (type %in% c("GRaster", "raster")) type <- "rast"
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
