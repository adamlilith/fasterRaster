#' Coordinate reference system of a GRaster or GVector
#' 
#' @description Get the coordinate reference system (CRS) of a `GRaster` or `GVector`s, or from the currently active **GRASS** "project/location" (see `vignette("projects_mapsets", package = "fasterRaster")`):
#' * `crs()`: Return a WKT string (an object of class `character`).
#' * `st_crs()`: Return a `crs` object (from the **sf** package).
#' * `coordRef()`: Return a `list`.
#'
#' @param x An object that inherits from a `GLocation` (i.e., a `GRaster` or `GVector`) or missing. If missing, the coordinate reference system of the currently active **GRASS** "project/location" is reported.
#' 
#' @param ... Other arguments (generally unused).
#'
#' @returns Function `crs()` returns a `character` object, `st_crs()` returns `crs` object, and `coordRef()` a `list`.
#' 
#' @seealso [terra::crs()], [sf::st_crs()]
#' 
#' @example man/examples/ex_GRaster.r
#'
#' @aliases crs
#' @rdname crs
#' @exportMethod crs
methods::setMethod(
    f = "crs",
    signature = c(x = "missing"),
    definition = function(x) {

	locs <- .locations()
	al <- .fasterRaster$activeLocation
	locs[[al]]

    } # EOF
)

#' @aliases crs
#' @rdname crs
#' @exportMethod crs
methods::setMethod(
	f = "crs",
	signature = "GLocation",
	definition = function(x) x@crs
)

#' @rdname crs
#' @aliases st_crs
#' @exportMethod st_crs
methods::setMethod(
	f = "st_crs",
	signature = "missing",
	definition = function(x, ...) {
		
	out <- crs()
	sf::st_crs(out)

	} # EOF
)

#' @rdname crs
#' @aliases st_crs
#' @exportMethod st_crs
methods::setMethod(
	f = "st_crs",
	signature = "GLocation",
	definition = function(x, ...) {

	out <- x@crs
	out <- sf::st_crs(out)
	out

	} # EOF
)

#' @aliases st_crs
#' @rdname crs
#' @export
st_crs <- function(x, ...) UseMethod("st_crs", x)

#' @rdname crs
#' @aliases coordRef
#' @exportMethod coordRef
methods::setMethod(
	f = "coordRef",
	signature = "missing",
	function(x) {
	
	info <- rgrass::execGRASS("g.proj", flags = "p", intern = TRUE)
	out <- list()

	if (any(grepl(info, pattern = "name       :"))) {

		y <- info[grepl(info, pattern = "name       :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		out <- c(out, list(name = y))

	}

	if (any(grepl(info, pattern = "ellps      :"))) {

		y <- info[grepl(info, pattern = "ellps      :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		out <- c(out, list(ellipse = y))

	}

	if (any(grepl(info, pattern = "datum"))) {

		y <- info[grepl(info, pattern = "datum")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		out <- c(out, list(datum = y))

	}

	if (any(grepl(info, pattern = "EPSG:"))) {

		y <- info[grepl(info, pattern = "EPSG:")]
		y <- strsplit(y, split = "EPSG:")[[1L]][2L]
		y <- trimws(y)

		out <- c(out, list(epsg = y))

	}

	if (any(grepl(info, pattern = "proj       :"))) {

		y <- info[grepl(info, pattern = "proj       :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		out <- c(out, list(proj = y))

	}

	if (any(grepl(info, pattern = "lat_0      :"))) {

		y <- info[grepl(info, pattern = "lat_0      :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		y <- as.numeric(y)

		out <- c(out, list(lat_0 = y))

	}

	if (any(grepl(info, pattern = "lon_0      :"))) {

		y <- info[grepl(info, pattern = "lon_0      :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		y <- as.numeric(y)

		out <- c(out, list(lon_0 = y))

	}

	if (any(grepl(info, pattern = "azi        :"))) {

		y <- info[grepl(info, pattern = "azi        :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		y <- as.numeric(y)

		out <- c(out, list(azimuth = y))

	}

	if (any(grepl(info, pattern = "x_0        :"))) {

		y <- info[grepl(info, pattern = "x_0        :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		y <- as.numeric(y)

		out <- c(out, list(x_0 = y))

	}

	if (any(grepl(info, pattern = "y_0        :"))) {

		y <- info[grepl(info, pattern = "y_0        :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		y <- as.numeric(y)

		out <- c(out, list(y_0 = y))

	}

	if (any(grepl(info, pattern = "k          :"))) {

		y <- info[grepl(info, pattern = "k          :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		y <- as.numeric(y)

		out <- c(out, list(k = y))

	}

	if (any(grepl(info, pattern = "meters     :"))) {

		y <- info[grepl(info, pattern = "meters     :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		y <- as.numeric(y)

		out <- c(out, list(meters = y))

	}

	if (any(grepl(info, pattern = "pm         :"))) {

		y <- info[grepl(info, pattern = "pm         :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		out <- c(out, list(prime_meridian = y))

	}

	if (any(grepl(info, pattern = "unit       :"))) {

		y <- info[grepl(info, pattern = "unit       :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		out <- c(out, list(unit = y))

	}

	if (any(grepl(info, pattern = "units      :"))) {

		y <- info[grepl(info, pattern = "units      :")]
		y <- strsplit(y, split = ":")[[1L]][2L]
		y <- trimws(y)

		out <- c(out, list(proj_unit = y))

	}
	out

	} # EOF
)

#' @rdname crs
#' @aliases coordRef
#' @exportMethod coordRef
methods::setMethod(
	f = "coordRef",
	signature = c(x = "GRaster"),
	function(x) {
	
	.locationRestore(x)
	coordRef()

	} # EOF
)

#' @rdname crs
#' @aliases coordRef
#' @exportMethod coordRef
methods::setMethod(
	f = "coordRef",
	signature = c(x = "GVector"),
	function(x) {
	
	.locationRestore(x)
	coordRef()

	} # EOF
)

