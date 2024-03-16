#' Test if a coordinate reference system is unprojected
#'
#' @description `is.lonlat()` attempst to determine if a coordinate reference system is unprojected (e.g., WGS84, NAD83, NAD27, etc.). For `GRaster`s and `GVector`s, the function should always be correct. For WKT character strings and `sf` vectors, it does this by looking for the "CONVERSION[" tag in the WKT string (or the object's WKT string), and if it finds one, returns `FALSE`. This may not be truthful in all cases.
#'
#' @param x A WKT coordinate reference string or an object from which on can be obtained (e.g., a `GRaster`, `GVector`, `GRegion`, `GLocation`, `SpatRaster`, `SpatVector`, or `sf` object).
#'
#' @returns Logical (`TRUE` if unprojected, `FALSE` otherwise).
#'
#' @seealso [terra::is.lonlat()]
#'
#' @aliases is.lonlat
#' @rdname is.lonlat
#' @exportMethod is.lonlat
methods::setMethod(
    f = "is.lonlat",
    signature = c(x = "character"),
    function(x) grepl(x, pattern = "CONVERSION[")[[1L]]
)

#' @aliases is.lonlat
#' @rdname is.lonlat
#' @exportMethod is.lonlat
methods::setMethod(
    f = "is.lonlat",
    signature = c(x = "GLocation"),
    function(x) .projection(x) == "Latitude-Longitude"
)

#' @aliases is.lonlat
#' @rdname is.lonlat
#' @exportMethod is.lonlat
methods::setMethod(
    f = "is.lonlat",
    signature = c(x = "sf"),
    function(x) is.lonlat(crs(x))
)
