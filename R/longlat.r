#' Create longitude/latitude rasters
#' 
#' @description `longlat()` creates two rasters, one with cell values equal to the longitude of the cell centers, and one with cell values equal to the latitude of the cell centers.
#' 
#' @param x A `GRaster`.
#'
#' @param degrees Logical: If `TRUE` (default), coordinate values of cells will be in degrees. If `FALSE`, and `x` is in a projected coordinate reference system, values will represent coordinates in map units (usually meters). Values will always be in degrees when the coordinate reference system is unprojected (e.g., WGS84, NAD83, etc.).
#' 
#' @returns A `GRaster` stack.
#'
#' @seealso [init()]
#' 
#' @example man/examples/ex_longlat.r
#' 
#' @aliases longlat
#' @rdname longlat
#' @exportMethod longlat
methods::setMethod(
    f = "longlat",
    signature(x = "GRaster"),
    function(x, degrees = TRUE) {

    .locationRestore(x)
    .region(x)
    srcs <- .longlat(x, degrees = degrees)
   .makeGRaster(srcs, c("longitude", "latitude"))

    } # EOF
)

#' @noRd
.longlat <- function(x, degrees) {

    if (inherits(x, "GRaster")) {
        src <- sources(x)
    } else {
        src <- x
    }

    srcs <- .makeSourceName(c("longlat_long", "longlat_lat"), "raster")
    if (degrees) {

        rgrass::execGRASS(
            cmd = "r.latlong",
            input = src,
            output = srcs[1L],
            flags = c("l", .quiet(), "overwrite")
        )

        rgrass::execGRASS(
            cmd = "r.latlong",
            input = src,
            output = srcs[2L],
            flags = c(.quiet(), "overwrite")
        )

    } else {
    
        # longitude
        ex <- paste0(srcs[1L], " = col(", src, ")")
        rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
    
        # latitude
        ex <- paste0(srcs[2L], " = row(", src, ")")
        rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
    
    }
    srcs

}
