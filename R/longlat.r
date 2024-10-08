#' Create longitude/latitude rasters
#' 
#' @description `longlat()` creates two rasters, one with cell values equal to the longitude of the cell centers, and one with cell values equal to the latitude of the cell centers. Values will be in decimal degrees, regardless of the projection of the raster. If you want projected coordinates, use [init()].
#' 
#' @param x A `GRaster`.
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
    function(x) {

    .locationRestore(x)
    .region(x)
    srcs <- .longlat(x)
   .makeGRaster(srcs, c("longitude", "latitude"))

    } # EOF
)

#' @noRd
.longlat <- function(x) {

    if (inherits(x, "GRaster")) {
        src <- sources(x)
    } else {
        src <- x
    }

    srcs <- .makeSourceName(c("longlat_long", "longlat_lat"), "raster")
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
    srcs

}
