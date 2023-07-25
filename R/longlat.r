#' Create longitude/latitude rasters
#' 
#' @description `longlat()` creates two rasters, one with cell values equal to the longitude of the cell centers, and one with cell values equal to the latitude of the cell centers. Values will be in decimal degrees, regardless of the projection of the raster.
#' 
#' @param x A `GRaster`.
#' 
#' @returns A `GRaster` stack.
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

    .restore(x)
    region(x)
    
    gns <- .makeGName(c("long", "lat"), "raster")
    args <- list(
        cmd = "r.latlong",
        input = .gnames(x)[1L],
        output = gns[1L],
        flags = c("l", "quiet", "overwrite"),
        intern = TRUE
    )

    do.call(rgrass::execGRASS, args=args)

    gnLat <- .makeGName("lat", "raster")
    args <- list(
        cmd = "r.latlong",
        input = .gnames(x)[1L],
        output = gns[2L],
        flags = c("quiet", "overwrite"),
        intern = TRUE
    )
    do.call(rgrass::execGRASS, args=args)

    .makeGRaster(gns, c("longitude", "latitude"))

    } # EOF
)