#' Convert a raster to a lines vector
#'
#' @description [as.lines()] converts a `GRaster` to a "lines" `GVector`. Before you apply this function, you may need to run [thinLines()] on the raster to reduce linear features to a single-cell width. You may also need to use [cleanGeom()] (especially the "duplicated" and "removeDangles" `method`s) afterward to remove duplicated vertices and "dangling" lines.
#' 
#' @param x A `GRaster`. If more than one layer is in the `GRaster`, only the first will be used (with a warning).
#'
#' @returns A `GVector`.
#' 
#' @seealso [as.points()], [as.polygons()], [terra::as.lines()], [thinLines()], [cleanGeom()], and **GRASS** module `r.to.vect`
#' 
#' @example man/examples/ex_asLines.r
#' 
#' @aliases as.lines
#' @rdname as.lines
#' @exportMethod as.lines
methods::setMethod(
    f = "as.lines",
    signature = c(x = "GRaster"),
    function(x) {

    if (nlyr(x) > 1L) warning("The raster has >1 layer. Only the first will be used.")
    x <- x[[1L]]

    .restore(x)
    region(x)

    src <- .makeSourceName("asLines", "raster")
    args <- list(
        cmd = "r.to.vect",
        input = sources(x),
        output = src,
        type = "line",
        flags = c(.quiet(), "overwrite"),
        intern = TRUE
    )
    do.call(rgrass::execGRASS, args = args)
    .makeGVector(src)

    } # EOF
)
