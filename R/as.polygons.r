#' Convert a raster to a polygons vector
#'
#' @description [as.polygons()] converts a `GRaster` to a "polygons" `GVector`. After running this function, [geometry cleaning][breakPolys] may be useful to use to "tidy up" the vector.
#' 
#' @param x A `GRaster`. If more than one layer is in the `GRaster`, only the first will be used (with a warning).
#' 
#' @param round Logical: If `TRUE` (default), values in the raster will be rounded first before conversion to a vector. This causes cells that are adjacent that have the same (rounded) values to be combined into a single polygon. For more control, see [clump()].
#'
#' @param smooth Logical: If `TRUE`, round the corners of square features. Default is `FALSE`.
#' 
#' @returns A `GVector`.
#' 
#' @seealso [as.points()], [as.lines()], [terra::as.polygons()], [geometry cleaning][breakPolys], and **GRASS** tool `r.to.vect`
#' 
#' @example man/examples/ex_asPolygons.r
#' 
#' @aliases as.polygons
#' @rdname as.polygons
#' @exportMethod as.polygons
methods::setMethod(
    f = "as.polygons",
    signature = c(x = "GRaster"),
    function(x, round = TRUE, smooth = FALSE) {

    if (nlyr(x) > 1L) warning("The raster has >1 layer. Only the first will be used.")
    x <- x[[1L]]

    .locationRestore(x)
    .region(x)

    if (round & datatype(x, "GRASS") != "CELL") x <- round(x)

    src <- .makeSourceName("asPolys", "raster")
    args <- list(
        cmd = "r.to.vect",
        input = sources(x),
        output = src,
        type = "area",
        column = names(x),
        flags = c(.quiet(), "overwrite"),
        intern = TRUE
    )
    if (smooth) args$flags <- c(args$flags, "s")
    do.call(rgrass::execGRASS, args = args)
    .makeGVector(src)

    } # EOF
)
