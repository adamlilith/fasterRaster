#' Convert a raster or lines or polygons vector to a points vector
#'
#' @description 'as.points()' converts a `GRaster` or `GVector` to a points `GVector`. For rasters, the points have the coordinates of cell centers and are assigned the cells' values. Only non-`NA` cells will be converted to a point. For vectors, each point will have the attributes of the line or polygon to which it belonged. Points are extracted from each vertex.
#' 
#' @param x A `GRaster`, `GVector`.
#' 
#' @param values Logical: If `TRUE` (default), create an attribute table with raster cell values, with one row per point.
#' 
#' @returns A `points` `GVector`.
#' 
#' @seealso [as.lines()], [as.polygons()], [terra::as.points()], and ** GRASS** modules `v.to.points` and `r.to.vect`
#' 
#' @example man/examples/ex_asPoints.r
#' 
#' @aliases as.points
#' @rdname as.points
#' @exportMethod as.points
methods::setMethod(
    f = "as.points",
    signature = c(x = "GVector"),
    function(x) {

    gtype <- geomtype(x, grass = TRUE)
    if (geomtype(x) == "point") {
        warning("Vector object is already a points object.")
        return(x)
    }

    .restore(x)

    gn <- .makeSourceName(NULL, "vector")
    args <- list(
        cmd = "v.to.points",
        input = sources(x),
        output = gn,
        use = "vertex",
        type = gtype,
        flags = c("quiet", "overwrite"),
        intern = TRUE
    )
    do.call(rgrass::execGRASS, args = args)
    .makeGVector(gn)

    } # E#OF
)

#' @aliases as.points
#' @rdname as.points
#' @exportMethod as.points
methods::setMethod(
    f = "as.points",
    signature = c(x = "GRaster"),
    function(x, values = TRUE) {

    .restore(x)
    region(x)

    if (nlyr(x) > 1L) warning("The raster has more than one layer. Only the first layer will be converted to points.")

    gn <- .makeSourceName(NULL, "vector")

    args <- list(
        cmd = "r.to.vect",
        input = sources(x)[1L],
        output = gn,
        type = "point",
        column = names(x)[1L],
        flags = c("quiet", "overwrite"),
        intern = TRUE
    )
    if (!values) args$flags <- c(args$flags, "t")
    do.call(rgrass::execGRASS, args = args)

    # drop "label" column
    info <- rgrass::execGRASS("v.info", map = gn, flags = c("c", "quiet"), intern = TRUE)
    if (length(info) == 3L & info[3L] == 'CHARACTER|label') {

        rgrass::execGRASS("v.db.dropcolumn", map = gn, columns = "label", flags = "verbose", intern = TRUE)

    }

    .makeGVector(gn)

    } # EOF
)
