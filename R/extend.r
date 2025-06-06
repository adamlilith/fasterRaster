#' Add rows and columns around a writeRaster
#'
#' @description `extend()` adds cells around a raster, making it larger.
#' 
#' @param x A `GRaster`.
#' 
#' @param y Any of:
#' *  An object from which an extent can be obtained; i.e., a `SpatRaster`, `SpatVector`, `SpatExtent`, `sf` vector, or a `GSpatial` object (any of `GRaster`, `GVector`, or `GRegion`). If the extent of `x` is "outside" the extent of `y` on any side, the side(s) of `x` that are outside will be kept as-is (i.e., the extent of `x` will never be shrunk).
#' * A single positive integer: Number of rows and columns to add to the top, bottom, and sides of the raster.
#' * Two integers >= 0: Number of columns (1st value) to add to the sides, and number of rows (2nd value) to add to the top and bottom of the raster.
#' * Four integers >= 0: Number of rows and columns to add (left column, right column, bottom row, top row).
#' 
#' @param snap Character: Method used to align `y` to `x`. Partial matching is used. This is only used if `y` is not a set of numbers.
#' * `"near"` (default): Round to nearest row/column
#' * `"in"`: Round "inward" toward the extent of `x` to nearest row/column
#' * `"out"`: Round "outward" away from the extent of `x` to the nearest row/column. 
#' 
#' @param fill Numeric: Value to place in the new cells. The default is `NA`.
#' 
#' @returns A `GRaster`.
#' 
#' @details Known issues: When `GRaster`s are saved to disk explicitly using [writeRaster()], or implicitly using [rast()] or [plot()], rows and columns that are entirely `NA` are dropped.
#' 
#' @seealso [terra::extend()]
#'
#' @example man/examples/ex_extend.r
#' 
#' @aliases extend
#' @rdname extend
#' @exportMethod extend
methods::setMethod(
    f = "extend",
    signature = c(x = "GRaster", y = "numeric"),
    function(x, y, fill = NA) .extend(x = x, y = y, snap = NA, fill = fill)
)

#' @aliases extend
#' @rdname extend
#' @exportMethod extend
methods::setMethod(
    f = "extend",
    signature = c(x = "GRaster", y = "SpatRaster"),
    function(x, y, snap = "near", fill = NA) .extend(x = x, y = y, snap = snap, fill = fill)
)

#' @aliases extend
#' @rdname extend
#' @exportMethod extend
methods::setMethod(
    f = "extend",
    signature = c(x = "GRaster", y = "SpatVector"),
    function(x, y, snap = "near", fill = NA) .extend(x = x, y = y, snap = snap, fill = fill)
)

#' @aliases extend
#' @rdname extend
#' @exportMethod extend
methods::setMethod(
    f = "extend",
    signature = c(x = "GRaster", y = "SpatExtent"),
    function(x, y, snap = "near", fill = NA) .extend(x = x, y = y, snap = snap, fill = fill)
)

#' @aliases extend
#' @rdname extend
#' @exportMethod extend
methods::setMethod(
    f = "extend",
    signature = c(x = "GRaster", y = "sf"),
    function(x, y, snap = "near", fill = NA) .extend(x = x, y = y, snap = snap, fill = fill)
)

#' @aliases extend
#' @rdname extend
#' @exportMethod extend
methods::setMethod(
    f = "extend",
    signature = c(x = "GRaster", y = "GSpatial"),
    function(x, y, snap = "near", fill = NA) .extend(x = x, y = y, snap = snap, fill = fill)
)

#' @noRd
.extend <- function(x, y, snap, fill) {

    # resolution of x
    ewres <- xres(x)
    nsres <- yres(x)

    ### find coordinates of new extent
    if (inherits(y, "numeric")) {

        if (!(length(y) %in% c(1L, 2L, 4L))) {
            stop("Argument `y` is invalid.")
        } else if (length(y) == 1L) {
            y <- rep(y, 4L)
        } else if (length(y) == 2L) {
            y <- rep(y, each = 2L)
        }
        
        if (any(!omnibus::is.wholeNumber(y))) stop("Values of `y` must be numeric integers.")
        if (any(y < 0L)) y[y < 0L] <- 0L

    } else {

        if (inherits(y, c("SpatRaster", "SpatVector", "SpatExtent"))) {
            extent <- terra::ext(y)
            extent <- as.vector(extent)
        } else if (inherits(y, "GSpatial")) {
            extent <- ext(y, vector = TRUE)
        } else {
            stop("Argument `y` is invalid.")
        }

        # by how many rows and columns do we grow?
        wgrow <- max(0, W(x) - extent[1L]) / ewres
        egrow <- max(0, extent[2L] - E(x)) / ewres
        sgrow <- max(0, S(x) - extent[3L]) / nsres
        ngrow <- max(0, extent[4L] - N(x)) / nsres

        snap <- omnibus::pmatchSafe(snap, c("near", "in", "out"))
        fx <- if (snap == "near") {
            round
        } else if (snap == "in") {
            floor
        } else if (snap == "out") {
            ceiling
        }

        grows <- c(wgrow, egrow, sgrow, ngrow)
        y <- do.call(fx, list(grows))

    } # if y is an object with an extent

    # new extent coordinates
    w <- W(x) - y[1L] * ewres
    e <- E(x) + y[2L] * ewres
    s <- S(x) - y[3L] * nsres
    n <- N(x) + y[4L] * nsres

    w <- as.character(w)
    e <- as.character(e)
    s <- as.character(s)
    n <- as.character(n)

    ewres <- as.character(ewres)
    nsres <- as.character(nsres)

    # reset region
    rgrass::execGRASS(
        cmd = "g.region",
        w = w, e = e, s = s, n = n,
        ewres = ewres, nsres = nsres,
        flags = c(.quiet(), "overwrite")
    )

    reg <- .region()
    nc <- ncol(reg)
    nr <- nrow(reg)

    ### extent
    nLayers <- nlyr(x)
    srcs <- rep(NA_character_, nLayers)
    for (i in seq_len(nLayers)) {

        if (is.na(fill)) {
            src <- .copyGSpatial(x = x[[i]], reshapeRegion = FALSE)
        } else {
            src <- .makeSourceName("extend", "raster", nLayers)

            ex <- paste0(src, " = if(col() <= ", y[1L], ", ", fill, ", if(col() > ncols() - ", y[2L], ", ", fill, ", if(row() <= ", y[4L], ", ", fill, ", if(row() > nrows() - ", y[3], ", ", fill, ", ", sources(x)[i], "))))")

            args <- list(
                cmd = "r.mapcalc",
                expression = ex,
                region = "current",
                flags = c(.quiet(), "overwrite")
            )
            do.call(rgrass::execGRASS, args = args)
        }

        srcs[i] <- src
        
    } # next raster
    
    .makeGRaster(srcs, names = names(x), levels = levels(x), ac = activeCats(x))

} # EOF
