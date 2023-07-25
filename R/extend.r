#' Add rows and columns around a writeRaster
#'
#' @description `extend()` adds cells around a raster, making it larger.
#' 
#' @param x A `GRaster`.
#' 
#' @param y Any of:
#'
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

.extend <- function(x, y, snap, fill) {

    # resolution of x
    xCellSize <- xres(x)
    yCellSize <- yres(x)

    ### find coordinates of new extent
    if (inherits(y, "numeric")) {

        if (!(length(y) %in% c(1L, 2L, 4L) )) {
            stop("Argument ", sQuote("y"), " is invalid.")
        } else if (length(y) == 1L) {
            y <- rep(y, 4L)
        } else if (length(y) == 2L) {
            y <- rep(y, each = 2L)
        }

    } else {

        if (inherits(y, c("SpatRaster", "SpatVector", "SpatExtent"))) {
            extent <- terra::ext(y)
            extent <- as.vector(extent)
        } else if (inherits(y, "GSpatial")) {
            extent <- ext(y, vector = TRUE)
        } else {
            stop("Argument ", sQuote("y"), " is invalid.")
        }

        # by how many rows and columns do we grow?
        wgrow <- max(0, west(x) - extent[1L]) / xCellSize
        egrow <- max(0, extent[2L] - east(x)) / xCellSize
        sgrow <- max(0, south(x) - extent[3L]) / yCellSize
        ngrow <- max(0, extent[4L] - north(x)) / yCellSize

        snap <- pmatchSafe(snap, c("near", "in", "out"))
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
    w <- west(x) - y[1L] * xCellSize
    e <- east(x) + y[2L] * xCellSize
    s <- south(x) - y[3L] * yCellSize
    n <- north(x) + y[4L] * yCellSize

    # reset region
    regionExt(c(w, e, s, n), respect="resolution")

    nLayers <- nlyr(x)
    gns <- .copyGSpatial(x, reshapeRegion = FALSE)
    
    ### fill cells
    if (!is.na(fill)) {

        reg <- region()
        dims <- dim(reg)
        nr <- dims[1L]
        nc <- dims[2L]
        extent <- ext(reg, vector = TRUE)
        
        template <- matrix(NA_real_, nrow = nr, ncol = nc)

        if (y[1L] > 0L) template[ , 1L:y[1L]] <- fill
        if (y[2L] > 0L) template[ , nc:(nc - y[2L] + 1L)] <- fill
        if (y[3L] > 0L) template[nr:(nr - y[3L] + 1L), ] <- fill
        if (y[4L] > 0L) template[1:y[4L], ] <- fill

        template <- terra::rast(template, extent = extent, crs = crs(x))

        template <- fast(template)
        out <- merge(template, x)
        names(out) <- names(x)

    } else {
        out <- .makeGRaster(gns, names(x))
    }
    out

} # EOF
