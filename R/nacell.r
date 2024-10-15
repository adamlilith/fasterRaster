#' Number of NA or non-NA cells in a raster
#'
#' @description The `nacell()` function counts the number of `NA` cells in a `GRaster`, and the `nonnacell()` reports the number of non-`NA` cells. If the raster is 3D, then all cells in all layers are counted.
#'
#' @param x A `GRaster`.
#'
#' @param warn Logical: If `TRUE` (default), display a warning about how much time the computation could take.
#'
#' @returns A numeric value, one per raster layer in the input.
#'
#' @example man/examples/ex_GRaster.r
#'
#' @seealso [ncell()], [ncell3d()], [terra::ncell()], [dim()], [terra::dim()]
#' 
#' @aliases nacell
#' @rdname nacell
#' @exportMethod nacell
methods::setMethod(
    f = "nacell",
    signature = c(x = "GRaster"),
    function(x, warn = TRUE) {

    .locationRestore(x)
    .region(x)
    ncell(x) - .nonnacell(x, warn = warn)

    } # EOF
)

#' @aliases nonnacell
#' @rdname nacell
#' @exportMethod nonnacell
methods::setMethod(
    f = "nonnacell",
    signature = c(x = "GRaster"),
    function(x, warn = TRUE) .nonnacell(x, warn = warn)
)

#' Number of non-NA cells in a GRaster
#'
#' @param x The [sources()] name of a `GRaster` or a `GRaster`. Location are region will be assumed to be appropriate to the raster.
#' @param warn Logical.
#' @param nLayers `NULL` or number of layers in `x`.
#'
#' @noRd
.nonnacell <- function(x, warn, nLayers = NULL) {

    if (warn) .message("nacell_nonnacell", "Calculating the number of NA or non-NA cells can take a while for large rasters.")

    # # NB `r.univar` can report incorrect numbers of cells sometimes (e.g., # NA cells > total number of cells), so we use `r.mapcalc` to create a 1s raster and sum the cell values.

    if (inherits(x, "GRaster")) {
        srcIn <- sources(x)
        nLayers <- nlyr(x)
    } else {
        srcIn <- x
        if (is.null(nLayers)) stop("Must specifify `nLayers` when supplying a sources() name to this function.")
    }

    out <- rep(NA_real_, nLayers)

    srcs <- .makeSourceName("nacell_r_mapcalc", n = nLayers)
    for (i in seq_len(nLayers)) {
    
        ex <- paste0(srcs[i], " = ", srcIn[i], " * 0 + 1")
        rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))

        args <- list(
            cmd = "r.univar",
            flags = c("r", .quiet()),
            map = srcs[i],
            nprocs = faster("cores"),
            Sys_show.output.on.console = FALSE,
            echoCmd = FALSE,
            intern = TRUE
        )

        # Should be GRASS eight point three!
        if (grassInfo("versionNumber") >= 8.3) args$nprocs <- faster("cores")
        info <- do.call(rgrass::execGRASS, args = args)

        # NB need to convert to numeric to obviate overflow
        pattern <- "sum: "
        n <- info[grepl(info, pattern = pattern)]
        n <- sub(n, pattern = pattern, replacement = "")
        out[i] <- as.numeric(n)

    } # next layer
    out

}
