#' Number of NA or non-NA cells in a raster
#'
#' @description Number of non-`NA` or number of `NA` cells in a raster. If the raster is 3D, then all cells in all layers are counted.
#'
#' @param x A `GRaster`.
#'
#' @returns A numeric value, one per raster layer in the input.
#'
#' @example man/examples/ex_GRaster.r
#'
#' @seealso [ncell()], [ncell3d()], [terra::ncell()]
#' 
#' @aliases nacell
#' @rdname nacell
#' @exportMethod nacell
methods::setMethod(
    f = "nacell",
    signature = c(x = "GRaster"),
    function(x) {

    .restore(x)

    args <- list(
        cmd = "r.univar",
        map = paste(sources(x), collapse=","),
        flags = c("r", .quiet(), "overwrite"),
        intern = TRUE
    )

    if (grassInfo("versionNumber") >= 8.3) args$nprocs <- getFastOptions("cores")

    stats <- do.call(rgrass::execGRASS, args = args)
    pattern <- "total null cells: "
    nulls <- stats[grepl(stats, pattern = pattern)]
    nulls <- substr(nulls, nchar(pattern) + 1L, nchar(nulls))
    as.numeric(nulls)

    } # EOF
)

#' @aliases nonnacell
#' @rdname nacell
#' @exportMethod nonnacell
methods::setMethod(
    f = "nonnacell",
    signature = c(x = "GRaster"),
    function(x) ncell(x) - nacell(x)
)