#' Number of NA or non-NA cells in a raster
#'
#' @description Number of `NA` cells or number of non-`NA` cells in a raster. If the raster is 3D, then all cells in all layers are counted.
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
    .message("nacell_nonnacell", "This function can take a while to calculate for large rasters.")

    # # NB `r.univar` can report incorrect numbers of cells sometimes (e.g., # NA cells > total number of cells)
    # args <- list(
    #     cmd = "r.univar",
    #     map = paste(sources(x), collapse=","),
    #     flags = c("r", .quiet(), "overwrite"),
    #     intern = TRUE
    # )

    # if (grassInfo("versionNumber") >= 8.3) args$nprocs <- faster("cores")

    # stats <- do.call(rgrass::execGRASS, args = args)
    # pattern <- "total null cells: "
    # nulls <- stats[grepl(stats, pattern = pattern)]
    # nulls <- substr(nulls, nchar(pattern) + 1L, nchar(nulls))
    # as.numeric(nulls)

    nLayers <- nlyr(x)
    out <- rep(NA_real_, nLayers)

    srcs <- .makeSourceName("nacell_r_mapcalc", n = nLayers)
    for (i in seq_len(nLayers)) {
    
        ex <- paste0(srcs[i], " = ", sources(x)[i], " * 0 + 1")
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

        if (grassInfo("versionNumber") >= 8.3) args$nprocs <- faster("cores")
        info <- do.call(rgrass::execGRASS, args = args)

        # NB need to convert to numeric to obviate overflow
        pattern <- "sum: "
        n <- info[grepl(info, pattern = pattern)]
        n <- sub(n, pattern = pattern, replacement = "")
        out[i] <- as.numeric(nrow(x[[i]])) * as.numeric(ncol(x[[i]])) - as.numeric(n)

    } # next layer
    out

    } # EOF
)

#' @aliases nonnacell
#' @rdname nacell
#' @exportMethod nonnacell
methods::setMethod(
    f = "nonnacell",
    signature = c(x = "GRaster"),
    function(x, warn = TRUE) ncell(x) - nacell(x)
)
