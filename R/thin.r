#' Reduce linear features on a raster so linear features are 1 cell wide
#'
#' @description The `thin()` function attempts to reduce linear features on a raster to just 1 cell wide. You may need to run `thin()` multiple times on the same raster (or experiment with the `iter` argument) to get acceptable output. `thin()` can be helpful run on a raster before using `as.lines()`.
#' 
#' @param x A `GRaster`.
#' 
#' @param iter Numeric integer: Number of iterations (default is 200).
#' 
#' @returns A `GRaster`.
#' 
#' @example man/examples/ex_asLines.r
#' 
#' @aliases thin
#' @rdname thin
#' @exportMethod thin
methods::setMethod(
    f = "thin",
    signature = c(x = "GRaster"),
    function(x, iter = 200) {
    .restore(x)
    region(x)

    nLayer <- nlyr(x)
    gns <- .makeGName("thin", "raster", nLayer)
    for (i in seq_len(nLayer)) {
        args <- list(
            cmd = "r.thin",
            input = .gnames(x)[i],
            output = gns[i],
            iterations = iter,
            flags = c("quiet", "overwrite"),
            intern = TRUE
        )
        do.call(rgrass::execGRASS, args = args)
    }
    .makeGRaster(gns, names(x))
    
    } # EOF
)
