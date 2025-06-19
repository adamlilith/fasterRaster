#' Reduce linear features on a raster so linear features are 1 cell wide
#'
#' @description The `thinLines()` function attempts to reduce linear features on a raster to just 1 cell wide. You may need to run `thinLines()` multiple times on the same raster (or experiment with the `iter` argument) to get acceptable output. `thinLines()` can be helpful to run on a raster before using [as.lines()].
#' 
#' @param x A `GRaster`.
#' 
#' @param iter Numeric integer: Number of iterations (default is 200).
#' 
#' @returns A `GRaster`.
#'
#' @seealso [as.lines()], **GRASS** manual page for tool `r.thin` (see `grassHelp("r.thin")`)
#' 
#' @example man/examples/ex_asLines.r
#' 
#' @aliases thinLines
#' @rdname thinLines
#' @exportMethod thinLines
methods::setMethod(
    f = "thinLines",
    signature = c(x = "GRaster"),
    function(x, iter = 200) {
    .locationRestore(x)
    .region(x)

    nLayer <- nlyr(x)
    srcs <- .makeSourceName("thin", "raster", nLayer)
    for (i in seq_len(nLayer)) {
        args <- list(
            cmd = "r.thin",
            input = sources(x)[i],
            output = srcs[i],
            iterations = iter,
            flags = c(.quiet(), "overwrite"),
            intern = TRUE
        )
        do.call(rgrass::execGRASS, args = args)
    }
    
    .makeGRaster(srcs, names(x))
    
    } # EOF
)
