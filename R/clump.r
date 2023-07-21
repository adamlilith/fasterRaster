#" Group cells with similar values into clumps
#"
#" @description `clump()` identifies groups of adjacent cells that have the same value or same approximate value, and assigns them a unique number, creating "clumps" of same- or similar-valued cells.
#" 
#" @param x A `GRaster`.
#" 
#" @param minDiff Numeric in the range [0, 1): Minimum difference between cells in order for them to be assigned to the same clump. This is a proportion of the range across all cells. For example, if `minDiff` is set to 0.01, then the maximum difference between cells in a clump can be up to 1% of the entire range across all cells. Small values can create large clumps. The default is 0, in which case values have to be exactly the same.
#" 
#" @param minClumpSize Numeric integer >= 1. Minimum number of cells in a clump. The default is 1.
#" 
#" @param diagonal Logical: If `TRUE` (default), then cells "connected" at corners will be included as part of the same clump.
#" 
#" @returns A `GRaster`.
#" 
#" @example man/examples/ex_clump.r
#" 
#" @aliases clump
#" @rdname clump
#" @exportMethod clump
methods::setMethod(
    f = "clump",
    signature = c(x = "GRaster"),
    function(x, minDiff = 0, minClumpSize = 1, diagonal = TRUE) {

    if (minDiff < 0 | minDiff >= 1) stop(sQuote("minDiff"), " must be >= 0 and < 1.")

    .restore(x)
    region(x)

    args <- list(
        cmd = "r.clump",
        input = NA_character_,
        output = NA_character_,
        threshold = minDiff,
        minsize = minClumpSize,
        flags = c("quiet", "overwrite"),
        intern = TRUE
    )

    if (diagonal) args$flags <- c(args$flags, "d")

    nLayers <- nlyr(x)
    gns <- .makeGName("clump", "rast", nLayers)
    for (i in seq_len(nLayers)) {

        args$input <- .gnames(x)[i]
        args$output <- gns[i]

        do.call(rgrass::execGRASS, args=args)
        this <- .makeGRaster(gns[i])

        if (i == 1L) {
            out <- this
        } else {
            out <- c(out, this)
        }

    } # next raster
    out

    } # EOF
)