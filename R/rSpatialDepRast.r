#' Create a random raster with or without spatial dependence
#'
#' @description `rSpatialDepRast()` creates a raster with random values in cells. Across the raster, values are approximately normally distributed, though a raster with a "true" normal distribution can be made with [rnormRast()]. Spatial dependence can be introduced, though all together the values will still be approximately normally distributed.
#'
#' @param x A `GRaster`: The output will have the same extent and dimensions as this raster.
#'
#' @param n An integer: Number of rasters to generate. 
#'
#' @param mu,sigma Numeric: Mean and sample standard deviation of output. If creating more than one raster, you can provide one value per raster. If there are fewer, they will be recycled.
#'
#' @param dist Numeric: Maximum distance of spatial autocorrelation (in map units--typically meters). Default is 0 (no spatial autocorrelation). If creating more than one raster, you can provide one value per raster. If there are fewer, values will be recycled.
#'
#' @param exponent Numeric > 0: Distance decay exponent. If creating more than one raster, you can provide one value per raster. If there are fewer, values will be recycled.
#'
#' @param delay Numeric >= 0: Values >0 force the distance decay of similarity to remain constant up to this distance. Beyond this distance, the decay exponent takes effect. Default is 0. If creating more than one raster, you can provide one value per raster. If there are fewer, values will be recycled.
#' 
#' @param seed Numeric integer or `NULL`: Random seed. If `NULL`, then a random seed is used for each raster. If provided, there should be one seed value per raster.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_randRast.r
#' 
#' @seealso [rnormRast()], [fractalRast()], [runifRast()], **GRASS** manual page for tool `r.random.surface` (see `grassHelp("r.random.surface")`)
#'
#' @aliases rSpatialDepRast
#' @rdname rSpatialDepRast
#' @exportMethod rSpatialDepRast
methods::setMethod(
    f = "rSpatialDepRast",
    signature = c(x = "GRaster"),
    function(
        x,
        n = 1,
        mu = 0,
        sigma = 1,
        dist = 0,
        exponent = 1,
        delay = 0,
        seed = NULL
    ) {

    if (any(dist < 0)) stop("Argument `dist` must be >= 0.")
    if (any(exponent <= 0)) stop("Argument `exponent` must be > 0.")
    if (any(delay < 0)) stop("Argument `delay` must be >= 0.")
    if (!is.null(seed)) if (length(seed) != n) stop("You must provide one value of `seed` per raster, or set it to NULL.")

    mu <- rep(mu, length.out = n)
    sigma <- rep(sigma, length.out = n)
    dist <- rep(dist, length.out = n)
    exponent <- rep(exponent, length.out = n)
    delay <- rep(delay, length.out = n)

    .locationRestore(x)
    .region(x)

    srcRands <- .makeSourceName("rand", "raster", n)
    srcs <- .makeSourceName("randScaled", "raster", n)

    for (i in seq_len(n)) {

        args <- list(
            cmd = "r.random.surface",
            output = srcRands[i],
            distance = dist[i],
            exponent = exponent[i],
            flat = delay[i],
            flags = c(.quiet(), "overwrite")
        )
        if (!is.null(seed)) args$seed <- seed[i]
        do.call(rgrass::execGRASS, args = args)

        xmu <- .global(srcRands[i], fun = "mean")
        xsigma <- .global(srcRands[i], fun = "sdpop")

        ex <- paste0(srcs[i], " = ", mu[i], " + ", sigma[i], " * (", srcRands[i], " - ", xmu, ") / ", xsigma)

        rgrass::execGRASS(
            cmd = "r.mapcalc",
            expression = ex,
            flags = c(.quiet(), "overwrite")
        )
        
    } # next raster
    .makeGRaster(srcs, "rnorm")

    } # EOF
)
