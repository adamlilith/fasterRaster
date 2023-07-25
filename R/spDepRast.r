#' Create a random raster with or without spatial dependence
#'
#' @description 'spDepRast()' creates a raster with random values in cells. Across the raster, values are approximately normally distributed, though a raster with a "true" normal distribution can be made with [rnformRast()]. Spatial dependence can be introduced, though all together the values will still be approximately normally distributed.
#'
#' @param x A `GRaster`. The output will have the same extent and dimensions as this raster.
#'
#' @param n A numeric integer: Number of rasters to generate. 
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
#' @seealso [rnormRast()], [fractalRast()], and module `r.random.surface` in **GRASS**
#'
#' @aliases spDepRast
#' @rdname spDepRast
#' @exportMethod spDepRast
methods::setMethod(
    f = "spDepRast",
    signature = c(x = "GRaster"),
    function(x,
        n = 1,
        mu = 0,
        sigma = 1,
        dist = 0,
        exponent = 1,
        delay = 0,
        seed = NULL) {

    if (any(dist < 0)) stop("Argument ", sQuote("dist"), " must be >= 0.")
    if (any(exponent <= 0)) stop("Argument ", sQuote("exponent"), " must be > 0.")
    if (any(delay < 0)) stop("Argument ", sQuote("delay"), " must be >= 0.")
    if (!is.null(seed)) if (length(seed) != n) stop("You must provide one value of ", sQuote("seed"), " per raster, or set it to NULL.")

    mu <- rep(mu, length.out = n)
    sigma <- rep(sigma, length.out = n)
    dist <- rep(dist, length.out = n)
    exponent <- rep(exponent, length.out = n)
    delay <- rep(delay, length.out = n)

    .restore(x)
    region(x)

    gnRands <- .makeGName("rand", "raster", n)
    gns <- .makeGName("randScaled", "raster", n)

    for (i in seq_len(n)) {

        args <- list(
            cmd = "r.random.surface",
            output = gnRands[i],
            distance = dist[i],
            exponent = exponent[i],
            flat = delay[i],
            flags = c("quiet", "overwrite"),
            intern = TRUE
        )
        if (!is.null(seed)) args$seed <- seed[i]
        do.call(rgrass::execGRASS, args = args)

        y <- .makeGRaster(gnRands[i], "random")

        xmu <- global(y, "mean")
        xsigma <- global(y, "sd")

        ex <- paste0(gns[i], " = ", mu[i], " + ", sigma[i], " * (", gnRands[i], " - ", xmu, ") / ", xsigma)

        args <- list(
            cmd = "r.mapcalc",
            expression = ex,
            flags = c("quiet", "overwrite"),
            intern = TRUE
        )
        do.call(rgrass::execGRASS, args = args)

        this <- .makeGRaster(gns[i], "rnorm")
        if (i == 1L) {
            out <- this
        } else {
            out <- c(out, this)
        }
    } # next raster
    out

    } # EOF
)
