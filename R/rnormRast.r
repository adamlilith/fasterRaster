#' Create a raster with random values drawn from a normal distribution
#'
#' @description 'rnormRast()' creates a raster with values drawn from a normal distribution.
#'
#' @param x A `GRaster`. The output will have the same extent and dimensions as this raster.
#'
#' @param n A numeric integer: Number of rasters to generate. 
#'
#' @param mu,sigma Numeric: Mean and sample standard deviation of output. If creating more than one raster, you can provide one value per raster. If there are fewer, they will be recycled.
#'
#' @param seed Numeric integer or `NULL`: Random seed. If `NULL`, then a random seed is used for each raster. If provided, there should be one seed value per raster.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_randRast.r
#' 
#' @seealso [spDepRast()], [fractalRast()], [runifRast()], and **GRASS** module `r.random.surface`
#'
#' @aliases rnormRast
#' @rdname rnormRast
#' @exportMethod rnormRast
methods::setMethod(
    f = "rnormRast",
    signature = c(x = "GRaster"),
    function(x,
        n = 1,
        mu = 0,
        sigma = 1,
        seed = NULL) {

    if (!is.null(seed)) if (length(seed) != n) stop("You must provide one value of ", sQuote("seed"), " per raster, or set it to NULL.")

    mu <- rep(mu, length.out = n)
    sigma <- rep(sigma, length.out = n)

    .restore(x)
    region(x)

    srcs <- .makeSourceName("rnormScaled", "raster", n)

    for (i in seq_len(n)) {

        args <- list(
            cmd = "r.surf.gauss",
            output = srcs[i],
            mean = mu[i],
            sigma = sigma[i],
            flags = c(.quiet(), "overwrite"),
            intern = TRUE
        )
        if (!is.null(seed)) args$seed <- seed[i]
        do.call(rgrass::execGRASS, args = args)

    } # next raster
    .makeGRaster(srcs, "rnorm")

    } # EOF
)
