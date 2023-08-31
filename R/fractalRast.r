#' Create fractal raster
#'
#' @description 'fractalRast()' creates a raster with a fractal pattern.
#'
#' @param x A `GRaster`. The output will have the same extent and dimensions as this raster.
#'
#' @param n A numeric integer: Number of rasters to generate. 
#'
#' @param mu,sigma Numeric: Mean and sample standard deviation of output.
#'
#' @param dimension Numeric: Fractal dimension. Must be between 2 and 3.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_randRast.r
#' 
#' @seealso [spDepRast()], [rnormRast()], [runifRast()], and module `r.surf.fractal` in **GRASS**
#'
#' @aliases fractalRast
#' @rdname fractalRast
#' @exportMethod fractalRast
methods::setMethod(
    f = "fractalRast",
    signature = c(x = "GRaster"),
    function(x,
        n = 1,
        mu = 0,
        sigma = 1,
        dimension = 2.05) {

    if (any(dimension <= 2) | any(dimension >= 3)) stop("Argument ", sQuote("dimension"), " must be in the range (2, 3).")

    mu <- rep(mu, length.out = n)
    sigma <- rep(sigma, length.out = n)
    dimension <- rep(dimension, length.out = n)

    .restore(x)
    region(x)

    gnFracts <- .makeSourceName("fractal", "raster", n)
    gns <- .makeSourceName("fractalScaled", "raster", n)

    for (i in seq_len(n)) {

        args <- list(
            cmd = "r.surf.fractal",
            output = gnFracts[i],
            dimension = dimension[i],
            flags = c("quiet", "overwrite"),
            intern = TRUE
        )
        do.call(rgrass::execGRASS, args = args)

        y <- .makeGRaster(gnFracts[i], "random")

        xmu <- global(y, "mean")
        xsigma <- global(y, "sd")

        ex <- paste0(gns[i], " = ", mu[i], " + ", sigma[i], " * (", gnFracts[i], " - ", xmu, ") / ", xsigma)

        args <- list(
            cmd = "r.mapcalc",
            expression = ex,
            flags = c("quiet", "overwrite"),
            intern = TRUE
        )
        do.call(rgrass::execGRASS, args = args)

    } # next raster
    .makeGRaster(gns, "fractal")

    } # EOF
)
