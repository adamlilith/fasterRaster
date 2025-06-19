#' Create fractal raster
#'
#' @description `fractalRast()` creates a raster with a fractal pattern.
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
#' @seealso [rSpatialDepRast()], [rNormRast()], [rUnifRast()], [rWalkRast()], **GRASS** manual page for tool `r.surf.fractal` (see `grassHelp("r.surf.fractal")`)
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

    if (any(dimension <= 2) | any(dimension >= 3)) stop("Argument `dimension` must be in the range (2, 3).")

    mu <- rep(mu, length.out = n)
    sigma <- rep(sigma, length.out = n)
    dimension <- rep(dimension, length.out = n)

    .locationRestore(x)
    .region(x)

    srcFracts <- .makeSourceName("fractal", "raster", n) # base fractal rasters
    srcs <- .makeSourceName("fractalScaled", "raster", n) # rescaled fractal rasters

    for (i in seq_len(n)) {

        rgrass::execGRASS(
            cmd = "r.surf.fractal",
            output = srcFracts[i],
            dimension = dimension[i],
            flags = c(.quiet(), "overwrite")
        )

        xmu <- .global(srcFracts[i], fun = "mean")
        xsigma <- .global(srcFracts[i], fun = "sdpop")

        ex <- paste0(srcs[i], " = ", mu[i], " + ", sigma[i], " * (", srcFracts[i], " - ", xmu, ") / ", xsigma)

        rgrass::execGRASS(
            "r.mapcalc",
            expression = ex,
            flags = c(.quiet(), "overwrite")
        )

    } # next raster
    
    on.exit(.rm(srcFracts, type = "raster", warn = FALSE), add = TRUE)
    .makeGRaster(srcs, "fractal")

    } # EOF
)
