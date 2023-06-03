#' Hillshading
#' 
#' @description Hillshade rasters are often used for display purposes becayse they make topographical relief look "real" to teh eye.
#'
#' @param x A `GRaster` (typically representing elevation).
#' @param angle Numeric: The altitude of the sun above the horizon in degrees. Valid values are in the range [0, 90], and the default value is 45 (half way from the horizon to overhead).
#' @param direction The direction (azimuth) in which the sun is shining in degrees. Valid values are in the range [0, 360]. The default is 0, meaning the sus is at due south (180 degrees) and shining due north (0 degrees). Note that in this function, 0 corresponds to north and 180 to south, but in the **GRASS** module `r.relief`, "east orientation" is used (0 is east, 90 is north, etc.).
#' @param zscale Numeric: Value by which to exaggerate terrain. The default is 1.  Numbers greater than this will increase apparant relief, and lessthan this (even negative) will diminish it.
#'
#' @returns A `GRaster`.
#'
#' @seealso [terra::shade()], module `r.relief` in **GRASS**
#' 
#' @example man/example/ex_terrain.r
#' 
#' @aliases shade
#' @rdname shade,hillshade
#' @exportMethod hillshade
methods::setMethod(
    f = 'hillshade',
    signature = c(x = 'GRaster'),
    definition = function(x, angle = 45, direction = 0, zscale = 1) {

    if (angle < 0 | angle > 90) stop('Argument ', sQuote('angle'), ' must be in the range [0, 90].')
    if (direction < 0 | direction > 360) stop('Argument ', sQuote('angle'), ' must be in the range [0, 360].')

    .restore(x)
    region(x)

    # convert from east to north orientation
    direction <- ((360 - direction) %% 360 + 90) %% 360

    for (i in 1L:nlyr(x)) {

        gn <- .makeGname('shade', 'rast')
        args <- list(
            cmd = 'r.relief',
            input = gnames(x)[i],
            output = gn,
            altitude = angle,
            azimuth = direction,
            zscale = zscale,
            flags = c('quiet', 'overwrite'),
            intern = TRUE
        )

        do.call(rgrass::execGRASS, args=args)

        this <- makeGRaster(gn, paste0(names(x)[i], '_shade'))
        if (i == 1L) {
            out <- this
        } else {
            out <- c(out, this)
        }

    }
    out

    } # EOF
)
