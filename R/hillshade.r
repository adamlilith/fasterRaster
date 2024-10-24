#' Hillshading
#' 
#' @description Hillshade rasters are often used for display purposes because they make topographical relief look "real" to the eye.
#'
#' @param x A `GRaster` (typically representing elevation).
#'
#' @param angle Numeric: The altitude of the sun above the horizon in degrees. Valid values are in the range \[0, 90\], and the default value is 45 (half way from the horizon to overhead).
#'
#' @param direction The direction (azimuth) in which the sun is shining in degrees. Valid values are in the range 0 to 360. The default is 0, meaning the sun is at due south (180 degrees) and shining due north (0 degrees). Note that in this function, 0 corresponds to north and 180 to south, but in the **GRASS** module `r.relief`, "east orientation" is used (0 is east, 90 is north, etc.).
#'
#' @param zscale Numeric: Value by which to exaggerate terrain. The default is 1.  Numbers greater than this will increase apparent relief, and less than this (even negative) will diminish it.
#'
#' @returns A `GRaster`.
#'
#' @seealso [terra::shade()]
#' 
#' @example man/examples/ex_terrain.r
#' 
#' @aliases hillshade
#' @rdname hillshade
#' @exportMethod hillshade
methods::setMethod(
    f = "hillshade",
    signature = c(x = "GRaster"),
    definition = function(x, angle = 45, direction = 0, zscale = 1) {

    if (angle < 0 | angle > 90) stop("Argument `angle` must be in the range [0, 90].")
    if (direction < 0 | direction > 360) stop("Argument `angle` must be in the range [0, 360].")

    .locationRestore(x)
    .region(x)

    # convert from east to north orientation
    direction <- ((360 - direction) %% 360 + 90) %% 360

    nLayers <- nlyr(x)
    srcs <- .makeSourceName("shade", "raster", n = nLayers)
    for (i in seq_len(nLayers)) {

        args <- list(
            cmd = "r.relief",
            input = sources(x)[i],
            output = srcs[i],
            altitude = angle,
            azimuth = direction,
            zscale = zscale,
            flags = c(.quiet(), "overwrite")
        )

        do.call(rgrass::execGRASS, args = args)

    }
    .makeGRaster(srcs, paste0(names(x), "_shade"))

    } # EOF
)
