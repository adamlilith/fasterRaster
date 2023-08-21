#' Number of levels in a categorical raster
#'
#' @description This function reports the number of categories (levels) in a [categorical raster][tutorial_categorical_rasters].
#'
#' @param x A `GRaster` or `SpatRaster`.
#'
#' @param droplevels Logical: If `TRUE` (default), drop levels that do not appear in the raster.
#'
#' @return A numeric vector of integers. These represent the number of levels that appear in the raster (always for `GRaster`s, and for `SpatRaster`s when `dropLevels = TRUE`), or the number of levels that appear or could appear in the raster (for `SpatRaster`s only, when `dropLevels = FALSE`).
#'
#' @seealso [levels()], [terra::cats()], [terra::levels()], [terra::addCats()], [terra::droplevels()], [categorical rasters][tutorial_categorical_rasters]
#'
#' @example man/examples/ex_categorical_rasters.r
#'
#' @aliases ncat
#' @rdname ncat
#' @exportMethod ncat
setMethod(
    f = "ncat",
    signature = "GRaster",
    definition = function(x) {
        if (x@nCats == 0L) {
            out <- 0L
        } else {
            levs <- levels(x)
            isChar <- sapply(levs, inherits, "character")
            out <- rep(NA_integer_, nlyr(x))
            if (any(isChar)) out[isChar] <- 0L
            if (any(!isChar)) out[!isChar] <- sapply(levs[!isChar], nrow)
        }
		names(out) <- names(x)
        out
    } # EOF
)

#' @aliases ncat
#' @rdname ncat
#' @exportMethod ncat
setMethod(
    f = "ncat",
    signature = "SpatRaster",
    definition = function(x, dropLevels = TRUE) {
        if (dropLevels) x <- terra::droplevels(x)
        out <- sapply(terra::levels(x), nrow)
        names(out) <- names(x)
        out
    } # EOF
)
