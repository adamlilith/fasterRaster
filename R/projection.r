#' Get "GRASS" projection of raster or vector
#'
#' @description "Getter" for the @projection slot.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @returns Character.
#'
#' @keywords internal
.projection <- function(x) x@projection
