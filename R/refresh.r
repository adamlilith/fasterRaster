#' Refresh metadata in a GRaster or GVector
#'
#' `GRaster`s and `GVector`s are really pointers to objects in **GRASS**. The values displayed when you use `show()` or `print()` for a `GRaster` or `GVector` are stored in **R**. If, on the odd chance that you make a change to a `GRaster` or `GVector` (e.g., using commands in the **rgrass** package), the changes will not be automatically reflected in the `GRaster` or `GVector`. This function can be used to refresh the objects in **R** to reflect their proper values.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @returns A `GRaster` or `GVector`.
#'
#' @example man/examples/ex_GRaster.r 
#'
#' @aliases .refresh
#' @noRd
methods::setMethod(
    f = ".refresh",
    signature = c(x = "GRaster"),
    definition = function(x) .makeGRaster(sources(x))
)

#' @aliases .refresh
#' @noRd
methods::setMethod(
    f = ".refresh",
    signature = c(x = "GVector"),
    definition = function(x) .makeGVector(sources(x))
)
