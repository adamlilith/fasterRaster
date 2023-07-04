#' Refresh metadata in a GRaster or GVector
#'
#' `GRaster`s and `GVector`s are really pointers to objects in **GRASS**. The values displayed when you use `show()` or `print()` for a `GRaster` or `GVector` are stored in **R**. If, on the odd chance that you make a change to a `GRaster` or `GVector` in **GRASS** (e.g., using the **GRASS** GUI using [grass()], the changes will not be automatically reflected in the `GRaster` or `GVector`. This function can be used to refresh the objects in **R** to reflect their proper values.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @returns A `GRaster` or `GVector`.
#'
#' @examples man/examples/ex_GRaster.r 
#'
#' @aliases refresh
#' @rdname refresh
#' @exportMethod refresh
methods::setMethod(
    f = 'refresh',
    signature = c(x = 'GRaster'),
    definition = function(x) makeGRaster(gnames(x))
)

#' @aliases refresh
#' @rdname refresh
#' @exportMethod refresh
methods::setMethod(
    f = 'refresh',
    signature = c(x = 'GVector'),
    definition = function(x) makeGVector(gnames(x))
)
