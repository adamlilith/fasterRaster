#' Refresh metadata in a GRaster or GVector
#'
#' `GRaster`s and `GVector`s are really pointers to objects in **GRASS**. The values displayed when you use `show()` or `print()` for a `GRaster` or `GVector` are stored in **R**. If, on the odd chance that you make a change to a `GRaster` or `GVector` (e.g., using commands in the **rgrass** package), the changes will not be automatically reflected in the `GRaster` or `GVector`. This function can be used to update the objects in **R** to reflect their proper values.
#'
#' @param object A `GRaster` or `GVector`.
#'
#' @returns A `GRaster` or `GVector`.
#'
#' @example man/examples/ex_GRaster.r 
#'
#' @aliases update
#' @rdname update
#' @exportMethod update
methods::setMethod(
    f = "update",
    signature = c(object = "GRaster"),
    definition = function(object) .makeGRaster(sources(object), levels = cats(object), ac = activeCats(object))
)

#' @aliases update
#' @rdname update
#' @exportMethod update
methods::setMethod(
    f = "update",
    signature = c(object = "GVector"),
    definition = function(object) .makeGVector(sources(object), table = as.data.table(object))
)
