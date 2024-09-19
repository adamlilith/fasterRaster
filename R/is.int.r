#' Data type of a raster
#'
#' @description In **fasterRaster**, rasters can have three data types: "factor" (categorical rasters), "integer" (integers), "float" (floating point values, accurate to the 6th to 9th decimal places), and "double" (double-precision values, accurate to the 15th to 17th decimal places). The type of raster can be checked with:
#'
#' * `is.factor()`: The raster will have integer values and categories matched to the integers (see levels()).
#' * `is.int()`: Are values integers? Note that `is.int()` will return `FALSE` for categorical rasters, even though cell values are technically integers.
#' * `is.cell()`: Are values integers (`TRUE` for `integer` and categorical rasters).
#' * `is.float()`: Are values floating-point precision?
#' * `is.doub()`: Are values double-floating point precision?
#'
#' @param x A `GRaster`.
#' 
#' @returns Logical.
#' 
#' @seealso [datatype()], [terra::datatype()], [as.int()], [as.float()], [as.doub()], [is.factor()], `vignette("01_types_of_GRasters", package = "fasterRaster")`
#'
#' @example man/examples/ex_GRaster.r
#' 
#' @aliases is.int
#' @rdname is.int
#' @exportMethod is.int
methods::setMethod(
    f = "is.int",
    signature = c(x = "GRaster"),
    function(x) datatype(x, "GRASS") == "CELL" & nlevels(x) == 0L
)

is.int <- function(x) UseMethod("is.int", x)

#' @aliases is.cell
#' @rdname is.int
#' @exportMethod is.cell
methods::setMethod(
    f = "is.cell",
    signature = c(x = "GRaster"),
    function(x) datatype(x, "GRASS") == "CELL"
)

#' @aliases is.float
#' @rdname is.int
#' @exportMethod is.float
methods::setMethod(
    f = "is.float",
    signature = c(x = "GRaster"),
    function(x) datatype(x, "GRASS") == "FCELL"
)

#' @aliases is.doub
#' @rdname is.int
#' @exportMethod is.doub
methods::setMethod(
    f = "is.doub",
    signature = c(x = "GRaster"),
    function(x) datatype(x, "GRASS") == "DCELL"
)

#' @aliases is.factor
#' @rdname is.int
#' @exportMethod is.factor
methods::setMethod(
    f = "is.factor",
    signature = c(x = "GRaster"),
    function(x) nlevels(x) > 0L & datatype(x, "GRASS") == "CELL"
)
