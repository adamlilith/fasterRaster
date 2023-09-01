#' Data type of a raster
#'
#' @description In **fasterRaster**, rasters can have three data types: "factor" (categorical rasters), "integer" (integers), "float" (floating point values, accurate to ~7 decimal places), and "double" (double-precision values, accurate to ~16 decimal places). #'
#' @param x A `GRaster`.
#' 
#' @returns Logical.
#' 
#' @seealso [datatype()], [terra::datatype()]
#'
#' @example man/examples/ex_GRaster.r
#' 
#' @aliases is.integer
#' @rdname is.integer
#' @exportMethod is.integer
methods::setMethod(
    f = "is.integer",
    signature = c(x = "GRaster"),
    function(x) datatype(x, "GRASS") == "CELL" & nlevels(x) == 0L
)

#' @aliases is.float
#' @rdname is.integer
#' @exportMethod is.float
methods::setMethod(
    f = "is.float",
    signature = c(x = "GRaster"),
    function(x) datatype(x, "GRASS") == "FCELL"
)

#' @aliases is.double
#' @rdname is.integer
#' @exportMethod is.double
methods::setMethod(
    f = "is.double",
    signature = c(x = "GRaster"),
    function(x) datatype(x, "GRASS") == "DCELL"
)

#' @aliases is.factor
#' @rdname is.integer
#' @exportMethod is.factor
methods::setMethod(
    f = "is.factor",
    signature = c(x = "GRaster"),
    function(x) nlevels(x) > 0L
)
