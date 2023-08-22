#' Data type of a raster
#'
#' @description In **GRASS**, rasters can have three data types: "CELL" (integers/categories), "float" (floating point values, accurate to ~7 decimal places), and "DCELL" (double-precision values, accurate to ~16 decimal places). Often rasters that have integer values are assumed by **GRASS** to represent CELL values, but in some cases they should be DCELL or FCELL.
#'
#' The `is.cell()`, `is.fcell()`, and `is.dcell()` functions return `TRUE` or `FALSE`, depending on the data type of the raster.
#'
#' The `is.factor()` returns `TRUE` if raster values represent categories. Note that unlike `SpatRaster`s, `GRaster`s can be integer or non-integer and still represent categorical data.
#'
#' @param x A `GRaster`.
#' 
#' @returns Logical.
#' 
#' @seealso [datatype()], [terra::datatype()]
#'
#' @example man/examples/ex_GRaster.r
#' 
#' @aliases is.cell
#' @rdname is.cell
#' @exportMethod is.cell
methods::setMethod(
    f = "is.cell",
    signature = c(x = "GRaster"),
    function(x) datatype(x) == "CELL"
)

#' @aliases is.fcell
#' @rdname is.cell
#' @exportMethod is.fcell
methods::setMethod(
    f = "is.fcell",
    signature = c(x = "GRaster"),
    function(x) datatype(x) == "FCELL"
)

#' @aliases is.dcell
#' @rdname is.cell
#' @exportMethod is.dcell
methods::setMethod(
    f = "is.dcell",
    signature = c(x = "GRaster"),
    function(x) datatype(x) == "DCELL"
)

#' @aliases is.factor
#' @rdname is.cell
#' @exportMethod is.factor
methods::setMethod(
    f = "is.factor",
    signature = c(x = "GRaster"),
    function(x) ncat(x) > 0L
)
