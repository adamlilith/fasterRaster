#' Data type of a raster
#'
#' @description In **GRASS**, rasters can have three data types: "CELL" (integers/categories), "float" (floating point values, accurate to ~7 decimal places), and "DCELL" (double-precision values, accurate to ~16 decimal places). Often rasters that have integer values are assumed by **GRASS** to represent CELL values, but in some cases they should be DCELL or FCELL.
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
