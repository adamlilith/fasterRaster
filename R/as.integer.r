#' Coerce raster to integer, float, or double precision
#'
#' @description Typically, changing the [datatype()] of a `GRaster` is not necessary in **fasterRaster**, but on occasions it can be useful.
#' 
#' In **GRASS** rasters can have three data types: "CELL" (integers/categories), "float" (floating point values, accurate to ~7 decimal places), and "DCELL" (double-precision values, accurate to ~16 decimal places). Often rasters that have integer values are assumed by **GRASS** to represent CELL values, but in some cases they should be DCELL or FCELL. These functions help convert between these types.
#'
#' @param x A `GRaster`.
#' 
#' @returns A `GRaster`.
#' 
#' @seealso [terra::datatype()]
#'
#' @example man/examples/ex_GRaster.r
#' 
#' @aliases as.integer
#' @rdname as.integer
#' @exportMethod as.integer
methods::setMethod(
    f = "as.integer",
    signature = c(x = "GRaster"),
    function(x) .as.type(x, fx = "int")
)

#' @aliases as.float
#' @rdname as.integer
#' @exportMethod as.float
methods::setMethod(
    f = "as.float",
    signature = c(x = "GRaster"),
    function(x) .as.type(x, fx = "float")
)

#' @aliases as.double
#' @rdname as.double
#' @exportMethod as.double
methods::setMethod(
    f = "as.double",
    signature = c(x = "GRaster"),
    function(x) .as.type(x, fx = "double")
)


# x GRaster
# type "int", "float", "double"
.as.type <- function(x, fx) {

    .restore(x)
    region(x)

    for (i in 1L:nlyr(x)) {

        gn <- .makeSourceName(names(x)[i], "rast")
        ex <- paste0(gn, " = ", fx, "(", sources(x)[i], ")")
        args <- list(
            cmd = "r.mapcalc",
            expression = ex,
            flags = c("quiet", "overwrite"),
            intern = TRUE
        )

        do.call(rgrass::execGRASS, args=args)

        this <- .makeGRaster(gn, names(x)[i])
        if (i == 1L) {
            out <- this
        } else {
            out <- c(out, this)
        }

    }
    out

} # EOF
