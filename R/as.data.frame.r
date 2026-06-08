#' Convert GRaster or GVector to a data frame
#'
#' @description Convert a `GRaster` to a table, or a `GVector`'s data table to a `data.frame` or `data.table`.
#'
#' @param x A `GRaster` or `GVector`.
#' @param na.rm Logical: Indicates whether to remove `NA` values (default is `TRUE`; `GRaster`s only)`.
#' @param labels Logical: If `TRUE`, a "factor" (categorical) `GRaster` will have its factor level labels returned. If `FALSE`, the integer codes are returned. Default is `TRUE` (for `GRaster`s only, and only has an effect if the `GRaster` is of type "factor").
#' @param xy Logical: If `TRUE`, return coordinates of cell centers (default is `FALSE`, for `GRaster`s only).
#' @param cells Logical: If `TRUE`, return cell columns and rows (default is `FALSE`, for `GRaster`s only).
#'
#' @returns A `data.frame` or `NULL` (if the `GVector` has no data table, or if `GRaster`'s values are all `NA`).
#' 
#' @seealso [terra::as.data.frame()], [data.table::as.data.table()]
#' 
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @aliases as.data.table
#' @rdname as.data.frame
#' @exportMethod as.data.table
methods::setMethod(
    f = "as.data.table",
    signature = c(x = "GRaster"),
    definition = function(x, na.rm = TRUE, labels = TRUE, xy = FALSE, cells = FALSE) {

    .locationRestore(x)

    # flags: -1 ==> one line per cell, n = ignore NULLs, -N = do not return cells where all rasters have NAs
    outputFile <- paste0(.workDir(), '/cell_values_', omnibus::rstring(1L), '.csv')

    args <- list(
        cmd = "r.stats",
        input = sources(x),
        output = outputFile,
        separator = "comma",
        null_value = "NA",
        flags = c(.quiet(), "overwrite", "1")
    )

    if (na.rm) args$flags <- c(args$flags, "n", "N")
    if (xy) args$flags <- c(args$flags, "g")
    if (cells) args$flags <- c(args$flags, "x")
    do.call(rgrass::execGRASS, args = args)

    columnNames <- names(x)
    if (cells) columnNames <- c("column", "row", columnNames)
    if (xy) columnNames <- c("longitude", "latitude", columnNames)
    out <- data.table::fread(outputFile, col.names = columnNames)

    if (labels & any(datatype(x) %in% "factor")) {

        offset <- 0L
        if (xy) offset <- offset + 2L
        if (cells) offset <- offset + 2L

        for (i in 1:nlyr(x)) {
            if (datatype(x)[i] == "factor") {

                levs <- levels(x[[i]])[[1L]]
                vals <- levs[[1L]]
                labs <- levs[[activeCats(x)[i] + 1L]]
                out[[i + offset]] <- factor(out[[i + offset]], levels = vals, labels = labs)

            }
        }
    }

    out

    } # EOF
)

#' @aliases as.data.frame
#' @rdname as.data.frame
#' @exportMethod as.data.frame
methods::setMethod(
	f = "as.data.frame",
	signature = c(x = "GRaster"),
	definition = function(x, na.rm = TRUE, labels = TRUE, xy = FALSE, cells = FALSE) {

    out <- as.data.table(x, na.rm = na.rm, labels = labels, xy = xy, cells = cells)
    as.data.frame(out)

	} # EOF
)

#' @aliases as.data.frame
#' @rdname as.data.frame
#' @exportMethod as.data.frame
methods::setMethod(
	f = "as.data.frame",
	signature = c(x = "GVector"),
	definition = function(x) {
		if (nrow(x) > 0L) {
			as.data.frame(x@table)
		} else {
			NULL
		}

	} # EOF
)

#' @aliases as.data.table
#' @rdname as.data.frame
#' @exportMethod as.data.table
methods::setMethod(
    f = "as.data.table",
    signature = c(x = "GVector"),
    definition = function(x) {
        if (nrow(x) > 0L) {
            x@table
        } else {
            NULL
        }
    } # EOF
)


