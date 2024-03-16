#' Values in a categorical raster with no assigned category
#'
#' @description This function reports the values in a categorical `GRaster` that have no matching category label in its "levels" table.
#'
#' `GRaster`s can represent [categorical data][tutorial_raster_data_types]. Cell values are actually integers, each corresponding to a category, such as "desert" or "wetland." A categorical raster is associated with a table that matches each value to a category name.
#'
#' @param x A `GRaster`.
#'
#' @param layer Numeric integers, logical vector, or character: Layer(s) for which to obtain missing categories.
#' 
#' @returns A numeric vector (if `x`is just one layer), or a named list of numeric vectors, one per layer in `x`.
#'
#' @seealso [missingCats()], [missing.cases()], [droplevels()], [categorical rasters][tutorial_raster_data_types] in **fasterRaster**
#'
#' @example man/examples/ex_GRaster_categorical.r
#'
#' @aliases missingCats
#' @rdname missingCats
#' @exportMethod missingCats
methods::setMethod(
    f = "missingCats",
    signature = c(x = "GRaster"),
    function(x, layer = 1:nlyr(x)) {
        
        layer <- .layerIndex(layer, x, recycle = TRUE)

        levs <- levels(x)
        isFact <- is.factor(x)

        out <- list()
        for (i in layer) {
            if (!isFact[i]) {
                out[[i]] <- numeric()
            } else {
              
                freqs <- freq(x[[i]])

                ac <- activeCat(x, names = TRUE)[i]
                val <- names(freqs)[1L]

                this <- freqs[(is.na(ac))]
                this <- this[[(val)]]
                out[[i]] <- this

            } # if this layer has levels
        } # next raster
        if (length(out) == 1L) {
            out <- out[[1L]]
        } else {
            names(out) <- names(x)[layer]
        }
        out

    } # EOF
)
