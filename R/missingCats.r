#' Values in a categorical raster with no assigned category
#'
#' @description This function reports the values that appear in a categorical raster that have no matching category label.
#'
#' `GRaster`s can represent [categorical data][tutorial_raster_data_types]. Cell values are actually integers, each corresponding to a category, such as "desert" or "wetland." A categorical raster is associated with a table that matches each value to a category name.
#'
#' @param x A `GRaster`.
#'
#' @param layer Numeric integers, logical vector, or character: Layer(s) for which to obtain missing categories.
#'
#' @seealso [categorical rasters][tutorial_raster_data_types] in **fasterRaster**
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

                out <- freqs[(is.na(ac)), (val)]
                out <- freqs[is.na(ac), val]

            } # if this layer has levels
        } # next raster
        names(out) <- names(x)[layer]
        out
    } # EOF
)
