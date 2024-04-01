#' Mask values in a raster
#'
#' @description The output of `mask()` is a `GRaster` that has the same as values as the input raster. However, if the `mask` argument is a `GRaster`, the output will have `NA` values in the same cells that the `mask` raster has `NA` cells. If the `mask` argument is a `GVector`, then the output raster will have `NA` values in cells the `GVector` does not cover.
#'
#' @param x A `GRaster`.
#' 
#' @param mask A `GRaster` or `GVector`.
#' 
#' @param inverse Logical: If `TRUE`, the effect of the mask is inverted. That is, a copy of the input raster is made, but cells that overlap with an `NA` in the mask raster or are not covered by the mask vector retain their values. Cells that overlap with an `NA` in the mask raster or overlap with the mask vector are forced to `NA`.
#' 
#' @param maskvalues Numeric vector, including `NA` (only for when `mask` is a `GRaster`): The value(s) in the mask raster cells that serve as the mask. The default is `NA`, in which case cells in the input raster that overlap with `NA` cells in the mask are forced to `NA`.
#' 
#' @param updatevalue Numeric, including `NA` (default): The values assigned to masked cells.
#' 
#' @returns A `GRaster`.
#' 
#' @seealso [terra::mask()], **GRASS** module `r.mask` 
#'
#' @example man/examples/ex_mask.r
#' 
#' @aliases mask
#' @rdname mask
#' @exportMethod mask
methods::setMethod(
    f = "mask",
    signature = c(x = "GRaster", mask = "GRaster"),
    function(x, mask, inverse = FALSE, maskvalues = NA, updatevalue = NA) {
    
    .locationRestore(x)
    .region(x)
    xname <- names(x)

    if (nlyr(mask) > 1L) warning("The mask raster has >1 layer. Only the first layer will be used.")

    x <- sources(x)
    mask <- sources(mask)[1L]

    srcs <- .mask(x = x, mask = mask, maskType = "raster", inverse = inverse, maskvalues = maskvalues, updatevalue = updatevalue)

    .makeGRaster(srcs, xname, levels = cats(x), ac = activeCats(x))

    } # EOF
)

#' @aliases mask
#' @rdname mask
#' @exportMethod mask
methods::setMethod(
    f = "mask",
    signature = c(x = "GRaster", mask = "GVector"),
    function(x, mask, inverse = FALSE, updatevalue = NA) {

    .locationRestore(x)
    .region(x)
    xname <- names(x)

    x <- sources(x)
    mask <- sources(mask)

    srcs <- .mask(x = x, mask = mask, maskType = "vector", inverse = inverse, maskvalues = NA, updatevalue = updatevalue)

    .makeGRaster(srcs, xname, levels = cats(x), ac = activeCats(x))
    
    } # EOF
)

#' @param x [sources()] name of a `GRaster`.
#' @param mask [sources()] name of the mask `GRaster` or `GVector`.
#' @param maskType "raster" or "vector".
#' @param inverse Logical.
#' @param maskvalues Numeric or `NA`.
#' @param updatevalue Numeric or `NA`
#' @noRd
.mask <- function(x, mask, maskType, inverse = FALSE, maskvalues = NA, updatevalue = NA) {

    # remove mask on exit
    on.exit(.removeMask(), add = TRUE)

    ### mask is a raster
    ####################
    if (maskType == "raster") {

        # custom mask values
        if (length(maskvalues) > 1L || !is.na(maskvalues)) {

            maskSrc <- .makeSourceName("r_mapcalc", "raster")

            if (anyNA(maskvalues)) {
                maskValuesHaveNAs <- TRUE
                maskvalues <- maskvalues[!is.na(maskvalues)]
            } else {
                maskValuesHaveNAs <- FALSE
            }

            maskvalues <- as.character(maskvalues)
            maskvalues <- paste(paste0(sources(mask), "=="), maskvalues)
            if (maskValuesHaveNAs) maskvalues <- c(maskvalues, paste0("isnull(", sources(mask), ")"))

            maskvalues <- paste(maskvalues, collapse = " | ")

            ex <- paste0(maskSrc, " = if(", maskvalues, ",1,null())")
            
            args <- list(
                cmd = "r.mapcalc",
                expression = ex,
                flags = c(.quiet(), "overwrite")
            )

            do.call(rgrass::execGRASS, args = args)

        } else {
            maskSrc <- mask
        }

        ### create a mask
        args <- list(
            cmd = "r.mask",
            flags = c(.quiet(), "overwrite")
        )

        args$raster <- maskSrc
    
    ### mask is a vector
    } else if (maskType == "vector") {
        args <- list(
            cmd = "r.mask",
            flags = c(.quiet(), "overwrite")
        )
        args$vector <- mask
    }

    if (inverse) args$flags <- c(args$flags, "i")

    ### create mask
    do.call(rgrass::execGRASS, args = args)

    ### copy rasters with mask operative
    ### NOT CORRECT AS OF 2023/07/19: Using g.copy/.copyGSpatial() will not allow the mask to be retained on exiting the function when the mask is automatically removed.
    n <- length(x)
    srcs <- .copyGRaster(x, topo = topology(x), reshapeRegion = FALSE)
    # srcs <- .makeSourceName("mask", "raster", n)

    # for (i in seq_len(n)) {

        # ex <- paste0(srcs[i], " = ", sources(x))
        # rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)

    # }

    ### change masked values
    if (!is.na(updatevalue)) {

		nLayers <- length(x)
        srcsUpdate <- .makeSourceName("r_mapcalc", "raster", nLayers)

        for (i in seq_len(nLayers)) {

            ex <- paste0(srcsUpdate[i], " = if(!isnull(", srcs[i], "), ", updatevalue, ", null())")
            
            rgrass::execGRASS(
                "r.mapcalc",
                expression = ex,
                flags = c(.quiet(), "overwrite")
            )

        } # next raster
        srcs <- srcsUpdate

    } else {
        out <- srcs
    }
    srcs

} # EOF

### remove mask on exit from within a function
#' @noRd
.removeMask <- function() {

    rgrass::execGRASS(
        cmd = "r.mask",
        flags = c(.quiet(), "overwrite", "r")
    )
    
}
