#' Mask values in a raster
#'
#' @description The output of `mask()` is a `GRaster` that has the same as values as the input raster. However, if the `mask` argument is a `GRaster`, the output will have `NA` values in the sample cells that the `mask` raster has `NA` cells. If the `mask` argument is a `GVector`, then the output raster will have `NA` values in cells the `GVector` does not cover.
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
        .mask(x, mask, inverse = inverse, maskvalues = maskvalues, updatevalue = updatevalue)
    } # EOF
)

#' @aliases mask
#' @rdname mask
#' @exportMethod mask
methods::setMethod(
    f = "mask",
    signature = c(x = "GRaster", mask = "GVector"),
    function(x, mask, inverse = FALSE, updatevalue = NA) {
        .mask(x, mask, inverse = inverse, maskvalues = NA, updatevalue = updatevalue)
    } # EOF
)

.mask <- function(x, mask, inverse, maskvalues, updatevalue) {

    .restore(x)
    region(x)

    ### remove mask on exit
    .removeMask <- function() {
        
        args <- list(
            cmd = "r.mask",
            flags = c("r", "quiet", "overwrite"),
            intern = TRUE
        )

        do.call(rgrass::execGRASS, args = args)
    
    }

    on.exit(.removeMask(), add = TRUE)

    ### create a mask
    args <- list(
        cmd = "r.mask",
        flags = c("quiet", "overwrite"),
        intern = TRUE
    )

    if (inverse) args$flags <- c(args$flags, "i")

    ### mask is a raster
    ####################
    if (inherits(mask, "GRaster")) {

        if (nlyr(mask) > 1L) warning("The mask raster has >1 layer. Only the first layer will be used.")
        mask <- mask[[1L]]

        # custom mask values
        if (length(maskvalues) > 1L || !is.na(maskvalues)) {

            maskGn <- .makeGName("mask", "raster")

            if (anyNA(maskvalues)) {
                maskValuesHaveNAs <- TRUE
                maskvalues <- maskvalues[!is.na(maskvalues)]
            } else {
                maskValuesHaveNAs <- FALSE
            }

            maskvalues <- as.character(maskvalues)
            maskvalues <- paste(paste0(.gnames(mask), " == "), maskvalues)
            if (maskValuesHaveNAs) maskvalues <- c(maskvalues, paste0("isnull(", .gnames(mask), ")"))

            maskvalues <- paste(maskvalues, collapse = " | ")

            ex <- paste0(maskGn, " = if(", maskvalues, ", 1, null())")
            rgrass::execGRASS("r.mapcalc", expression = ex, flags = c("quiet", "overwrite"), intern = TRUE)

        } else {
            maskGn <- .gnames(mask)
        }

        args$raster <- maskGn
    
    ### mask is a vector
    } else if (inherits(mask, "GVector")) {
        args$vector <- .gnames(mask)
    }

    ### create mask
    do.call(rgrass::execGRASS, args = args)

    ### copy rasters with mask operative
    ### NOT CORRECT AS OF 2023/07/19: Using g.copy/.copyGSpatial() will not allow the mask to be retained on exiting the function when the mask is automatically removed.
    n <- nlyr(x)
    gns <- .copyGSpatial(x, reshapeRegion = FALSE)
    # gns <- .makeGName("mask", "raster", n)

    # for (i in seq_len(n)) {

        # ex <- paste0(gns[i], " = ", .gnames(x))
        # rgrass::execGRASS("r.mapcalc", expression = ex, flags = c("quiet", "overwrite"), intern = TRUE)

    # }

    ### change masked values
    if (!is.na(updatevalue)) {

		nLayers <- nlyr(x)
        gnsUpdate <- .makeGName("mask", "raster", nLayers)
        for (i in seq_len(nLayers)) {

            ex <- paste0(gnsUpdate[i], " = if(!isnull(", gns[i], "), ", updatevalue, ", null())")
            rgrass::execGRASS("r.mapcalc", expression = ex, flags = c("quiet", "overwrite"), intern = TRUE)

        } # next raster
        out <- .makeGRaster(gnsUpdate, names(x))

    } else {
        out <- .makeGRaster(gns, names(x))
    }
    out

} # EOF
