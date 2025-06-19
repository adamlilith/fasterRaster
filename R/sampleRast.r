#' Randomly sample cells from a GRaster
#'
#' @description `sampleRast()` randomly samples cells from non-`NA` cells of a raster. The output will be a raster with selected non-`NA` cells, and all other cells set to `NA`. To generate random points, see [spatSample()].
#'
#' @param x A `GRaster`.
#'
#' @param size Numeric: Number of cells or proportion of cells to select.
#' 
#' @param prop Logical: If `TRUE`, the value of `size` will be interpreted as a proportion of cells. The default is `FALSE` (`size` is interpreted as the number of cells to select).
#'
#' @param maskvalues Numeric vector, including `NA`, or `NULL` (default): Values in the raster to select from. All others will be ignored. If this is `NULL`, then only non-`NA` cells will be selected for retention.
#'
#' @param updatevalue Numeric or `NULL` (default): Value to assign to masked cells. If `NULL`, then the values in the input raster are retained.
#' 
#' @param test Logical: If `TRUE`, and `size` is greater than the number of non-`NA` cells in `x`, then fail. Testing this can take a long time for large rasters. The default is `FALSE`.
#'
#' @param seed `NULL` (default) or numeric: If `NULL`, then a random seed will be generated for the random number generator. Otherwise a seed can be provided.
#' 
#' @returns A `GRaster`.
#' 
#' @seealso [spatSample()]; [terra::spatSample()], tool `r.random` in **GRASS**
#' 
#' @example man/examples/ex_sampleRast_spatSample.r
#' 
#' @aliases sampleRast
#' @rdname sampleRast
#' @exportMethod sampleRast
methods::setMethod(
    f = "sampleRast",
    signature = c(x = "GRaster"),
    function(
        x,
        size,
        prop = FALSE,
        maskvalues = NA,
        updatevalue = NULL,
        test = FALSE,
        seed = NULL
    ) {

    if (size <= 0) stop("Cannot select this number of cells.")
    if (prop && size > 1) stop("Cannot select more than 100% of cells.")
    if (!prop && size > ncell(x)) stop("Cannot select more than the total number of cells.")
    if (test) {
        if (any(size > nacell(x))) stop("At least one raster has too few non-NA cells.")
    }

    .locationRestore(x)
    .region(x)

    if (!prop) {
        # npoints <- as.character(size)
        npoints <- formatC(size, format = "f", drop0trailing = TRUE) # overcome scientific notation

    } else {
        npoints <- paste0(100 * npoints, "%")
    }

    nLayers <- nlyr(x)
    srcs <- .makeSourceName("sampleRast_r_random", "raster", nLayers)
    for (i in seq_len(nLayers)) {
    
        args <- list(
            cmd = "r.random",
            input = sources(x)[i],
            cover = sources(x)[i],
            npoints = npoints,
            raster = srcs[i],
            flags = c(.quiet(), "overwrite")
        )

        if (is.null(seed)) {
            args$flags <- c(args$flags, "s")
        } else {
            args$seed <- seed
        }
        # if (selectNA) args$flags <- c(args$flags, "size")

        # custom mask values
        if (length(maskvalues) > 1L || !is.na(maskvalues)) {

            maskSrc <- .makeSourceName("mask", "raster")

            if (anyNA(maskvalues)) {
                maskValuesHaveNAs <- TRUE
                maskvalues <- maskvalues[!is.na(maskvalues)]
            } else {
                maskValuesHaveNAs <- FALSE
            }

            maskvalues <- as.character(maskvalues)
            maskvalues <- paste(paste0(sources(x)[i], " == "), maskvalues)
            if (maskValuesHaveNAs) maskvalues <- c(maskvalues, paste0("isnull(", sources(mask), ")"))

            maskvalues <- paste(maskvalues, collapse = " | ")

            ex <- paste0(maskSrc, " = if(", maskvalues, ", 1, null())")
            rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)

        } else {
            maskSrc <- sources(x)[i]
        }
        args$input <- maskSrc

        do.call(rgrass::execGRASS, args = args)

    } # next raster

    ### change masked values
    if (!is.null(updatevalue)) {

        srcsUpdate <- .makeSourceName("random", "raster", nLayers)
        for (i in seq_len(nLayers)) {

            ex <- paste0(srcsUpdate[i], " = if(!isnull(", srcs[i], "), ", updatevalue, ", null())")
            rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"), intern = TRUE)

        } # next raster
        out <- .makeGRaster(srcsUpdate, names(x))

    } else {
        levels <- cats(x)
        ac <- activeCats(x)
        out <- .makeGRaster(srcs, names(x), levels = levels, ac = ac)
    }
    out

    } # EOF
)
