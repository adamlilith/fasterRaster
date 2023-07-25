#' Calculate area of non-NA cells
#'
#' @description `expanse()` calculates the area of all non-`NA` cells in meters-squared.
#'
#' @param x A `GRaster`.
#' 
#' @param unit Character: Units of the output. Valid values include `"m"` (meters-squared), `"km"` (kilometers-squared), or `"ha"` (hectares). Partial matching is used.
#'
#' @returns A `data.frame` with two columns, one for each layer's name and one for the summed area of all non-`NA` cells in the layer.
#'
#' @example man/examples/ex_expanse.r
#'
#' @aliases expanse
#' @rdname expanse
#' @exportMethod expanse
methods::setMethod(
    f = "expanse",
    signature = c(x = "GRaster"),
    function(x, unit = "m") {
    
        .restore(x)
        region(x)

        nLayers <- nlyr(x)
        gns <- .makeGName("area", "raster", nLayers)
        out <- data.frame()

        for (i in seq_len(nLayers)) {

            # create raster with values of each cell's area            
            ex <- paste0(gns[i], " = area(", .gnames(x)[i], ")")
            args <- list(
                cmd = "r.mapcalc",
                expression = ex,
                args = c("quiet", "overwrite"),
                intern = TRUE
            )
            do.call(rgrass::execGRASS, args = args)

            # sum area
            args <- list(
                cmd = "r.univar",
                flags = c("quiet", "r"),
                map = .gnames(x)[i],
                Sys_show.output.on.console = FALSE,
                echoCmd = FALSE,
                intern = TRUE
            )
            if (getFastOptions("grassVer") >= "8.3") args$nprocs <- getFastOptions("cores")

            info <- do.call(rgrass::execGRASS, args)
            
            y <- info[grepl(info, pattern = "sum:")]
            y <- sub(y, pattern = pattern, replacement = "")
            y <- as.numeric(y)

            out <- rbind(
                out,
                data.frame(
                    layer = names(x)[i],
                    area = y
                )
            )

        } # next raster

        unit <- pmatchSafe(unit, c("m", "km", "ha"))
        if (unit == "km") {
            out$area <- out$area / 1000^2
        } else if (unit == 'ha') {
            out$area <- out$area * 0.0001
        }

        out

    } # EOF
)
