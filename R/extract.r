#' Extract values from a GRaster at locations in a points GVector
#'
#' @description `extract()` obtains the values of a `GRaster` or `GVector` associated with the locations of a set of points. The output depends on the input:
#' * **Case #1: `x` is a numeric or integer `GRaster` and `y` is a points `GVector`**: Returns values of cells that have points. If `xy` is `TRUE`, also returns the coordinates of the points.
#' * **Case #2: `x` is a [categorical][tutorial_raster_data_types] (factor) `GRaster` and `y` is a points `GVector`**: Same as case #1, but if `cats` is `TRUE`, returns category labels of cells that have points. If `xy` is `TRUE`, also returns the coordinates of the points.
#' * **Case #3: `x` is a categorical `GRaster` and `y` is a lines or polygons `GVector`**: Returns a summary (e.g., mean, standard deviation, etc.) of all cells that overlap the line(s) or polygon(s).
#' * **Case #4: `x` is a `GVector` and `y` is a points `GVector`**: Returns the data table row associated each point. If `xy` is `TRUE`, also returns the coordinates of the points.
#' Note that whenever a points `GVector` is allowed for `y`, a `data.frame`, `data.table`, `matrix`, or `numeric` values representing points can be used instead.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param y A `GVector`, *or* a `data.frame` or `matrix` where the first two columns represent longitude and latitude (in that order), *or* a two-element numeric vector where the first column represents longitude and the second latitude. Values of `x` will be extracted from the points in `y`. `GVector`s can be of types points, lines, or polygons. 
#'
#' @param fun Character vector: Name(s) of function(s) to apply to values. This is used when `x` is a `GRaster` and `y` is a lines or polygons `GVector`. The method(s) specified by `fun` will be applied to all cell values that overlap with each geometry (i.e., individual cell values will not be returned). Valid functions include:
#' * `"countNonNA"`: Number of overlapping cells.
#' * `"countNA"`: Number of overlapping `NA` cells.
#' * `"mean"`: Average.
#' * `"min"`: Minimum.
#' * `"max"`: Minimum.
#' * `"sum"`: Sum.
#' * `"range"`: Maximum - minimum.
#' * `"sd"`: Sample standard deviation (same as [stats::sd()]).
#' * `"sdpop"`: Population standard deviation.
#' * `"var"`: Sample variance (same as [stats::var()]).
#' * `"varpop"`: Population variance.
#' * `"cv"`: Coefficient of variation.
#' * `"cvpop"`: Population coefficient of variation.
#' * `"median"`: Median.
#' * `"quantile"`: Quantile; you can specify the quantile using the `prob` argument.
#'
#' @param prob Numeric in the range from 0 to 1: Quantile which to calculate. The value of `prob` will be rounded to the nearest hundredth.
#'
#' @param overlap Logical: If `TRUE` (default), and `y` is a lines or polygons `GVector`, then account for potential overlap of geometries when extracting. This can be slow, so if you are sure geometries do not overlap, you can change this to `FALSE`. This argument is ignored if `y` is a points `GVector`.
#'
#' @param xy Logical: If `TRUE` and `y` represents points, also return the coordinates of each point. Default is `FALSE.`
#'
#' @param cats Logical (extracting from a raster): If `TRUE` and `x` is a [categorical raster][tutorial_raster_data_types], then return the category labels instead of the values. Default is `FALSE.
#'
#' @returns A `data.frame` or `data.table`.
#'
#' @example man/examples/ex_extract.r
#'
#' @seealso [terra::extract()], and modules `r.what` and `v.what` in **GRASS**
#'
#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GRaster", y = "GVector"),
    function(
        x,
        y,
        fun = "mean",
        prob = 0.5,
        overlap = TRUE,
        xy = FALSE,
        cats = FALSE
    ) {

    .locationRestore(x)
    compareGeom(x, y)

    nLayers <- nlyr(x)
    gtype <- geomtype(y)

    ### if GVector is lines/polygons
    if (gtype %in% c("lines", "polygons")) {

        ### assuming may be overlap in geometries
        if (overlap) {
        
            numGeoms <- ngeom(y)

            for (i in seq_len(numGeoms)) {
            
                yy <- y[i]
                yy <- .aggregate(yy, gtype = "polygons", dissolve = FALSE)
                yy <- .makeGVector(yy)
                
                thisOut <- extract(
                    x = x, y = yy,
                    fun = fun,
                    prob = prob,
                    overlap = FALSE
                )

                if (i == 1L) {
                    out <- thisOut
                } else {
                    out <- rbind(out, thisOut)
                }
            
            } # next geometry
        
        ### assuming no overlap in geometries
        } else {

            funs <- c(
                "countNonNA",
                "countNA",
                "min",
                "max",
                "range",
                "mean",
                "sd",
                "sdpop",
                "var",
                "varpop",
                "cv",
                "cvpop",
                "sum",
                "median",
                "quantile"
            )

            fun <- omnibus::pmatchSafe(fun, funs)

            # get sample versions
            sampleSd <- any(fun == "sd")
            sampleVar <- any(fun == "var")
            sampleCv <- any(fun == "cv")

            if (any(fun == "quantile")) {
                
                if (length(prob) > 1L) stop("Argument ", sQuote("prob"), " can only have one value.")
                if (prob > 1 | prob < 0) stop("Argument ", sQuote("prob"), " must be in the range [0, 1].")
                prob <- round(100 * prob)
            
            }

            method <- fun
            method <- method[!(method %in% c("sd", "var", "cv"))]
            method[method == "countNonNA"] <- "number"
            method[method == "countNA"] <- "null_cells"
            method[method == "min"] <- "minimum"
            method[method == "max"] <- "maximum"
            method[method == "mean"] <- "average"
            method[method == "sdpop"] <- "stddev"
            method[method == "varpop"] <- "variance"
            method[method == "cvpop"] <- "coeff_var"
            method[method == "quantile"] <- "percentile"
        
            method <- unique(method)

            for (i in seq_len(nLayers)) {

                prefix <- omnibus::rstring(1L)

                if (length(method) > 0L) {
                    
                    args <- list(
                        cmd = "v.rast.stats",
                        raster = sources(x)[i],
                        map = sources(y),
                        column_prefix = prefix,
                        method = method,
                        flags = c(.quiet())
                    )
                    if (gtype == "lines") args$flags <- c(args$flags, "d")
                    if (any(method == "percentile")) args$percentile <- prob
                    do.call(rgrass::execGRASS, args = args)

                    extracted <- .vAsDataTable(sources(y))

                } else {
                    extracted <- data.table::data.table(NULL)
                }

                # sample SD, var, CV
                if (sampleSd | sampleVar | sampleCv) {
                    
                    sampleStats <- .sampleStatsFromRast(i = i, x = x, y = y, fun = fun, extracted = extracted, prefix = prefix)
                    extracted <- cbind(extracted, sampleStats)
                    
                }

                if (any(names(extracted) == "cat")) extracted$cat <- NULL
                nms <- names(extracted)
                nms <- gsub(nms, pattern = paste0(prefix, "_"), replacement = "")

                nms[nms == "number"] <- "countNonNA"
                nms[nms == "null_cells"] <- "countNA"
                nms[nms == "minimum"] <- "min"
                nms[nms == "maximum"] <- "max"
                nms[nms == "average"] <- "mean"
                nms[nms == "stddev"] <- "sdpop"
                nms[nms == "variance"] <- "varpop"
                nms[nms == "coeff_var"] <- "cvpop"
                nms[grepl(nms, pattern = "percentile")] <- "quantile"

                names(extracted) <- nms

                if (any(fun == "cvpop")) {
                    cvpop <- NULL
                    extracted[ , cvpop := cvpop / 100]
                }

                extracted <- extracted[ , ..fun]
                names(extracted) <- paste0(names(x)[i], "_", names(extracted))

                if (i == 1L) {
                    out <- extracted
                } else {
                    out <- cbind(out, extracted)
                }

            } # next raster layer

        } # no overlap/overlap in geometries

    ### if GVector is points
    } else {

        for (i in seq_len(nLayers)) {

            args <- list(
                cmd = "r.what",
                map = sources(x)[i],
                points = sources(y),
                null_value = "NA",
                flags = c(.quiet(), "overwrite"),
                cache = faster("memory"),
                intern = TRUE
            )

            info <- do.call(rgrass::execGRASS, args = args)

            pillars <- gregexpr(info, pattern = "\\|\\|")
            pillars <- unlist(pillars)
            ncs <- nchar(info)
            info <- substr(info, pillars + 2L, ncs)

            if (is.cell(x)[i]) {
                info <- as.integer(info)
            } else {
                info <- as.numeric(info)
            }

            this <- data.table::data.table(TEMPTEMP__ = info)
            names(this) <- names(x)[i]

            # category label instead of value
            if (cats && is.factor(x)[i]) {

                levs <- levels(x[[i]])[[1L]]
                this <- levs[match(info, levs[[1L]]), 2L]
                names(this) <- paste0(names(x)[i], "_cat")

            }

            if (i == 1L) {
                out <- this
            } else {
                out <- cbind(out, this)
            }

        } # next raster layer

        if (xy) {

            coords <- crds(y, z = is.3d(y))
            coords <- data.table::as.data.table(coords)
            out <- cbind(coords, out)

        }

    } # if lines/polygons or points
    
    if (!faster("useDataTable")) out <- as.data.frame(out)
    out

    } # EOF
)

#' Function to calculate sample sd, variance, and CV from raster
#'
#' @noRd
.sampleStatsFromRast <- function(i, x, y, fun, extracted, prefix) {

    ### calculate variance
    x2 <- x[[i]]^2

    # n * SUM(x^2)
    if (any(fun == "countNonNA")) {
        
        count <- extracted[[paste0(prefix, "_number")]]

        numer1 <- extract(
            x = x2,
            y = y,
            fun = "sum"
        )

        numer1 <- numer1[[1L]]
        numer1 <- numer1 * count

    
    } else {

        numer1 <- extract(
            x = x2,
            y = y,
            fun = c("countNonNA", "sum")
        )

        count <- numer1[[paste0(names(x2), "_countNonNA")]]
        count <- as.numeric(count) # avoid integer overflow
        sums <- numer1[[paste0(names(x2), "_sum")]]
        sums <- as.numeric(sums)
        numer1 <- count * sums

    }

    # (SUM(x))^2
    if (!any(fun == "sum")) {

        numer2 <- extract(
            x = x[[i]],
            y = y,
            fun = "sum"
        )
        numer2 <- numer2[[paste0(names(x), "_sum")]]

    } else {
        numer2 <- extracted[[paste0(prefix, "_sum")]]
    }

    numer2 <- as.numeric(numer2)
    numer2 <- numer2^2

    denom <- count * (count - 1)
    variance <- (numer1 - numer2) / denom

    ### calculate mean for CV
    if (any(fun == "cv")) {
    
        if (any(fun == "mean")) {
            means <- extracted[[paste0(prefix, "_average")]]
        } else {
        
            means <- extract(
                x = x[[i]],
                y = y,
                fun = "mean"
            )

            means <- means[[paste0(names(x), "_mean")]]
        
        }
    
    }

    thisExtracted <- data.table::data.table(NULL)

    if (any(fun == "var")) {
        var <- NULL
        thisExtracted[ , var := variance]
    }

    if (any(fun == "sd")) {
        sd <- NULL
        thisExtracted[ , sd := sqrt(variance)]
    }

    if (any(fun == "cv")) {
        cv <- NULL
        thisExtracted[ , cv := sqrt(variance) / means]
    }

    thisExtracted

}

#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GRaster", y = "data.frame"),
    function(
        x,
        y,
        xy = FALSE,
        cats = FALSE
    ) {

    if (ncol(y) < 2L) stop("Argument ", sQuote("y"), " must have at least two columns. The first must represent longitude and the second latitude.")
    if (ncol(y) > 2L) warning("Argument ", sQuote("y"), " has more than two columns. The first will be assumed to represent longitude and the second latitude.")

    y <- terra::vect(y, geom = colnames(y)[1L:2L], crs = crs(x), keepgeom = FALSE)
    y <- fast(y)
    extract(x = x, y = y, xy = xy, cats = cats)

    } # EOF
)

#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GRaster", y = "data.table"),
    function(
        x,
        y,
        xy = FALSE,
        cats = FALSE
    ) {

    y <- as.data.frame(y)
    extract(x = x, y = y, xy = xy, cats = cats)

    } # EOF
)

#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GRaster", y = "matrix"),
    function(
        x,
        y,
        xy = FALSE,
        cats = FALSE
    ) {

    y <- as.data.frame(y)
    extract(x = x, y = y, xy = xy, cats = cats)

    } # EOF
)

#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GRaster", y = "numeric"),
    function(
        x,
        y,
        xy = FALSE,
        cats = FALSE
    ) {

    if (length(y) != 2L) stop("Argument ", sQuote("y"), " must have two values, longitude and latitude.")
    y <- cbind(y)
    colnames(y) <- c("x", "y")
    extract(x = x, y = y, xy = xy, cats = cats)

    } # EOF
)

#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GVector", y = "GVector"),
    function(x, y, xy = FALSE) {

    if (geomtype(y) != "points") stop("Argument", sQuote("y"), " must be a points vector.")
    if (is.3d(y)) warning("Coordinates in the z-dimension will be ignored.")

    .locationRestore(x)
    coords <- crds(y, z = FALSE)
    .extractFromVect(x, coords, xy)

    } # EOF

)

#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GVector", y = "data.frame"),
    function(x, y, xy = FALSE) .extractFromVect(x, y[ , 1L:2L], xy)
)

#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GVector", y = "data.table"),
    function(x, y, xy = FALSE) {

    y <- as.data.frame(y)
    .extractFromVect(x, y[ , 1L:2L], xy)

    } # EOF
)

#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GVector", y = "matrix"),
    function(x, y, xy = FALSE) .extractFromVect(x, y[ , 1L:2L], xy)
)

#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GVector", y = "numeric"),
    function(x, y, xy = FALSE) {
        
    y <- cbind(y)
    .extractFromVect(x, y, xy)

    } # EOF
)

# #' For internal use by other functions
# #' 
# #' @param y Character: [sources()] of points vector
# #' 
# #' @noRd
# methods::setMethod(
#     f = "extract",
#     signature = c(x = "GVector", y = "character"),
#     function(x, y, xy = FALSE) {

#     y <- .crdsVect(y, z = FALSE, gm = "points")
#     .extractFromVect(x, y, xy)

#     } # EOF
# )

#' @param x GVector
#' @param coords 2-column matrix of coordinates
#' @param xy T/F: Return coordinates
#' 
#' @noRd
.extractFromVect <- function(x, y, xy) {

    .locationRestore(x)

    if (!inherits(y, "data.table")) y <- data.table::as.data.table(y)

    colnames(y) <- c("x", "y")
    n <- nrow(y)
    coords <- rep(NA_real_, 2L * n)
    coords[seq(1L, 2 * n, by = 2L)] <- y[["x"]]
    coords[seq(2L, 2 * n, by = 2L)] <- y[["y"]]

    args <- list(
        cmd = "v.what",
        map = sources(x),
        coordinates = coords,
        type = geomtype(x, TRUE),
        flags = .quiet(),
        intern = TRUE
    )

    info <- do.call(rgrass::execGRASS, args = args)

    nonnas <- which(grepl(info, pattern = "Category: "))
    nas <- which(grepl(info, pattern = "Nothing found."))

    lenNonnas <- length(nonnas)
    lenNas <- length(nas)

    # get index of non-NAs
    if (lenNonnas > 0L & lenNas >= 0L) {
        
        names(nonnas) <- rep("notna", lenNonnas)
        names(nas) <- rep("na", lenNas)
        together <- c(nonnas, nas)
        together <- sort(together)

        nonnasIndex <- which(names(together) == "notna")

    } else if (lenNonnas > 0L & lenNas == 0) {
        nonnasIndex <- 1L:n
    }

    ### at least one point was on the vector
    if (lenNonnas > 0L) {

        nons <- info[nonnas]
        nons <- sub(nons, pattern = "Category: ", replacement = "")
        nons <- as.integer(nons)

    }

    # vector has no data table (return IDs)
    if (nrow(x) == 0L) {

        id.x <- NULL

        out <- data.table::data.table(id.y = 1L:n, id.x = NA_integer_)

        if (lenNonnas > 0L) out[nonnasIndex, id.x := nons]

    # vector has data table
    } else {

        # make template of data table (1 row, all NAs)
        out <- as.data.table(x)[1L]

        classes <- sapply(out, class)
        cols <- names(x)
        
        for (i in seq_along(classes)) {

            if (classes[i] == "integer") {
                assign <- NA_integer_
            } else if (classes[i] == "numeric") {
                assign <- NA_real_
            } else if (classes[i] == "character") {
                assign <- NA_character_
            } else {
                assign <- NA
            }

            col <- cols[i]
            out[1L, (col) := assign]

        }

        # create template data table with all NAs
        out <- out[rep(1L, n)]
        id.y <- data.table::data.table(id.y = 1L:n)
        out <- cbind(id.y, out)

        # assign values to rows
        if (lenNonnas > 0L) {
        
            for (i in seq_along(cols)) {
                
                col <- cols[i]
                out[nonnasIndex, (col) := x@table[nons, get(col)]]
            
            }

        }

    } # vector has a data.table

    if (xy) out <- cbind(y, out)
    
    if (!faster("useDataTable")) out <- as.data.frame(out)
    out
    
} # EOF

