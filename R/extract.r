#' Extract values from a GRaster at locations in a points GVector
#'
#' @description `extract()` obtains the values of a `GRaster` or `GVector` associated with the locations of a set of points. The output depends on the input:
#' * **Case #1: `x` is a numeric or integer `GRaster` and `y` is a points `GVector`**: Returns values of cells that have points. If `xy` is `TRUE`, also returns the coordinates of the points.
#' * **Case #2: `x` is a categorical (factor) `GRaster` and `y` is a points `GVector`**: Same as case #1, but if `cats` is `TRUE`, returns category labels of cells that have points. If `xy` is `TRUE`, also returns the coordinates of the points.
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
#' @param cats Logical (extracting from a raster): If `TRUE` (default) and `x` is a categorical `GRaster`, then return the category labels instead of the values.
#'
#' @param verbose Logical: If `TRUE`, display progress (will only function when extracting from points on a `GRaster` when the number of `GRaster`s is large).
#'
#' @param ... Arguments to pass to [project()]. This is used only if extracting from a `GRaster` at locations specified by a `GVector`, and they have a different coordinate reference system. In this case, users should specify the `wrap` argument to [project()].
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
        cats = TRUE,
        verbose = FALSE,
        ...
    ) {

    .locationRestore(x)

    sameCRS <- compareGeom(x, y, stopOnError = FALSE, messages = FALSE)
    if (!sameCRS) {
    
        warning("The GVector has a different coordinate reference system than the GRaster.\n  The GVector was projected to the CRS of the GRaster. If the GVector spans\n  the international date line, you may need to specify `wrap = TRUE` when\n  using this function.", immediate. = TRUE)

        dots <- list(...)
        if (any(names(dots) == "wrap")) {
            wrap <- dots$wrap
        } else {
            wrap = FALSE
        }
        y <- project(y, x, wrap = wrap)

    }

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
                    
                    if (!.vHasDatabase(y)) .vAttachDatabase(y)

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

                    extracted <- .vAsDataTable(y)

                } else {
                    extracted <- data.table::data.table(NULL)
                }

                # sample SD, var, CV
                if (sampleSd | sampleVar | sampleCv) {
                    
                    sampleStats <- .sampleStatsFromRast(i = i, x = x, y = y, fun = fun, extracted = extracted, prefix = prefix)
                    extracted <- cbind(extracted, sampleStats)
                    
                }

                extracted <- extracted[!duplicated(extracted)]

                if (any(names(extracted) == "cat")) extracted$cat <- NULL
                if (any(names(extracted) == "cat_")) extracted$cat_ <- NULL
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

        if (xy) {

            coords <- crds(y, z = is.3d(y))
            out <- data.table::as.data.table(coords)

        }

        vals <- .extractFromRasterAtPoints(x = x, y = y, cats = cats, verbose = verbose)

        if (exists("out", inherits = FALSE)) {
            out <- cbind(out, vals)
        } else {
            out <- vals
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
        cats = TRUE
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
        cats = TRUE
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
        cats = TRUE
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
        cats = TRUE
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
#' @param nperSet Number of points to extract... too large throws an error. 1000 seems to break, so going with 900.
#' @param data.table return as `data.table`
#' 
#' @noRd
.extractFromVect <- function(x, y, xy, nPerSet = 900L, data.table = TRUE) {

    .locationRestore(x)

    if (!inherits(y, "data.table")) y <- data.table::as.data.table(y)

    colnames(y) <- c("x", "y")
    n <- nrow(y)

	if (n > nPerSet) {

		sets <- ceiling(n / nPerSet)
		out <- data.table::data.table()
		for (set in seq_len(sets)) {
			
			omnibus::say(set)
			
			yy <- y[(1L + (set - 1L) * nPerSet):min(set * nPerSet, n)]
			thisOut <- .extractFromVect(x, y = yy, xy = xy, data.table = TRUE)
			out <- rbind(out, thisOut)
		
		}
		
	} else {

		coords <- rep(NA_real_, 2L * n)
		coords[seq(1L, 2 * n, by = 2L)] <- y[["x"]]
		coords[seq(2L, 2 * n, by = 2L)] <- y[["y"]]

		gtype <- geomtype(x, grass = TRUE)

		info <- rgrass::execGRASS(
			cmd = "v.what",
			map = sources(x),
			coordinates = coords,
			type = gtype,
			flags = c(.quiet(), "verbose"), # could use "g" to make output easier to parse
			intern = TRUE
		)

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
		
	}
	
	if (!data.table & !faster("useDataTable")) out <- as.data.frame(out)
	out
    
} # EOF

#' Extract values from a set of rasters at points
#'
#' @param x `GRaster` or [sources()] name of one or more `GRaster`s.
#' @param y A points `GVector` or [sources()] name of one
#' @param dtype **GRASS** format `datatype` of each raster in `x`. Will be obtained from `x` if `x` is a `GRaster`.
#' @param xNames [names()] of `x`.  Will be obtained from `x` if `x` is a `GRaster`.
#' @param levels [levels()] of a `GRaster`. Will be taken from `x` if `NULL` and `x` is a `GRaster`.
#' @param cats Logical: Extract category labels instead of values
#' @param verbose Logical.
#'
#' @returns A `data.table`.
#'
#' @noRd
.extractFromRasterAtPoints <- function(
    x,
    y,
    xNames = NULL,
    dtype = NULL,
    levels = NULL,
    cats = TRUE,
    verbose = TRUE
) {

    if (inherits(x, "GRaster")) {
        
        nLayers <- nlyr(x)
        dtype <- datatype(x, "GRASS")
        xNames <- names(x)
        xSrc <- sources(x)
        levels <- levels(x)

    } else {

        if (is.null(xNames)) stop("If `x` is the sources() name of a GRaster, `xNames` must be supplied (i.e., not NULL).")
        if (is.null(levels)) stop("If `x` is the sources() name of a GRaster, `levels` must be supplied (i.e., not NULL).")
        if (is.null(dtype)) stop("If `x` is the sources() name of a GRaster, `dtype` must be supplied (i.e., not NULL).")
        nLayers <- length(x)
        xSrc <- x
    }

    if (inherits(y, "GVector")) {
        ySrc <- sources(y)
    } else {
        ySrc <- y
    }

    ### extracting sets of rasters at a time to obviate issues with too-long of a GRASS command string if there are too many rasters
    layersAtATime <- 30L
    sets <- ceiling(nLayers / layersAtATime)
    if (verbose | faster("verbose")) {
        omnibus::say("Extracting values...")
        if (sets > 1) pb <- utils::txtProgressBar(min = 0, max = sets, initial = 0, style = 3, width = 30)
    }

    # # don't know why, but having this here obviates extraction of all NAs on first extract attempt
    if (inherits(x, "GRaster")) x <- x + 0

    for (set in seq_len(sets)) {

        if ((verbose | faster("verbose")) & sets > 1)  utils::setTxtProgressBar(pb, set)

        index <- (layersAtATime * (set - 1) + 1):min(layersAtATime * set, nLayers)
        xxSrc <- xSrc[index]

        vals <- rgrass::execGRASS(
            cmd = "r.what",
            map = xxSrc,
            points = ySrc,
            null_value = "NA",
            separator = "pipe",
            flags = c(.quiet(), "overwrite"),
            intern = TRUE
        )

        vals <- strsplit(vals, split = "\\|")
        vals <- do.call(rbind, vals)
        vals <- vals[ , 4:ncol(vals)]
        vals <- data.table::as.data.table(vals)
        names(vals) <- xNames[index]

        suppressWarnings(
            for (i in 1:ncol(vals)) {
                dt <- dtype
                col <- names(vals)[i]
                if (dt[i] == "CELL") {
                    vals[ , (col) := as.integer(get(col)), by = .SD]
                } else {
                    vals[ , (col) := as.numeric(get(col)), by = .SD]
                }
            }
        )

        if (exists("out", inherits = FALSE)) {
            out <- cbind(out, vals)
        } else {
            out <- vals
        }

    } # next set of rasters
    if ((verbose | faster("verbose")) & sets > 1) close(pb)

    # category label instead of value
    levelRows <- sapply(levels, nrow)
    if (cats & any(levelRows > 0)) {

        facts <- which(levelRows > 0)
        for (fact in facts) {

            factName <- xNames[fact]

            levs <- levels[[fact]]
            this <- levs[match(out[[fact]], levs[[1L]]), 2L]
            # names(this) <- names(xx)[fact]
            # this <- this[ , lapply(.SD, as.factor)] # convert to factor... do we want this?
            out[ , (factName) := this[[1L]]]

        }

    } # extracting category labels

    out

}

