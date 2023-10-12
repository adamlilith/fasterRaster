#' Extract values from a GRaster at locations in a points GVector
#'
#' @description `extract()` obtains the values of a `GRaster` or `GVector` associated with the locations of a set of points.
#'
#' @param x A `GRaster` or `GVector`.
#'
#' @param y A `points` `GVector`, *or* a `data.frame` or `matrix` where the first two columns represent longitude and latitude (in that order), *or* a two-element numeric vector where the first column represents longitude and the second latitude. Values of `x` will be extracted from the points in `y`.
#'
#' @param xy Logical: If `TRUE`, also return the coordinates of each point. Default is `FALSE.`
#'
#' @param cats Logical: If `TRUE` and `x` is a [categorical raster][tutorial_raster_data_types], then return the category labels instead of the values. If `TRUE` and `x` is a `GVector`, then ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Default is `FALSE.
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
        xy = FALSE,
        cats = FALSE
    ) {

    if (geomtype(y) != "points") stop("Argument", sQuote("y"), " must be a points vector.")
    
    nLayers <- nlyr(x)
    for (i in seq_len(nLayers)) {

        args <- list(
            cmd = "r.what",
            map = sources(x)[i],
            points = sources(y),
            null_value = "NA",
            flags = c("quiet", "overwrite"),
            intern = TRUE
        )

        vals <- do.call(rgrass::execGRASS, args = args)

        pillars <- gregexpr(vals, pattern = "\\|\\|")
        pillars <- unlist(pillars)
        ncs <- nchar(vals)
        vals <- substr(vals, pillars + 2L, ncs)

        if (is.cell(x)[i]) {
            vals <- as.integer(vals)
        } else {
            vals <- as.numeric(vals)
        }

        this <- data.table::data.table(TEMPTEMP__ = vals)
        names(this) <- names(x)[i]

        # category label instead of value
        if (cats && is.factor(x)[i]) {

            levs <- levels(x[[i]])[[1L]]
            this <- levs[match(vals, levs[[1L]]), 2L]
            names(this) <- paste0(names(x)[i], "_cat")

        }

        if (i == 1L) {
            out <- this
        } else {
            out <- cbind(out, this)
        }

    } # next raster
    
    if (xy) {

        coords <- crds(y, z = is.3d(y))
        coords <- data.table::as.data.table(coords)
        out <- cbind(coords, out)

    }

    if (!getFastOptions("useDataTable")) out <- as.data.frame(out)
    out

    } # EOF
)

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
    function(x,
             y,
             xy = FALSE
    ) {
    
    if (geomtype(y) != "points") stop("Argument", sQuote("y"), " must be a points vector.")
    if (is.3d(y)) warning("Coordinates in the z-dimension will be ignored.")

    coordsXY <- crds(y, z = FALSE)
    n <- nrow(coordsXY)
    coords <- rep(NA_real_, 2L * n)
    coords[seq(1L, 2 * n, by = 2L)] <- coordsXY$x
    coords[seq(2L, 2 * n, by = 2L)] <- coordsXY$y

    args <- list(
        cmd = "v.what",
        map = sources(x),
        coordinates = coords,
        type = geomtype(x, TRUE),
        flags = "quiet",
        intern = TRUE
    )

    vals <- do.call(rgrass::execGRASS, args = args)

    nonnas <- which(grepl(vals, pattern = "Category: "))
    nas <- which(grepl(vals, pattern = "Nothing found."))

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

        nons <- vals[nonnas]
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

    if (xy) out <- cbind(coordsXY, out)
    
    if (!getFastOptions("useDataTable")) out <- as.data.frame(out)
    out
    
    } # EOF
)

