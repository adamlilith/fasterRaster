#' Extract values from a raster at locations in a points vector
#'
#' @description `extract()` obtains the values of a raster associated with the locations of a points vector. Note that only points vectors are accommodated--lines and areas vectors cannot me used.
#'
#' @param x A `GRaster`.
#'
#' @param y A `points` `GVector`, a `data.frame` or `matrix` where the first two columns represent longitude and latitude (in that order), or a two-element numeric vector where the first column represents longitude and the second latitude
#'
#' @param xy Logical: If `TRUE`, also return the coordinates of each point. Default is `FALSE.`
#'
#' @param cat Logical: If `TRUE`, report the category label rather than the cell value. Default is `FALSE.
#'
#' @returns A `data.frame`.
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
    function(x, y, xy = FALSE, cat = FALSE) {

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

        if (cat & ncat(x)[i] > 0L) args$flags <- c(args$flags, "f")
        this <- do.call(rgrass::execGRASS, args = args)

        pillars <- gregexpr(this, pattern = "\\|\\|")
        pillars <- unlist(pillars)
        ncs <- nchar(this)
        this <- substr(this, pillars + 2L, ncs)
        
        if (cat & ncat(x)[i] > 0) {

            this <- strsplit(this, split = "\\|")
            cats <- rep(NA_character_)
            for (index in indexHasCat) cats[index] <- this[[index]][2L]
            this <- cats

        } else {
            this <- as.numeric(this)
        }

        this <- data.table::data.table(xyzabc_ = this)
        names(this) <- names(x)[i]

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
    function(x, y, xy = FALSE, cat = FALSE) {

    if (ncol(y) < 2L) stop("Argument ", sQuote("y"), " must have at least two columns. The first must represent longitude and the second latitude.")
    if (ncol(y) > 2L) warning("Argument ", sQuote("y"), " has more than two columns. The first will be assumed to represent longitude and the second latitude.")

    y <- terra::vect(y, geom = colnames(y)[1L:2L], crs = crs(x), keepgeom = FALSE)
    extract(x = x, y = y, xy = xy, cat = cat)

    } # EOF
)

#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GRaster", y = "matrix"),
    function(x, y, xy = FALSE, cat = FALSE) {

    if (ncol(y) < 2L) stop("Argument ", sQuote("y"), " must have at least two columns. The first must represent longitude and the second latitude.")
    if (ncol(y) > 2L) warning("Argument ", sQuote("y"), " has more than two columns. The first will be assumed to represent longitude and the second latitude.")

    y <- terra::vect(y, geom = colnames(y)[1L:2L], crs = crs(x), keepgeom = FALSE)
    extract(x = x, y = y, xy = xy, cat = cat)

    } # EOF
)

#' @aliases extract
#' @rdname extract
#' @exportMethod extract
methods::setMethod(
    f = "extract",
    signature = c(x = "GRaster", y = "numeric"),
    function(x, y, xy = FALSE, cat = FALSE) {

    if (length(y) != 2L) stop("Argument ", sQuote("y"), " must have two values (longitude and latitude).")
    y <- cbind(y)
    colnames(y) <- c("x", "y")
    y <- terra::vect(y, geom = colnames(y)[1L:2L], crs = crs(x), keepgeom = FALSE)
    extract(x = x, y = y, xy = xy, cat = cat)

    } # EOF
)
