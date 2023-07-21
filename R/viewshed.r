#" Areas visible from points on a raster
#"
#" @description `viewshed()` calculates the area that can be seen from a given point on a raster. It accounts for elevation, and optionally) curvature of the Earth and atmospheric refraction.
#" 
#" @param x A `GRaster` typically representing elevation.
#" 
#" @param loc A "points" `GVector`, a two-column matrix with coordinate pairs (longitude then latitude), or a pair of numeric values indicating longitude and latitude of the point from which the viewshed should be calculated. If more than one point is supplied, the final map will reflect the combined viewshed from all points.
#" 
#" @param observer Numeric: The altitude of the observer above the surface (in meters). The default is 1.8 (approximately eye-level).
#" 
#" @param target Numeric: The altitude of the target above the surface (in meters). The default is 0.
#" 
#" @param curvature Logical: If `TRUE` (default), account for curvature of the Earth.
#" 
#" @param refraction Logical: If `TRUE`, account for atmospheric refraction. Default is `FALSE`.
#" 
#" @param refractCoeff Numeric between 0 and 1: Refraction coefficient. The default is 0.14286.
#" 
#" @param maxDist Numeric >= 0: Maximum distance for which to calculate viewshed. Default is `Inf` (infinite). This is in map units (usually meters).
#" 
#" @returns A `GRaster`.
#" 
#" @example man/examples/ex_viewshed.r
#" 
#" @aliases viewshed
#" @rdname viewshed
#" @exportMethod viewshed
methods::setMethod(
    f = "viewshed",
    signature = c(x = "GRaster"),
    function(
        x,
        loc,
        observer = 1.8,
        target = 0,
        curvature = TRUE,
        refraction = FALSE,
        refractCoeff = 0.14286,
        maxDist = Inf
    ) {

    if (maxDist < 0) stop("Argument", sQuote("maxDist"), " must be >= 0.")
    if (refractCoeff < 0 | refractCoeff > 1) stop("Argument ", sQuote("refractCoeff"), " must be in the range [0, 1].")

    .restore(x)
    region(x)

    ### process coordinates
    if (inherits(loc, "GVector")) {
        coords <- crds(loc, z = FALSE)
    } else if (inherits(loc, "numeric")) {
        coords <- rbind(loc)
    } else if (!inherits(loc, "data.frame")) {
        coords <- as.matrix(loc)
    } else if (!inherits(loc, "matrix")) {
        stop("Argument ", sQuote("loc"), " must be a GVector, a two-column matrix, or a numeric vector with two values.")
    }

    args <- list(
        cmd = "r.viewshed",
        input = NA_character_,
        output = NA_character_,
        coordinates = NA_real_,
        observer_elevation = observer,
        target_elevation = 0,
        refraction_coeff = refractCoeff,
        memory = getFastOptions("memory"),
        directory = tempdir(),
        flags = c("b", "quiet", "overwrite"),
        intern = TRUE
    )

    if (curvature) args$flags <- c(args$flags, "c")
    if (refraction) args$flags <- c(args$flags, "c")
    if (is.infinite(maxDist)) args$max_distance <- -1
    if (!is.infinite(maxDist)) args$max_distance <- madDist

    nLayers <- nlyr(x)
    for (i in seq_len(nLayers)) {

        args$input <- .gnames(x)[i]

        nPoints <- nrow(coords)
        gns <- .makeGName("viewshed", "raster", nPoints)
        for (j in seq_len(nPoints)) {

            args$output <- gns[j]
            args$coordinates <- unlist(coords[j, ])

            do.call(rgrass::execGRASS, args = args)
            this <- .makeGRaster(gns[j], "viewshed")
            if (j == 1L) {
                thisOut <- this
            } else {
                thisOut <- c(thisOut, this)
            }

        } # next coordinate

        # collapse across points
        if (nPoints > 1L) {
            thisOut <- sum(thisOut)
            thisOut <- thisOut >= 1L
        }

        if (i == 1L) {
            out <- thisOut
        } else {
            out <- c(out, thisOut)
        }

    } # next raster
    out

    } # EOF
)
