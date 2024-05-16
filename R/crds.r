#' Coordinates of a vector"s features or a raster"s cell centers
#'
#' @description Returns the coordinates of the center of cells of a `GRaster` or coordinates of a `GVector`'s vertices. The output will be a `matrix`, `data.frame`, or `list`. If you want the output to be a "points" `GVector`, use [as.points()].
#'
#' @param x A `GVector` or a `GRaster`.
#' @param z If `TRUE` (default), return x-, y-, and z-coordinates. If `FALSE`, just return x- and y-coordinates. For 2-dimensional objects, z-coordinates will all be 0.
#' @param na.rm Logical: If `TRUE`, remove cells that are `NA` (`GRaster`s only).
#'
#' @returns A `matrix`, `data.frame`, or `list`.
#'
#' @seealso [terra::crds()]
#'
#' @example man/examples/ex_crds.r
#'
#' @aliases crds
#' @rdname crds
#' @exportMethod crds
methods::setMethod(
    f = "crds",
    signature = c(x = "GRaster"),
    function(x, z = TRUE, na.rm = TRUE) {

        ..char <- NULL

        .locationRestore(x)
        .region(x)

        flags <- c(.quiet(), "overwrite")
        if (!na.rm) flags <- c(flags, "i")

        temp <- paste0(tempfile(), ".csv")
        rgrass::execGRASS("r.out.xyz", input = sources(x), output = temp, separator = "comma", flags = flags, intern = TRUE)
        # see https://grass.osgeo.org/grass82/manuals/r.out.xyz.html
        out <- data.table::fread(temp)

        if (is.3d(x)) {
            names(out) <- c("x", "y", "z", names(x))
        } else {
            names(out) <- c("x", "y", names(x))
        }

        # convert to numerics
        classes <- sapply(out, class)
        if (any(classes %in% "character")) {
            chars <- which(classes == "character")
            for (char in chars) out[, ..char] <- as.numeric(out[, ..char])
        }

        if (!faster("useDataTable")) out <- as.data.frame(out)
        out
    } # EOF
)

#' @aliases crds
#' @rdname crds
#' @exportMethod crds
methods::setMethod(
	f = "crds",
	signature = c(x = "GVector"),
	function(x, z = TRUE) .crdsVect(x, z = z)
)

#' @rdname crds
#' @export
st_coordinates <- function(x) {
	if (inherits(x, "GSpatial")) {
		.crdsVect(x, z = FALSE)
	} else {
		sf::st_coordinates(x)
	}
}

#' Extract coordinates for vector
#'
#' @param x A GVector or [sources()] name of one.
#' @param z T/F Extract z-coordinate?
#' @param gtype Character: [geomtype()] of the vector.
#' 
#' @noRd
.crdsVect <- function(x, z, gtype = NULL) {

	if (inherits(x, "GVector")) {
		.locationRestore(x)
		gtype <- geomtype(x)
		src <- sources(x)
	} else {
		src <- x
	}

	# if lines or polygons, convert to points first
	if (gtype %in% c("lines", "polygons")) {

		stop("crds() will only work on points vectors.")
		
		# ####### NB seems to work on lines but disagrees with st_coordinates() and crds()

		# if (z && is.3d(x)) warning("z coordinates ignored.")
	
		# src <- .makeSourceName("points", "vect")
		# rgrass::execGRASS("v.to.points", input=sources(x), output=src, use="vertex", flags=c(.quiet(), "overwrite"), intern=TRUE)
		# pts <- .makeGVector(src)
		# pts <- vect(pts)

		# out <- terra::crds(pts)

	} else if (gtype == "points") {
		
		data <- rgrass::execGRASS(
			cmd = "v.to.db",
			map = src,
			flags = c(.quiet(), "p"),
			option = "coor",
			type = "point",
			intern = TRUE
		)
		
		data <- data[-1L]
		cutAt <- which(data == "Reading features...")
		if (length(cutAt) > 0L) data <- data[1L:(cutAt - 1L)]
		
		data <- strsplit(data, split="\\|")
		data <- lapply(data, as.numeric)
		out <- do.call(rbind, data)
		
		if (z) {
			out <- out[ , 2L:4L, drop = FALSE]
			colnames(out) <- c("x", "y", "z")
		} else {
			out <- out[ , 2L:3L, drop = FALSE]
			colnames(out) <- c("x", "y")
		}
		
	}	
		
	if (faster("useDataTable")) out <- data.table::as.data.table(out)
	out
	
}
