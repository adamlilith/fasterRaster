#' @title Replace layers of a GRaster
#'
#' @description The `[[<-` operator can be used to replace a layer in a multi-layer `GRaster`.
#'
#' @param x A `GRaster`.
#'
#' @param i A numeric integer, integer, logical, or character: Indicates the layer to replace. If a logical vector, then the vector must have the same length as there are layers in `x`.
#'
#' @param value Either a `GRaster` or `NULL`: If `NULL`, then the layer indicated by `i` will be removed.
#'
#' @example man/examples/ex_GRaster_GVector_subset_assign.r
#'
#' @name [[<-
#' @aliases [[<-,GRaster,ANY-method
#' @docType methods
#' @rdname replace_double_square_brackets
#' @exportMethod [[<-
methods::setMethod(
	"[[<-",
	signature = c(x = "GRaster", i = "ANY"),
	function(x, i, value) {

	if (!inherits(value, c("GRaster", "NULL"))) stop("Cannot assign a non-GRaster to a GRaster layer.")
	
	if (is.logical(i)) {
		if (length(i) != nlyr(x)) stop("A logical vector used to select GRaster layers must have the same length as there are number of layers.")
		i <- which(i)
	} else if (is.character(i)) {
		i <- match(i, names(x))
		if (anyNA(i)) stop("At least one name does not appear in this raster layer\'s names.")
	}
	
	if (any(i < 1L)) stop("Cannot use negative indices.")

	nLayers <- nlyr(x)
	out <- x
	if (is.null(value)) {

		notNulls <- setdiff(seq_len(nLayers), i)

		if (length(notNulls) == 0L) {
			out <- NULL
		} else {

			out <- new(
				"GRaster",
				location = .location(out),
				mapset = .mapset(out),
				workDir = faster("workDir"),
				crs = crs(out),
				projection = .projection(out),
				nLayers = length(notNulls),
				dimensions = dim(out),
				topology = topology(out),
				extent = as.vector(ext(out)),
				zextent = zext(out),
				sources = sources(out)[notNulls],
				names = names(out)[notNulls],
				datatypeGRASS = datatype(out, "GRASS")[notNulls],
				resolution = res(out),
				minVal = .minVal(out)[notNulls],
				maxVal = .maxVal(out)[notNulls],
				activeCat = activeCat(out)[notNulls],
				levels = out@levels[notNulls]
			)
		
		}
		
	# "value" is a GRaster
	} else {

		if (length(i) != nlyr(value)) stop("The number of rasters to be assigned is different from the number of indices.")

		compareGeom(x, value)
		
		insides <- i[i <= nLayers]
		outsides <- i[i > nLayers]

		lenIns <- length(insides)
		lenOuts <- length(outsides)

		if (lenOuts > 0L) {

			if (any(outsides != seq_len(lenOuts) + nLayers)) stop("Indices are not sequential.")

			valueOutsides <- lenIns + seq_len(lenOuts)
			valueOutsides <- valueOutsides[lenIns + seq_len(lenOuts)]
			out <- c(x, value[[valueOutsides]])

		}

		if (lenIns > 0L) {
		
			valueInsides <- seq_len(lenIns)

			srcs <- sources(out)
			srcs[i] <- sources(value)[valueInsides]

			names <- names(out)
			names[insides] <- names(value)[valueInsides]

			dts <- datatype(out, "GRASS")
			dts[insides] <- datatype(value, "GRASS")[valueInsides]

			minVal <- .minVal(out)
			minVal[insides] <- .minVal(value)[valueInsides]

			maxVal <- .maxVal(out)
			maxVal[insides] <- .maxVal(value)[valueInsides]

			ac <- activeCat(out, layer=seq_len(nlyr(x)))
			ac[insides] <- activeCat(value)[valueInsides]

			levs <- levels(out)
			levs[insides] <- levels(value)[valueInsides]

			out <- new(
				"GRaster",
				location = .location(out),
				mapset = .mapset(out),
				workDir = faster("workDir"),
				crs = crs(out),
				projection = .projection(out),
				nLayers = nlyr(out),
				dimensions = dim(out),
				topology = topology(out),
				extent = as.vector(ext(out)),
				zextent = zext(out),
				sources = srcs,
				names = names,
				datatypeGRASS = dts,
				resolution = res(out),
				minVal = minVal,
				maxVal = maxVal,
				activeCat = ac,
				levels = levs
			)

		}

	}
 	
	if (!is.null(out)) {
		if (length(anyDuplicated(out@names)) > 0L) out@names <- make.unique(out@names)
	}
	out

	} # EOF
)
