#' Sub-setting operator for GRasters and GVectors
#'
#' @description The `[` and `[[` operators do different things depending on whether they are applied to a `GRaster` or `GVector`:
#' * `GVector`s:
#'     * `[` operator: Returns a subset of geometries (i.e., points, lines, or polygons) of the `GVector`. For example, `vector[2:4]` will return the second through the fourth geometries.
#'     * `[[` operator: Returns a vector with the selected columns in its data frame. For example, `vector[[2:4]]` returns the `GVector`, but with just columns 2 through 4 of the data frame.
#' * `GRaster`s:
#'     * `[` operator with the assignment operator, `<-`: Assigns the given value to all cells in the `GRaster`. For examples, `raster[] <- 7` replaces all cell values in `raster` with 7.
#'     * `[[` operator: Returns `GRaster`s from a "[stack][c]" of `GRaster`s. For example, `raster[[2:3]]` returns the second and third rasters in a stack of `GRaster`s, and `raster[[c("bio1", "bio12")]]` returns the two rasters with [names()] "bio1" and "bio12".
#'     * `[[` operator with the assignment operator, `<-`: Replaces existing layers or adds new layers to a `GRaster`. For example, `x[[2]] <- newRaster` makes the second layer of `x` `newRaster`. If `x` already has two layers, the second one is replaced. If it only had one layer, a second is added.
#'     * `add<-`: "Stacks" two `GRaster`s. This is the same as stacking rasters with [c()]. For example, you `c(raster1, raster2)` and `add(raster1) <- raster2` are equivalent.
#'
#' @param x A `GVector`, or a `GRaster` with one or more layers.
#'
#' @param i A character, numeric, integer, or logical vector:
#' * `GVector`s:
#'     * `[` operator: Indicates the geometries/rows to retain.
#'     * `[[` operator: Indicates which columns to retain of a `GVector` or which layers to replace or fetch of a `GRaster`.
#'
#' @param j Not used.
#' @param value A numeric, integer, logical value (including `NA`), or `NULL`: Value to assign to all cells in a raster.
#'
#' @returns A `GRaster`.
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @name subset_assign
#' @aliases subset_assign
#' @rdname subset_assign
#' @exportMethod [[
methods::setMethod(
	"[[",
	signature = c(x = "GRaster", i = "ANY"),
	function(x, i) {

	# test indices
	if (inherits(i, "character")) {
		if (any(!(i %in% names(x)))) stop("At least one name does not match a raster in this stack.")
		i <- match(i, names(x))
	}
	if (any(!(i %in% seq_len(nlyr(x))))) stop("Index out of bounds.")
	
	mm <- minmax(x)
	
	out <- new(
		"GRaster",
		location = location(x),
		mapset = mapset(x),
		crs = crs(x),
		projection = .projection(x),
		nLayers = length(i),
		dimensions = dim(x),
		topology = topology(x),
		extent = as.vector(ext(x)),
		zextent = zext(x),
		sources = sources(x)[i],
		names = names(x)[i],
		datatypeGRASS = datatype(x, "GRASS")[i],
		resolution = res(x),
		minVal = mm["min", i],
		maxVal = mm["max", i],
		activeCat = x@activeCat[i],
		levels = levels(x)[i]
	)
	
	if (length(anyDuplicated(out@names)) > 0L) out@names <- make.unique(out@names)
	out

	} # EOF
)

#' @aliases [[<-
#' @rdname subset_assign
#' @exportMethod [[<-
methods::setMethod(
	"[[<-",
	signature = c(x = "GRaster", i = "ANY", j = "missing"),
	function(x, i, j, value) {

	if (!inherits(value, c("GRaster", "NULL"))) stop("Cannot assign a non-GRaster to a GRaster layer.")
	
	if (is.logical(i)) i <- which(i)
	if (is.character(i)) {
		i <- match(i, names(x))
		if (anyNA(i)) stop("At least one name does not appear in this raster layer\'s names.")
	}

	nLayers <- nlyr(x)
	out <- x
	if (is.null(value)) {

		notNulls <- setdiff(seq_len(nLayers), i)

		if (length(notNulls) == 0L) {
			out <- NULL
		} else {

			out <- new(
				"GRaster",
				location = location(out),
				mapset = mapset(out),
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

			ac <- out@activeCat
			ac[insides] <- activeCat(value)[valueInsides]

			levs <- levels(out)
			levs[insides] <- levels(value)[levs]

			out <- new(
				"GRaster",
				location = location(out),
				mapset = mapset(out),
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

#' @aliases add<-
#' @rdname subset_assign
#' @exportMethod add<-
methods::setMethod(
	"add<-",
	signature = c(x = "GRaster", value = "GRaster"),
	function(x, value) c(x, value)
)

#' @rdname subset_assign
#' @aliases subset_assign
#' @exportMethod [<-
setMethod(
	f = "[<-",
	signature = "GRaster",
	definition = function(x, value) {
	
	if (!inherits(value, c("numeric", "integer", "logical"))) stop("Can only assign numeric, integer, or logical values to a raster.")
	if (length(value) != 1L) stop("Cannot assign multiple values to a raster.")
	
	.restore(x)
	region(x)
	
	if (is.na(value)) {
		value <- "null()"
	} else if (is.logical(value)) {
		value <- as.integer(value)
	}
	
	nLayers <- nlyr(x)
	srcs <- .makeSourceName(x, "raster", nLayers)
	for (i in seq_len(nLayers)) {
	
		ex <- paste0(srcs[i], " = ", value)
		args <- list(
			cmd = "r.mapcalc",
			expression = ex,
			flags = c("quiet", "overwrite"),
			intern = TRUE
		)
		do.call(rgrass::execGRASS, args = args)
	
	} # next raster
	.makeGRaster(srcs, 'layer')
	
	} # EOF
)

#' @aliases subset_assign
#' @rdname subset_assign
#' @exportMethod [
methods::setMethod(
	"[",
	signature = c(x = "GVector"),
	function(x, i) {

	nr <- nrow(x)
	if (is.logical(i)) i <- which(i)
	if (any(i < 1L | i > nr)) stop("Index out of bounds.")

	# select rows to drop	
	rows <- seq_len(nr)
	drops <- rows[!(rows %in% i)]

	if (length(i) >= nr) {
		where <- paste0("cat IN (", paste0(drops, collapse = ","), ")")
	} else {
		where <- paste0("cat NOT IN (", paste0(i, collapse = ","), ")")
	}
	
cat("Command line too long when selecting > ~1500 records.")
utils::flush.console()

	src <- .makeSourceName(NULL, rastOrVect = "vector")
	args <- list(
		cmd = "v.db.droprow",
		input = sources(x),
		layer = .dbLayer(x),
		output = src,
		where = where,
		flags = c("quiet", "overwrite"),
		intern = FALSE
	)

	do.call(rgrass::execGRASS, args=args)
	.makeGVector(src)

	} # EOF
)

#' @aliases subset_assign
#' @rdname subset_assign
#' @exportMethod [[
methods::setMethod(
	"[[",
	signature = c(x = "GVector"),
	function(x, i) {

	nc <- ncol(x)
	if (is.logical(i)) {
		i <- which(i)
	} else if (is.character(i)) {
		misses <- !(i %in% names(x))
		if (any(misses)) stop("At least one named column does not exist in this vector\"s data table.")
		i <- which(names(x) %in% i)
	}

	if (any(i < 1L | i > nc)) stop("Index out of bounds.")
	
	cols <- seq_len(nc)
	drops <- cols[!(cols %in% i)]
	drops <- names(x)[drops]

	src <- .copyGSpatial(x)
	args <- list(
		cmd = "v.db.dropcolumn",
		map = src,
		columns = drops,
		flags = "quiet",
		layer = "1",
		intern = FALSE
	)

	do.call(rgrass::execGRASS, args=args)
	.makeGVector(src)

	} # EOF
)
