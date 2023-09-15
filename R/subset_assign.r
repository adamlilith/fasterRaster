#' Sub-setting operator for GRasters and GVectors
#'
#' @description The `[` and `[[` operators do different things depending on whether they are applied to a `GRaster` or `GVector` and whether the index is positive or negative. Positive indices select the given geometry/row, column, or layer. Negative indices remove them. In general, indices can be numeric integers, integers, logical vectors, or character vectors.
#' * `GVector`s:
#'     * `[` operator: Returns a subset of geometries (i.e., points, lines, or polygons) of the `GVector`. For example, `vector[2:4]` will return the second through the fourth geometries. `vector[2:4, 1]` returns a vector with the second and third geometries and the first column of its attribute table.
#'     * `$` and `[[` operator: Returns a vector with the selected columns in its data frame. For example, `vector[[2:4]]` returns the `GVector`, but with just columns 2 through 4 of its data frame. `vector$State` returns the values in the `State` column.
#'     * `$<-` and `[[<-` operator: Replaces the values in the column(s) indicated by the index by the new value. For example, `vector$State <- "Kansas"` replaces all values in the `State` column with heaven.
#' * `GRaster`s:
#'     * `$` and `[[` operators: Returns `GRaster`s from a "[stack][c]" of `GRaster`s. For example, `raster[[2:3]]` returns the second and third rasters in a stack of `GRaster`s, `raster[[c("bio1", "bio12")]]` returns the two rasters with [names()] "bio1" and "bio12", and `raster$bio12` returns the `bio12` raster.
#'     * `[<-` operator with the assignment operator, `<-`: Assigns the given value to all cells in the `GRaster`. For examples, `raster[] <- 7` replaces all cell values in `raster` with 7.
#'     * `$<-` and [[<-` operators: Replaces existing layers or adds new layers to a `GRaster`. For example, `x[[2]] <- newRaster` makes the second layer of `x` `newRaster`. If `x` already has two layers, the second one is replaced. If it only had one layer, a second is added. `x$bio12 <- newRaster` replaces the layer named `bio12` with `newRaster`.
#'     * `add<-`: "Stacks" two `GRaster`s. This is the same as stacking rasters with [c()]. For example, `c(raster1, raster2)` and `add(raster1) <- raster2` are equivalent.
#'
#' @param x A `GVector`, or a `GRaster` with one or more layers.
#'
#' @param i A character, numeric, integer, or logical vector:
#' * `GVector`s:
#'     * `[` operator: Indicates the geometries/rows to retain.
#'     * `[[` operator: Indicates which columns to retain of a `GVector` or which layers to replace or subset of a `GRaster`.
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
	i <- .layerIndex(i, x, recycle = TRUE)
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
		levels = cats(x)[i]
	)
	
	if (length(anyDuplicated(out@names)) > 0L) out@names <- make.unique(out@names)
	out

	} # EOF
)

#' @name subset_assign
#' @aliases subset_assign
#' @rdname subset_assign
#' @exportMethod $
methods::setMethod(
    "$",
    signature = c(x = "GRaster"),
    function(x, name) x[[name]]
)

#' @name subset_assign
#' @aliases subset_assign
#' @rdname subset_assign
#' @exportMethod $<-
methods::setMethod(
    "$<-",
    signature = c(x = "GRaster"),
    function(x, name, value) {

		if (is.logical(name)) {
			if (length(name) < nlyr(x)) name <- rep(name, length.out = nlyr(x))
			i <- which(name)
		} else if (is.character(name)) {
			i <- match(name, names(x))
		}

		x[[name]] <- value
		names(x)[i] <- names(value)
		x

	} # EOF
)

#' @aliases [[<-
#' @rdname subset_assign
#' @exportMethod [[<-
methods::setMethod(
	"[[<-",
	signature = c(x = "GRaster", i = "ANY", j = "ANY"),
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

			ac <- activeCat(out, layer=seq_len(nlyr(x)))
			ac[insides] <- activeCat(value)[valueInsides]

			levs <- levels(out)
			levs[insides] <- levels(value)[valueInsides]

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
	f = "add<-",
	signature = c(x = "GRaster", value = "GRaster"),
	function(x, value) c(x, value)
)

#' @name subset_assign
#' @aliases subset_assign
#' @rdname subset_assign
#' @exportMethod $<-
methods::setMethod(
    "$<-",
    signature = c(x = "GRaster"),
    function(x, name, value) x[[name]] <- value
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

	.restore(x)

	nr <- ngeom(x)
	if (is.logical(i)) {
		if (length(i) < nr) i <- rep(i, length.out = nr)
		i <- which(i)
	}

	if (any(i > 0L) & any(i < 0L)) stop("Cannot mix positive and negative indices.")

	if (all(i < 0L)) {
		reverseSelect <- TRUE
		iRev <- i
		i <- -1L * i
		removeAll <- length(i) == nrow(x) && all(sort(i) == seq_len(nrow(x)))
	} else {
		reverseSelect <- removeAll <- FALSE
	}
	if (any(i > nr)) stop("Index out of bounds.")

	if (removeAll) {
		out <- NULL # removed all rows
	} else {

		# **keep** rows
		iSeq <- seqToSQL(i)
		src <- .makeSourceName("v_extract", "vector")

		args <- list(
			cmd = "v.extract",
			input = sources(x),
			cats = iSeq,
			output = src,
			new = 1,
			flags = c("quiet", "overwrite", "t"),
			intern = FALSE
		)

		if (reverseSelect) args$flags <- c(args$flags, "r")
		do.call(rgrass::execGRASS, args = args)
		
		if (nrow(x) == 0L) {
			table <- NULL
		} else if (reverseSelect) {
			table <- x@table[iRev]
		} else {
			table <- x@table[i]
		}

		out <- .makeGVector(src, table = table)

	} # not removing all rows
	out

	} # EOF
)

#' @aliases subset_assign
#' @rdname subset_assign
#' @exportMethod [
methods::setMethod(
	"[",
	signature = c(x = "GVector"),
	function(x, i, j) {

	.restore(x)

	nr <- ngeom(x)

	if (missing(i)) {
		out <- x[[j]]
	} else {

		if (is.logical(i)) {
			if (length(i) < nr) i <- rep(i, length.out = nr)
			i <- which(i)
		}

		if (any(i > 0L) & any(i < 0L)) stop("Cannot mix positive and negative indices.")

		if (all(i < 0L)) {
			reverseRowSelect <- TRUE
			iRev <- i
			i <- -1L * i
			removeAll <- length(i) == nrow(x) && all(sort(i) == seq_len(nrow(x)))

		} else {
			reverseRowSelect <- removeAll <- FALSE
		}
		if (any(i > nr)) stop("Index out of bounds.")

		if (removeAll) {
			out <- NULL # removed all rows
		} else {

			# **keep** rows
			iSeq <- seqToSQL(i)
			src <- .makeSourceName("v_extract", "vector")

			args <- list(
				cmd = "v.extract",
				input = sources(x),
				cats = iSeq,
				output = src,
				new = 1,
				flags = c("quiet", "overwrite", "t"),
				intern = FALSE
			)

			if (reverseRowSelect) args$flags <- c(args$flags, "r")
			do.call(rgrass::execGRASS, args = args)
			
			if (nrow(x) == 0L) {
				table <- NULL
			} else {

				table <- x@table

				# select columns
				if (missing(j)) {
					removeAllCols <- FALSE
				} else {

					nc <- ncol(table)
					if (is.character(j)) {
						j <- match(j, names(table))
					} else if (is.logical(j)) {
						if (length(j) < nc) j <- rep(j, length.out = nc)
						j <- which(j)
					}
					
					if (all(j < 0L)) {
						reverseColSelect <- TRUE
						j <- j * -1L
						removeAllCols <-  (all(sort(j) == seq_len(ncol(x))))
					} else {
						reverseColSelect <- removeAllCols <- FALSE
					}

					if (any(j > nc)) stop("Index out of bounds.")

					if (removeAllCols) {
						table <- data.table::data.table(NULL)
					} else {
					
						if (reverseColSelect) {
							j <- setdiff(names(x), names(x)[j])
						} else {
							j <- names(x)[j]
						}
						table <- table[ , ..j]

					}
				
				}

				# select rows
				if (!removeAllCols) {
					if (reverseRowSelect) {
						table <- table[iRev]
					} else {
						table <- table[i]
					}
				}
			
			} # vector has table
			out <- .makeGVector(src, table = table)

		} # keep some rows (vs discarding all)

	} # if selecting some rows
	out

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
	if (nc == 0L) stop("Vector has no data table associated with it.")

	# turn i into integer
	if (is.logical(i)) {
		if (length(i) <- nc) i <- rep(i, length.out = nc)
		i <- which(i)
	} else if (is.character(i)) {
		i <- match(i, names(x))
  		if (anyNA(i)) stop("At least one named column does not exist in this vector\'s data table.")
	}

	if (any(i < 0L) & any(i > 0L)) stop("Cannot have both positive and negative indices.")

	if (all(i < 0L)) {
		reverseSelect <- TRUE
		i <- i * -1L
		removeAll <- length(i) == ncol(x) && all(sort(i) == seq_len(ncol(x)))
	} else {
		reverseSelect <- removeAll <- FALSE
	}

	if (any(i > nc)) stop("Index out of bounds.")
	
	# removed all columns
	if (removeAll) {
		x@table <- data.table::data.table(NULL)
	# keep some columns
	} else {
		if (reverseSelect) {
			i <- setdiff(names(x), names(x)[i])
		} else {
			i <- names(x)[i]
		}

		x@table <- x@table[ , ..i]
	}
	x
	
	} # EOF
)

#' @aliases subset_assign
#' @rdname subset_assign
#' @exportMethod $
methods::setMethod(
	"$",
	signature = c(x = "GVector"),
	function(x, name) x@table[[name]]
)

#' @aliases subset_assign
#' @rdname subset_assign
#' @exportMethod $<-
methods::setMethod(
	"$<-",
	signature = c(x = "GVector"),
	function(x, name, value) {

	x@table <- data.table::set(x@table, i = NULL, j = name, value = value)
	x

	} # EOF
)
