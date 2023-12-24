#' "Stack" GRasters and combine GVectors
#'
#' @description `GRaster`s can be "stacked" using this function, effectively creating a multi-layered raster. This is different from creating a 3-dimensional raster, though such an effect can be emulated using stacking. `GVector`s can be combined into a single vector.  Stacks can only be created when:
#' * All objects are the same class (either all `GRaster`s or all `GVector`s).
#' * All objects have the same coordinate reference system (see crs()).
#' * For `GRaster`s:
#'      * Horizontal extents are the same (see [ext()]).
#'      * Horizontal dimensions are the same (see [res()]).
#'      * The topology (2- or 3-dimensional) must be the same. If 3D, then all rasters must have the same number of depths and vertical extents (see [topology()]).
#' * For `GVector`s:
#'      * The geometry (points, lines, or polygons) must be the same. See [geomtype()].
#'
#' If features (boundaries, lines, etc.) of `GVector`s that are combined are identical or nearly identical, they can be cleaned using [removeDupNodes()] and [snap()].
#'
#' Data tables associated with `GVector`s will be combined if each vector has a table and if each table has the same columns and data types. Otherwise, the data table will be dropped.
#'
#' You can speed operations by putting the largest vector first in `c(x, ...)`.
#'
#' @param x A `GRaster` or a `GVector`.
#'
#' @param ... One or more `GRaster`s, one or more `GVector`s, a list of `GRaster`s, or a list of `GVector`s. You can use a mix of lists and individual rasters or vectors.
#'
#' @return A `GRaster`.
#' 
#' @seealso [add<-], [terra::c()], \code{\link[terra]{add<-}}
#'
#' @example man/examples/ex_c_colbind.r
#'
#' @aliases c
#' @rdname c
#' @exportMethod c
setMethod(
	f = "c",
	signature = "GRaster",
	definition = function(x, ...) {

	.locationRestore(x)
	dots <- list(...)

	# unlist any lists
	if (length(dots) > 0L) dots <- omnibus::unlistRecursive(dots)

	### concatenate
	out <- x
	z <- zext(out)
	dims <- dim(out)

	if (length(dots) >= 1L) {

		for (i in seq_along(dots)) {

			if (!inherits(dots[[i]], "GRaster")) stop("Can only combine GRasters with GRasters.")
			
			compareGeom(out, dots[[i]])
			mmx <- minmax(out)
			mmdots <- minmax(dots[[i]])

			mn <- c(.minVal(out), .minVal(dots[[i]]))
			mx <- c(.maxVal(out), .maxVal(dots[[i]]))
			
			out <- GRaster(
				location = .location(out),
				mapset = .mapset(out),
				workDir = faster("workDir"),
				crs = crs(out),
				projection = .projection(out),
				topology = topology(out),
				sources = c(sources(out), sources(dots[[i]])),
				names = c(names(out), names(dots[[i]])),
				extent = as.vector(ext(out)),
				zextent = z,
				datatypeGRASS = c(out@datatypeGRASS, dots[[i]]@datatypeGRASS),
				dimensions = dims,
				nLayers = nlyr(out) + nlyr(dots[[i]]),
				resolution = res(out),
				minVal = mn,
				maxVal = mx,
				activeCat = c(out@activeCat, dots[[i]]@activeCat),
				levels = c(out@levels, dots[[i]]@levels)
			)
		
		} # next GRaster to combine
		
	} # next item
	out <- .makeUniqueNames(out)		
	out
	
	} # EOF
)

#' @aliases c
#' @rdname c
#' @exportMethod c
setMethod(
	f = "c",
	signature = "GVector",
	definition = function(x, ...) {

	.locationRestore(x)
	dots <- list(...)

	if (length(dots) == 0L) return(x)

	# unlist any lists
	dots <- omnibus::unlistRecursive(dots)

	# comparable?
	for (i in seq_along(dots)) compareGeom(x, dots[[i]], geometry = TRUE, topo = TRUE)

	### recat everything
	# v.patch does not increment cat numbers, so different geometries will have the same cat.
	# This step increases cat numbers in each subsequent vector so conflicts do not happen.

	src <- sources(x)
	srcs <- sapply(dots, sources)

	cats <- .vCats(src, db = FALSE)
	topCat <- max(cats)
	
	for (i in seq_along(dots)) {

		srcs[i] <- .vIncrementCats(srcs[i], add = topCat)
		cats <- .vCats(srcs[i], db = FALSE)
		topCat <- max(cats)
	
	}

	### combine vectors
	# seems like we can combine at least 11 vectors at a time, but not a lot at a time
	srcsAtATime <- 10L # number of sources to combine at a time (plus the running `x` source)

	nSrcs <- length(srcs)
	sets <- ceiling(nSrcs / srcsAtATime)
	
	for (set in seq_len(sets)) {

		index <- (1L + srcsAtATime * (set - 1L)) : min(nSrcs, set * srcsAtATime)
		srcIn <- srcs[index]
		input <- paste(srcIn, collapse = ",")
		input <- paste0(src, ",", input)
	
		src <- .makeSourceName("v_patch", "vector")

		rgrass::execGRASS(
			cmd = "v.patch",
			input = input,
			output = src,
			flags = c(.quiet(), "overwrite")
		)
		
	}

	# # increment category numbers
	# input <- rep(NA_character_, length(dots) + 1L)
	# input[1L] <- sources(x) # collects names of all vectors

	# cats <- list()
	# for (i in seq_along(dots)) {
		# cats[[i]] <- .vCats(dots[[i]])
	# }

	# catsSoFar <- sort(unique(.vCats(x)))
	# for (i in seq_along(dots)) {
		# if (any(cats[[i]] %in% catsSoFar)) {

			# maxCat <- max(catsSoFar)
			# input[i + 1L] <- .vIncrementCats(dots[[i]], add = maxCat, return = "sources")
			# dotCats <- .vCats(input[i + 1])
			# dotCats <- sort(unique(dotCats))
			# catsSoFar <- c(catsSoFar, dotCats)

		# } else {
			# input[i + 1L] <- sources(dots[[i]])
		# }
	# }

	# # sources of inputs
	# input <- paste(input, collapse = ",")

	# src <- .makeSourceName("v_patch", "vector")
	# rgrass::execGRASS(
		# cmd = "v.patch",
		# input = input,
		# output = src,
  		# flags = c(.quiet(), "overwrite") ### ??? "e"???
	# )
	
	table <- list(x@table)
	tables <- lapply(dots, as.data.table)
	tables <- c(table, tables)
	table <- do.call(rbind, tables)

	# table <- as.data.table(x)
	# if (!is.null(table) && nrow(table) > 0L) {
	
		# xNames <- names(table)
		# xClasses <- sapply(table, class)
		# xNcol <- ncol(table)

		# valid <- TRUE
		# i <- 1L
		# while (valid & i <= length(dots)) {

			# dotTable <- as.data.table(dots[[i]])

			# if (nrow(dotTable) == 0L) {
				# valid <- FALSE
			# } else {
				
				# if (ncol(dotTable) != xNcol) {
					# valid <- FALSE
				# } else {
					
					# dotNames <- names(dotTable)
					# dotClasses <- sapply(dotTable, class)

					# if (any(dotNames != xNames) | any(dotClasses != xClasses)) {
						# valid <- FALSE
					# } else {
						# table <- rbind(table, dotTable)
					# }

				# }
			# }

			# i <- i + 1L

		# } # while valid

		# if (!valid) table <- data.table::data.table(NULL)

	# }
	
	.makeGVector(src, table = table)
	
	} # EOF
)

