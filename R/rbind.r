#' Combine one or more GVectors
#'
#' @description `rbind()` combines two or more `GVector`s of the same type (points, lines, or polygons) and same coordinate reference system. You can speed operations by putting vector that is largest in memory first in `rbind(...)`. If the `GVector`s have data tables, these will also be combined using `rbind()` if their columns and types possible.
#'
#' @param ... One or more `GVector`s.
#'
#' @returns A `GVector`.
#'
#' @example man/examples/ex_cbind_rbind.r
#'
#' @seealso [rbind()], [cbind()], [c()]
#'
#' @aliases rbind
#' @rdname rbind
#' @export rbind
rbind.GVector <- function(...) {

	dots <- list(...)
	.locationRestore(dots[[1L]])

	# unlist any lists
	dots <- omnibus::unlistRecursive(dots)

	# comparable?
	nDots <- length(dots)
	if (nDots == 1L) {
		return(dots[[1L]])
	} else if (nDots > 1L) {
		for (i in 2L:nDots) compareGeom(dots[[1L]], dots[[i]], geometry = TRUE, topo = TRUE)
	}

	### recat everything
	# v.patch does not increment cat numbers, so different geometries will have the same cat.
	# This step increases cat numbers in each subsequent vector so conflicts do not happen.

	x <- dots[[1L]]
	src <- sources(x)
	srcs <- sapply(dots, sources)

	cats <- .vCats(src, db = FALSE)
	topCat <- max(cats)
	
	for (i in 2L:nDots) {

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

	tables <- lapply(dots, as.data.table)
	table <- tryCatch(
		do.call(rbind, tables),
		error = function(cond) FALSE
	)

	if (is.logical(table)) {

		warning("Data tables cannot be combined so will be dropped from the output vector.")
		table <- NULL

	}

	.makeGVector(src, table = table)
	
} # EOF

rbind <- function(...) UseMethod("rbind")
