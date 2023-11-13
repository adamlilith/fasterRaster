#' Subset geometries of a GVector
#'
#' @description The `[` operator returns a subset or remove specific geometries of a `GVector`. You can get the number of geometries using [ngeom()].
#'
#' @param x A `GVector`.
#'
#' @param i Numeric integer, integer, or logical vector: Indicates which geometry(ies) to obtain. Negative numeric or integer values will remove the given geometries from the output. If a logical vector is supplied and it is not the same length as the number of geometries, it will be recycled.
#'
#' @param j Numeric integer, integer, logical, or character: Indices or name(s) of the column(s) to obtain. You can see column names using [names()]. Negative numeric or integer values will remove the given columns from the output. If a logical vector is supplied and it is not the same length as the number of columns, it will be recycled.
#'
#' @returns A `GVector`.
#'
#' @example man/examples/ex_GRaster_GVector_subset_assign.r
#'
#' @name [
#' @aliases [,GVector,ANY,ANY-method
#' @docType methods
#' @rdname subset_single_bracket
#' @exportMethod [
methods::setMethod(
	"[",
	signature = c(x = "GVector", i = "ANY", j = "ANY"),
	function(x, i, j) {

	.restore(x)

	nGeoms <- ngeom(x)

	if (missing(i)) {
		out <- x[[j]]
	} else {

		if (is.logical(i)) {
			if (length(i) < nGeoms) i <- rep(i, length.out = nGeoms)
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
		
		if (any(i > nGeoms)) stop("Index out of bounds.")

		if (removeAll) {
			out <- NULL # removed all
		} else {

			# **keep** rows
			cats <- .vCats(x)
			ucats <- unique(cats)
			keepCats <- ucats[i]
			keepCats <- sort(keepCats)
			keepCats <- seqToSQL(keepCats)

			src <- .makeSourceName("v_extract", "vector")

			args <- list(
				cmd = "v.extract",
				input = sources(x),
				cats = keepCats,
				output = src,
				new = -1,
				flags = c(.quiet(), "overwrite")
			)

			if (reverseRowSelect) args$flags <- c(args$flags, "r")

			do.call(rgrass::execGRASS, args = args)
			
			# vCats <- .vCats(src)
			# vCats <- omnibus::renumSeq(vCats)
			# .vRecat(src, vCats, removeTable = TRUE)
			
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
					
						# ..j <- NULL

						if (reverseColSelect) {
							j <- setdiff(names(x), names(x)[j])
						} else {
							j <- names(x)[j]
						}
						
						table <- table[ , ..j, with = FALSE]

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
