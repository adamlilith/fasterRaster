#' Subset geometries of a GVector
#'
#' @description The `[` operator returns a subset or remove specific geometries of a `GVector`. You can get the number of geometries using [ngeom()]. Note that you cannot use this function to change the "order" in which geometries or their associated records in a data table appear. For example, `vector[1:3]` and `vector[3:1]` will yield the exact same results, where the first geometry in `vector` will also be the first geometry in either of the outputs.
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
#' @seealso [$], \code{\link[fasterRaster]{[[}}
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

	.locationRestore(x)

	if (missing(i)) {
		out <- x[[j]]
	} else {

		nGeoms <- ngeom(x)
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
		
		if (any(i > nGeoms) | any(i == 0L)) stop("Index out of bounds.")
		i <- sort(i)

		if (removeAll) {
			out <- NULL # removed all
		} else {

			# # # create database with frid value = -1 for records we do not want
			# # frid <- .vCats(x, db = FALSE)
			# # if (reverseRowSelect) {
			# # 	frid[i] <- -1L
			# # } else {
			# # 	frid[omnibus::notIn(frid, i)] <- -1L
			# # }	
			# # frid[frid > -1] <- omnibus::renumSeq(frid[frid > -1])

			# # .vAttachDatabase(x, table = frid, replace = TRUE)

			# # srcIn <- sources(x)
			# # src <- .makeSourceName("v_extract", "vector")
			
			# # where <- paste0("frid > -1")
			# # # # # where <- paste0("(frid = 1) or (frid = 11291)")

			cats <- .vCats(x)
			index <- omnibus::renumSeq(cats)
			if (reverseRowSelect) {
				select <- which(omnibus::notIn(index, i))
			} else {
				select <- which(index %in% i)
			}
			cats <- cats[select]
			cats <- seqToSQL(cats)
			cats <- as.character(cats)

			src <- .makeSourceName("v_extract", "vector")
			rgrass::execGRASS(
				cmd = "v.extract",
				input = sources(x),
				# where = where,
				output = src,
				new = -1,
				cats = cats,
				# layer = "-1",
				flags = c(.quiet(), "overwrite", "t")
			)

			### select data table rows
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

			# if (reverseRowSelect) {
			# 	keepCats <- cats[-i]
			# } else {
			# 	keepCats <- cats[i]
			# }

			# keepRows <- frid[frid > -1L]
			# table <- table[keepRows]

			src <- .vRecat(src, gtype = geomtype(x, grass = TRUE))
			out <- .makeGVector(src, table = table)

		} # keep some rows (vs discarding all)

	} # if selecting some rows
	out

	} # EOF
)
