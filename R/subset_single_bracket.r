#' Subset geometries of a GVector
#'
#' @description The `[` operator returns a subset or remove specific geometries of a `GVector`. You can get the number of geometries using [ngeom()]. Note that you cannot use this function to change the "order" in which geometries or their associated records in a data table appear. For example, `vector[1:3]` and `vector[3:1]` will yield the exact same results.
#'
#' Note that subsetting can take a very long time if `i` has many values or the vector is very large.
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
	nGeoms <- ngeom(x)

	if ((missing(i) || all(i == 1L:nGeoms)) & missing(j)) {
		out <- x
	} else if ((missing(i) & !missing(j)) || (all(i == 1L:nGeoms) & !missing(j))) {
		out <- x[[i = j]]
	} else if (!missing(i) & missing(j)) {

		if (is.logical(i)) {
			if (length(i) < nGeoms) i <- rep(i, length.out = nGeoms)
			i <- which(i)
		}

		if (any(i > 0L) & any(i < 0L)) stop("Cannot mix positive and negative indices.")

		# negative indices
		if (all(i < 0L)) {

			reverseRowSelect <- TRUE
			iRev <- i
			iSeq <- seq_len(nGeoms) # all possible i's
			i <-  iSeq[i] # retained i's
			removeAll <- length(i) == 0L

		} else {
			reverseRowSelect <- removeAll <- FALSE
		}
		
		if (any(i > nGeoms) | any(i == 0L)) stop("Index out of bounds.")
		i <- sort(i)

		if (removeAll) {
			out <- NULL # removed all
		} else if (all(1L:nGeoms %in% i) & missing(j)) {
			out <- x
		} else {

			# will be deleting geometries from this copy
			src <- .copyGSpatial(x)

			# get cats to delete
			cats <- seq_len(nGeoms) # copying a vector re-orders cats from 1 to number of geometries
			cats <- cats[omnibus::notIn(cats, i)] # cats to delete

			# delete geometries in subsets bc removing large numbers of geometries at a time is inefficient
			done <- FALSE
			n <- 200000L # maximum number of cats to delete at a time (seqToSQL() usually stops far short)
			# n <- 10L # maximum number of cats to delete at a time (seqToSQL() usually stops far short)
			startFrom <- 1L
			numRemoved <- 0L
			stopAt <- min(length(cats), startFrom + n - 1L)
			while (!done) {
				
				thisCats <- cats[startFrom:stopAt]
				thisCats <- seqToSQL(thisCats)

				args <- list(
					cmd = "v.edit",
					map = src,
					# type = gtype, # breaks
					tool = "delete",
					cats = thisCats,
					flags = c(.quiet(), "overwrite")
				)
				do.call(rgrass::execGRASS, args = args)

				# rgrass::execGRASS("v.build", map = src, option = "build", flags = c(.quiet(), "overwrite"))

				numRemoved <- numRemoved + attr(thisCats, "n")
				if (numRemoved == length(cats)) {
					done <- TRUE
				} else {
					startFrom <- startFrom + attr(thisCats, "lastIndex") # what was the index of the last used cat?
					stopAt <- min(length(cats), startFrom + n)
				}

			} # next removal

		} # not removing all and not removing nothing

		### select data table rows and columns
		if (nrow(x) == 0L) {
			table <- NULL
		} else if (!removeAll) {

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
		
		if (removeAll) {
			out <- NULL
		} else {
			out <- .makeGVector(src, table = table)
		}

	}
	out

	} # EOF
)

# # # #' @param src Character: [sources()] name of the vector.
# # # #' @param i,j Integer, numeric, character, or logical indices
# # # #' @param gtype Character: The vector's [geomtype()] in **GRASS** format.
# # # #' @param cats Integer: "Cats" of the vector (category numbers).
# # # #' @param reverseRowSelect Logical.
# # # #' @param dropTable Logical.
# # # #' @param table The vector's `data.table`.
# # # #' @param n Integer: Number of geometries to select at a time.
# # # #' @param verbose Logical.
# # # #'
# # # #' @returns A `GVector`.
# # # #'
# # # #' @noRd
# # # .subset_single_bracket <- function(src, i, j, gtype, cats, reverseRowSelect = FALSE, dropTable = FALSE, table = NULL, n = faster("n"), verbose = FALSE) {

# # # 	srcIn <- src

# # # 	index <- omnibus::renumSeq(cats)
# # # 	if (reverseRowSelect) {
# # # 		select <- which(omnibus::notIn(index, i))
# # # 	} else {
# # # 		select <- which(index %in% i)
# # # 	}
# # # 	nSelect <- length(select)
# # # 	cats <- cats[select]
	
# # # 	# select all at once (OK for small number of selected geometries)
# # # 	if (nSelect < n) {

# # # 		cats <- seqToSQL(cats)
# # # 		cats <- as.character(cats)

# # # 		src <- .makeSourceName("subset_single_bracket_v_extract", "vector")
# # # 		rgrass::execGRASS(
# # # 			cmd = 'v.extract',
# # # 			input = srcIn,
# # # 			output = src,
# # # 			cats = cats,
# # # 			new = -1,
# # # 			type = gtype,
# # # 			flags = c(.quiet(), 'overwrite')
# # # 		)

# # # 	# select in sets
# # # 	# NB this parts obviates an issue where if the `cat` SQL statement is too long, the requested number of geometries is not returned
# # # 	} else {

# # # 		src <- .copyGSpatial(x)
# # # 		while (!done) {
		


		
# # # 		}



# # # 		worked <- FALSE
# # # 		while (!worked & n >= 1L) {

# # # 			sets <- ceiling(nSelect / n)
# # # 			starts <- seq(1L, nSelect, by = n)
# # # 			stops <- pmin(starts + n - 1L, nSelect)

# # # 			verbose <- verbose | faster("verbose")
# # # 			if (verbose) {
# # # 				omnibus::say("Selecting geometries...")
# # # 				width <- min(getOption("width"), 80L)
# # # 				pb <- utils::txtProgressBar(min = 0, max = sets, style = 3, initial = 0, width = 80)
# # # 			}
			
# # # 			ok <- TRUE

# # # 			srcs <- .makeSourceName("v_extract", "vector", n = sets)
# # # 			set <- 1L
# # # 			while (set <= sets & ok) {

# # # 				if (verbose) utils::setTxtProgressBar(pb, set)

# # # 				thisCats <- cats[starts[set]:stops[set]]
# # # 				thisCats <- seqToSQL(thisCats)

# # # 				if (attr(thisCats, "trim")) {
# # # 					ok <- FALSE
# # # 				} else {

# # # 					thisCats <- as.character(thisCats) # remove attributes

# # # 					srcs[set] <- .makeSourceName("v_extract", "vector")
# # # 					rgrass::execGRASS(
# # # 						cmd = 'v.extract',
# # # 						input = sources(x),
# # # 						output = srcs[set],
# # # 						cats = thisCats,
# # # 						# new = -1,
# # # 						type = gtype,
# # # 						flags = c(.quiet(), 'overwrite')
# # # 					)

# # # 					info <- .vectInfo(srcs[set])
# # # 					nSelected <- stops[set] - starts[set] + 1L
# # # 					if (info$nGeometries != nSelected) ok <- FALSE
# # # 					set <- set + 1L

# # # 				}

# # # 			}

# # # 			if (verbose) close(pb)
# # # 			if (ok) {
# # # 				worked <- TRUE
# # # 			} else {
# # # 				n <- round(n / 2)
# # # 				if (verbose) omnibus::say("Selection failure. Retrying with smaller value for argument `n`.")
# # # 			}

# # # 		}

# # # 		### combine vectors
# # # 		# seems like we can combine at least 11 vectors at a time, but not a lot more
# # # 		srcsAtATime <- 10L # number of sources to combine at a time (not including the one being added to)

# # # 		nSrcs <- length(srcs)
# # # 		sets <- ceiling(nSrcs / srcsAtATime)
		
# # # 		if (verbose) {
# # # 			omnibus::say("Combining sub-vectors...")
# # # 			pb <- utils::txtProgressBar(min = 0, max = sets, style = 3, initial = 0, width = 80)
# # # 		}

# # # 		for (set in seq_len(sets)) {

# # # 			if (verbose) utils::setTxtProgressBar(pb, set)

# # # 			index <- (1L + srcsAtATime * (set - 1L)) : min(nSrcs, set * srcsAtATime)
# # # 			srcIn <- srcs[index]
# # # 			input <- paste(srcIn, collapse = ",")
# # # 			if (set > 1L) input <- paste0(src, ",", input)
# # # 			src <- .makeSourceName("v_patch", "vector")

# # # 			rgrass::execGRASS(
# # # 				cmd = "v.patch",
# # # 				input = input,
# # # 				output = src,
# # # 				flags = c(.quiet(), "overwrite")
# # # 			)
		
# # # 		}
# # # 		if (verbose) close(pb)

# # # 	} # if selecting in sets

# # # 	### select data table rows
# # # 	if (nrow(table) == 0L | dropTable) {
# # # 		table <- NULL
# # # 	} else {

# # # 		# select columns
# # # 		if (missing(j)) {
# # # 			removeAllCols <- FALSE
# # # 		} else {

# # # 			nc <- ncol(table)
# # # 			if (is.character(j)) {
# # # 				j <- match(j, names(table))
# # # 			} else if (is.logical(j)) {
# # # 				if (length(j) < nc) j <- rep(j, length.out = nc)
# # # 				j <- which(j)
# # # 			}
			
# # # 			if (all(j < 0L)) {
# # # 				reverseColSelect <- TRUE
# # # 				j <- j * -1L
# # # 				removeAllCols <-  (all(sort(j) == seq_len(ncol(x))))
# # # 			} else {
# # # 				reverseColSelect <- removeAllCols <- FALSE
# # # 			}

# # # 			if (any(j > nc)) stop("Index out of bounds.")

# # # 			if (removeAllCols) {
# # # 				table <- data.table::data.table(NULL)
# # # 			} else {
			
# # # 				# ..j <- NULL

# # # 				if (reverseColSelect) {
# # # 					j <- setdiff(names(x), names(x)[j])
# # # 				} else {
# # # 					j <- names(x)[j]
# # # 				}
				
# # # 				table <- table[ , ..j, with = FALSE]

# # # 			}
		
# # # 		}

# # # 		# select rows
# # # 		if (!removeAllCols) {
# # # 			if (reverseRowSelect) {
# # # 				table <- table[iRev]
# # # 			} else {
# # # 				table <- table[i]
# # # 			}
# # # 		}

# # # 	} # vector has table
# # # 	out <- .makeGVector(src, table = table)
# # # 	out

# # # } # EOF
