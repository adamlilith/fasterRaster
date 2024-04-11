#' Subset geometries of a GVector
#'
#' @description The `[` operator returns a subset or remove specific geometries of a `GVector`. You can get the number of geometries using [ngeom()]. Note that you cannot use this function to change the "order" in which geometries or their associated records in a data table appear. For example, `vector[1:3]` and `vector[3:1]` will yield the exact same results, where the first geometry in `vector` will also be the first geometry in either of the outputs.
#'
#' Note that subsetting can take a very long time if `i` has many values, even for moderately-sized vectors. In these cases, it may help to set `verbose = TRUE` so you have something exciting to watch as it does its work.
#'
#' @param x A `GVector`.
#'
#' @param i Numeric integer, integer, or logical vector: Indicates which geometry(ies) to obtain. Negative numeric or integer values will remove the given geometries from the output. If a logical vector is supplied and it is not the same length as the number of geometries, it will be recycled.
#'
#' @param j Numeric integer, integer, logical, or character: Indices or name(s) of the column(s) to obtain. You can see column names using [names()]. Negative numeric or integer values will remove the given columns from the output. If a logical vector is supplied and it is not the same length as the number of columns, it will be recycled.
#'
#' @param verbose Logical: If `TRUE` (or if `faster("verbose")` is `TRUE`), then display progress.
#'
#' @param dropTable Logical: If `FALSE` (default), the appropriate subset of the `GVector`s data table will be included in the subset. If `TRUE`, the table will be dropped.
#'
#' @param n Integer or numeric (default: 100000): Number of geometries to subset at a time. The subset operation will sometimes fail if `i` has many values. Internally, this arises when a large number of indices is used. When this fails, the function will try again, but with fewer indices used at a time. The starting number is given by `n`. Smaller values take more time, but are also less prone to failure.
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
	function(x, i, j, verbose = FALSE, dropTable = FALSE, n = 100000) {

	.message(msg = "subset_single_bracket", message = "Subsetting can take a long time, even for moderately-sized GVectors.")
	.locationRestore(x)

	if (missing(i)) {
		out <- x[[i = j]]
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
		} else if (all(1L:nGeoms %in% i) & missing(j)) {
			out <- x
		} else {

			gtype <- geomtype(x, grass = TRUE)
			cats <- .vCats(x, db = FALSE)

			success <- FALSE
			src <- sources(x)
			
			if (dropTable) {
				table <- data.table::data.table(NULL)
			} else {

				table <- x@table
				out <- .subset_single_bracket(
					src = src,
					i = i,
					j = j,
					gtype = gtype,
					cats = cats,
					reverseRowSelect = reverseRowSelect,
					dropTable = dropTable,
					table = table,
					n = n,
					verbose = verbose
				)

			}

		}
	}

	if (is.logical(out)) stop("Selection failure.")
	out

	} # EOF
)

#' @param src Character: [sources()] name of the vector.
#' @param i,j Integer, numeric, character, or logical indices
#' @param gtype Character: The vector's [geomtype()] in **GRASS** format.
#' @param cats Integer: "Cats" of the vector (category numbers).
#' @param reverseRowSelect Logical.
#' @param dropTable Logical.
#' @param table The vector's `data.table`.
#' @param n Integer: Number of geometries to select at a time.
#' @param verbose Logical.
#'
#' @returns A `GVector`.
#'
#' @noRd
.subset_single_bracket <- function(src, i, j, gtype, cats, reverseRowSelect = FALSE, dropTable = FALSE, table = NULL, n = faster("n"), verbose = FALSE) {

	srcIn <- src

	index <- omnibus::renumSeq(cats)
	if (reverseRowSelect) {
		select <- which(omnibus::notIn(index, i))
	} else {
		select <- which(index %in% i)
	}
	nSelect <- length(select)
	cats <- cats[select]
	
	# select all at once (OK for small number of selected geometries)
	if (nSelect < n) {

		cats <- seqToSQL(cats)
		cats <- as.character(cats)

		src <- .makeSourceName("subset_single_bracket_v_extract", "vector")
		rgrass::execGRASS(
			cmd = 'v.extract',
			input = srcIn,
			output = src,
			cats = cats,
			new = -1,
			type = gtype,
			flags = c(.quiet(), 'overwrite')
		)

	# select in sets
	# NB this parts obviates an issue where if the `cat` SQL statement is too long, the requested number of geometries is not returned
	} else {
	
		worked <- FALSE
		while (!worked & n >= 1L) {

			sets <- ceiling(nSelect / n)
			starts <- seq(1L, nSelect, by = n)
			stops <- pmin(starts + n - 1L, nSelect)

			verbose <- verbose | faster("verbose")
			if (verbose) {
				omnibus::say("Selecting geometries...")
				width <- min(getOption("width"), 80L)
				pb <- utils::txtProgressBar(min = 0, max = sets, style = 3, initial = 0, width = 80)
			}
			
			ok <- TRUE

			srcs <- .makeSourceName("v_extract", "vector", n = sets)
			set <- 1L
			while (set <= sets & ok) {

				if (verbose) utils::setTxtProgressBar(pb, set)

				thisCats <- cats[starts[set]:stops[set]]
				thisCats <- seqToSQL(thisCats)

				if (attr(thisCats, "trim")) {
					ok <- FALSE
				} else {

					thisCats <- as.character(thisCats) # remove attributes

					srcs[set] <- .makeSourceName("v_extract", "vector")
					rgrass::execGRASS(
						cmd = 'v.extract',
						input = sources(x),
						output = srcs[set],
						cats = thisCats,
						# new = -1,
						type = gtype,
						flags = c(.quiet(), 'overwrite')
					)

					info <- .vectInfo(srcs[set])
					nSelected <- stops[set] - starts[set] + 1L
					if (info$nGeometries != nSelected) ok <- FALSE
					set <- set + 1L

				}

			}

			if (verbose) close(pb)
			if (ok) {
				worked <- TRUE
			} else {
				n <- round(n / 2)
				if (verbose) omnibus::say("Selection failure. Retrying with smaller value for argument `n`.")
			}

		}

		### combine vectors
		# seems like we can combine at least 11 vectors at a time, but not a lot more
		srcsAtATime <- 10L # number of sources to combine at a time (not including the one being added to)

		nSrcs <- length(srcs)
		sets <- ceiling(nSrcs / srcsAtATime)
		
		if (verbose) {
			omnibus::say("Combining sub-vectors...")
			pb <- utils::txtProgressBar(min = 0, max = sets, style = 3, initial = 0, width = 80)
		}

		for (set in seq_len(sets)) {

			if (verbose) utils::setTxtProgressBar(pb, set)

			index <- (1L + srcsAtATime * (set - 1L)) : min(nSrcs, set * srcsAtATime)
			srcIn <- srcs[index]
			input <- paste(srcIn, collapse = ",")
			if (set > 1L) input <- paste0(src, ",", input)
			src <- .makeSourceName("v_patch", "vector")

			rgrass::execGRASS(
				cmd = "v.patch",
				input = input,
				output = src,
				flags = c(.quiet(), "overwrite")
			)
		
		}
		if (verbose) close(pb)

	} # if selecting in sets

	### select data table rows
	if (nrow(table) == 0L | dropTable) {
		table <- NULL
	} else {

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
	out

} # EOF
