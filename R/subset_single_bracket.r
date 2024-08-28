#' Subset geometries of a GVector
#'
#' @description The `[` operator returns a subset or remove specific geometries of a `GVector`. You can get the number of geometries using [ngeom()]. Note that you cannot use this function to change the "order" in which geometries or their associated records in a data table appear. For example, `vector[1:3]` and `vector[3:1]` will yield the exact same results.
#'
#' Note that subsetting can take a very long time if you are retaining only a small number of geometries from a vector with many geometries. The routine selects geometries by removing those that are not in `i`. So if you can write code to remove fewer geometries (i.e., an "inverse" selection), it may go faster.
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

	if (missing(i) & missing(j)) {
		out <- x
	} else if (missing(i) & !missing(j)) {
		out <- x[[i = j]]
	} else if (all(1L:nGeoms %in% i) & !missing(j)) {
		out <- x[[i = j]]
	} else if (all(1L:nGeoms %in% i) & missing(j)) {
		out <- x
	} else if (!missing(i)) {

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

			# # # # will be deleting geometries from this copy
			# # # src <- .copyGSpatial(x)
			# # # cats <- .vCats(src)
			# # # cats <- sort(unique(cats))
			# # # iNot <- seq_len(nGeoms)
			# # # iNot <- iNot[!(iNot %in% i)]
			# # # cats <- cats[iNot] # will be removed

			# # # # delete geometries in subsets bc removing large numbers of geometries at a time is inefficient
			# # # done <- FALSE
			# # # n <- 200000L # maximum number of cats to delete at a time (seqToSQL() usually stops far short)
			# # # # n <- 10L # maximum number of cats to delete at a time (seqToSQL() usually stops far short)
			# # # startFrom <- 1L
			# # # numRemoved <- 0L
			# # # stopAt <- min(length(cats), startFrom + n - 1L)
			# # # while (!done) {
				
			# # 	# thisCats <- cats[startFrom:stopAt]
			# # 	# thisCats <- seqToSQL(thisCats)

			# # # # <<< ORIGINAL -- selecting by deleting unwanted... VERY slow for large vectors >>>
			# # 	# rgrass::execGRASS(
			# # 		# cmd = "v.edit",
			# # 		# map = src,
			# # 		# # type = gtype, # breaks
			# # 		# tool = "delete",
			# # 		# cats = thisCats,
			# # 		# flags = c(.quiet(), "overwrite")
			# # 	# )
			# # # # <<< end ORIGINAL >>>

			# # 	# # args <- list(
			# # 	# # 	cmd = "v.edit",
			# # 	# # 	map = src,
			# # 	# # 	# type = gtype, # breaks
			# # 	# # 	tool = "delete",
			# # 	# # 	cats = thisCats,
			# # 	# # 	flags = c(.quiet(), "overwrite")
			# # 	# # )
			# # 	# # # if (geomtype(x) == "polygons") args$type <- "area"
			# # 	# # # if (geomtype(x) == "points") args$flags <- c(args$flags, "b")
			# # 	# # do.call(rgrass::execGRASS, args = args)

			# # 	# # rgrass::execGRASS("v.build", map = src, option = "build", flags = c(.quiet(), "overwrite"))

			# # 	# numRemoved <- numRemoved + attr(thisCats, "lastIndex")
			# # 	# if (numRemoved == length(cats)) {
			# # 		# done <- TRUE
			# # 	# } else {
			# # 		# startFrom <- startFrom + attr(thisCats, "lastIndex") # what was the index of the last used cat?
			# # 		# stopAt <- min(length(cats), startFrom + n)
			# # 	# }

			# # # } # next removal

			# # # # check to see if all cats have been removed
			# # # # a hack... for some reason, seqToSQL may not list all cats
			# # # newCats <- .vCats(src)
			# # # badCats <- cats[cats %in% newCats]
			# # # if (length(badCats) > 0L) {
			
			# # 	# badCats <- cats[cats %in% newCats]
			# # 	# thisCats <- seqToSQL(badCats)

			# # 	# rgrass::execGRASS(
			# # 		# cmd = "v.edit",
			# # 		# map = src,
			# # 		# # type = gtype, # breaks
			# # 		# tool = "delete",
			# # 		# cats = thisCats,
			# # 		# flags = c(.quiet(), "overwrite")
			# # 	# )

			# # 	# newCats <- newCats[!(newCats %in% badCats)]
			
			# # # }

			# ### WORKS!!!!! Keeping copy.
			# src <- .makeSourceName("subset_single_bracket_v_extract", "vector")
			# cats <- .vCats(sources(x))
			# cats <- unique(cats)
			# cats <- cats[cats %in% i] # selects same as GVector but wrong geometry
			# sqlCats <- seqToSQL(cats)
			# sqlCats <- as.character(sqlCats)
			# gtype <- geomtype(x, grass = TRUE)
			# ### ^^^ WORKS!!!!! Keeping copy.
			
			gtype <- geomtype(x, grass = TRUE)

			src <- .makeSourceName("subset_single_bracket_v_extract", "vector")
			cats <- .vCats(sources(x))
			cats <- unique(cats)
			keepCats <- cats[cats %in% i] # selects same as GVector but wrong geometry

			args <- list(
				cmd = "v.extract",
				input = sources(x),
				output = src,
				type = gtype,
				flags = c(.quiet(), "overwrite")
			)

			if (gtype == "point") {

				# selecting geometries using database method
				qid <- rep(0L, nGeoms)
				qid[keepCats] <- 1L

				db <- data.table::data.table(cat = cats, qid = qid)
				.vAttachDatabase(x, db)

				args$where <- "qid = 1" # will not work with inequalities

			} else if (gtype %in% c("area", "line")) {
			
				sqlCats <- seqToSQL(keepCats)
				sqlCats <- as.character(sqlCats)
				args$cats <- sqlCats
			
			}

			do.call(rgrass::execGRASS, args = args)
			src <- .vRecat(src, gtype = gtype, cats = keepCats)

# 				srcs <- character()
# 				trim <- TRUE
# 				j <- 1L
# 				while (trim) {
				
# 					if (j > 1L) {
# 						thisKeepCats <- seqToSQL(keepCats[(stoppedAt + 1):length(i)])
# 						stoppedAt <- attr(thisKeepCats, "lastIndex")
# 						trim <- attr(thisKeepCats, "trim")
# 					}

# 					thisKeepCats <- as.character(thisKeepCats)

# 					srcs[j] <- .makeSourceName("single_bracket_select_v_extract", "vector")
# 					rgrass::execGRASS(
# 						cmd = "v.extract",
# 						input = sources(x),
# 						output = srcs[j],
# 						cats = thisKeepCats,
# 						type = gtype,
# 						flags = c(.quiet(), "overwrite")
# 					)

# 					j <- j + 1L
				
# 				}
			
			
			# }

			# renumber cats so they start from 1 and are sequential
			
		
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
			# out <- .makeGVector(src, table = table, cats = newCats)
			out <- .makeGVector(src, table = table)
		}

	}
	out

	} # EOF
)

#' @name [
#' @aliases [,GRaster,ANY,ANY-method
#' @docType methods
#' @rdname subset_single_bracket
#' @exportMethod [
methods::setMethod(
	"[",
	signature = c(x = "GRaster", i = "GRaster", j = "ANY"),
	function(x, i, j) {

	.locationRestore(x)
	.region(x)

	if (missing(j)) j <- NULL

	nLayers <- nlyr(x)
	nLayersI <- nlyr(i)

	if (nLayersI != nLayers) {

		if (nlyr(i) != 1L) warning("The number of layers in the subset raster (inside the `[]`) does not have\n  the same number of layers as the raster to be subset. The layers inside\n  the square brackets will be recycled.")

		iIndex <- rep(seq_len(nLayersI), length.out = nLayers)
		i <- i[[iIndex]]

	}

	srcs <- .makeSourceName("subset_single_bracket", "raster", nLayers)
	for (count in seq_len(nLayers)) {
			
		if (!(.minVal(i)[count] %in% c(NA, 0, 1)) & !(.maxVal(i)[count] %in% c(NA, 0, 1))) stop("The GRaster in `i` must 0, 1, or NA values.")

		ex <- paste0(srcs[count], " = if(", sources(i)[count], " == 1, ", sources(x)[count], ", 0)")
		rgrass::execGRASS("r.mapcalc", expression = ex, flags = c(.quiet(), "overwrite"))
	
	}
	.makeGRaster(srcs, names(x))

	} # EOF
)
