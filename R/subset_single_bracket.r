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
#' @param drop Logical: If `FALSE` (default), the appropriate subset of the `GVector`s data table will be included in the subset. If `TRUE`, the table will be dropped.
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
	function(x, i, j, drop = FALSE) {

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

			gtype <- geomtype(x, grass = TRUE)

			cats <- .vCats(x, db = FALSE)
			index <- omnibus::renumSeq(cats)
			if (reverseRowSelect) {
				select <- which(omnibus::notIn(index, i))
			} else {
				select <- which(index %in% i)
			}
			cats <- cats[select]
			cats <- unique(cats)
			cats <- seqToSQL(cats)
			cats <- as.character(cats)

			src <- .makeSourceName("v_extract", "vector")
			rgrass::execGRASS(
				cmd = 'v.extract',
				input = sources(x),
				output = src,
				cats = cats,
				new = -1,
				# type = gtype,
				flags = c(.quiet(), 'overwrite')
			)


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

			# where <- paste0("cat IN (", paste(cats, collapse = ","), ")")
			# where <- paste0("cat = ", paste(cats, collapse = ","))
			
			# where <- "id = 'a'"
			# where <- "id IS 'a'"
			# where <- "id IS a"
			# where <- "id = a"
			# where <- 'id = "a"'
			# where <- 'id IS "a"'

			# rgrass::execGRASS('v.db.renamecolumn', map = sources(x), column = 'cat_,cat')
			# .vAsDataTable(x)

			# rgrass::parseGRASS("v.extract")
			# # rgrass::stringexecGRASS("v.extract input=kansas output=test2 type=area layer='-1' cats='77'")

			# # rgrass::stringexecGRASS("v.extract input=kansas output=test type=area layer='-1' where='cat = 77'")
			# .rm("test")
			# # rgrass::stringexecGRASS(paste0("v.extract input=kansas output=test type=area where='cat = 1'")) # works if db intact and "cat" column exists
			# rgrass::stringexecGRASS(paste0("v.extract input=", sources(ks), " output=test type=area where='cat IN (1)'")) # works if db intact and "cat" column exists AND needs to have topology corrected!
			# .makeGVector("test")
			
			# cats <- paste(cats, collapse = ",")
			# src <- .makeSourceName("v_extract", "vector")
			# quiet <- if (is.null(.quiet())) { NULL } else { "-quiet" }
			# # string <- paste0("v.extract -overwrite -t ", quiet, " input=", sources(x), " output=", src, " type=", gtype, " new=-1 where='cat IN (", cats, ")'") # works!
			# string <- paste0("v.extract -overwrite -t ", quiet, " input=", sources(x), " output=", src, " type=", gtype, " new=-1 cats='", cats, "'") # works!
			# rgrass::stringexecGRASS(string) # works as above
			# .makeGVector(src)

			# src <- .makeSourceName("v_extract", "vector")
			# rgrass::execGRASS(
			# 	cmd = "v.extract",
			# 	input = sources(x),
			# 	# where <- "SUB_NAME = 'Nam Loi'", # works
			# 	# where <- "cat = 481", # works
			# 	# where = where,
			# 	output = src,
			# 	new = -1,
			# 	type = gtype,
			# 	cats = cats,
			# 	# file = 'C:/!scratch/cats.txt',
			# 	layer = "-1",
			# 	# layer = "db_in_ogr_table_m6u2SVUBnsE2",
			# 	flags = c(.quiet(), "overwrite", "t")
			# 	# flags = c(.quiet(), "overwrite", "r")
			# 	# flags = c(.quiet(), "overwrite")
			# )

			# .makeGVector(src)

			### select data table rows
			if (nrow(x) == 0L | drop) {
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

			# gtype <- geomtype(x, grass = TRUE)
			# src <- .vRecat(src, gtype = gtype)
			out <- .makeGVector(src, table = table)

		} # keep some rows (vs discarding all)

	} # if selecting some rows
	out

	} # EOF
)
