#' Remove rows in a data.table
#'
#' @description As of September of 2023, the **data.table** package does not have a function for removing rows by index. This function does this job and is adapted from an \url{issue}{https://github.com/Rdatatable/data.table/issues/635} to that effect raised on the **data.table** *GitHub* page.
#'
#' @param X A `data.table` or `data.frame`.
#' @param drops Numeric, integer, or logical vector: Indices or indicators of rows to remove.
#'
#' If a logical vector is supplied, rows that correspond to `TRUE` will be removed. If the vector is shorter than the number of rows, values of `drops` will be recycled.
#'
#' @returns A `data.table` or `data.frame`.
#'
#' @example man/examples/ex_data_table.r
#'
#' @aliases dropRows
#' @rdname dropRows
#' @exportMethod dropRows
methods::setMethod(
	f = "dropRows",
	signature = c(x = "data.frame"),
	function(x, drops) {
	
	if (nrow(x) > 0L) {

  		drops <- .rowIndices(x, select = drops)
		out <- x[-drops, , drop = FALSE]

	} else {
		out <- x # has no rows
	}
	out
		
	} # EOF
)

#' @aliases dropRows
#' @rdname dropRows
#' @exportMethod dropRows
methods::setMethod(
	f = "dropRows",
	signature = c(x = "data.table"),
	function(x, drops) {

	if (nrow(x) > 0L) {

  		drops <- .rowIndices(x, select = drops)

		# select row indexes to keep
		keeps <- setdiff(x[ , .I], drops)

		cols <- names(x)
		
		# this is the subsetted table
		xSubset <- data.table::data.table(x[[1L]][keeps]) 
  		# x[ , 1L := NULL] # delete
		
		data.table::setnames(xSubset, cols[1L])

		if (length(cols) > 1L) {
			for (col in cols[2L:length(cols)]) {
    			these <- x[[col]][keeps]
				xSubset[ , (col) := these]
				# x[ , (col) := NULL] # delete
			}
		}
		out <- xSubset
	
	} else {
		out <- x # has no rows
	}
	out

	} # EOF
)

#' Get indices of rows to remove
#'
#' @param x A `data.frame` or `data.table`.
#' @param select Numeric, integer, logical, or character.
#'
#' @noRd
 .rowIndices <- function(x, select) {
     if (is.logical(select)) {
         if (length(select) != nrow(x)) {
             select <- rep(select, length.out = nrow(x))
         }
         select <- which(select)
     } else if (is.character(select)) {
         if (inherits(x, "data.table")) stop("Row names can only be used with a data.frame, not with a data.table.")
		 rns <- rownames(x)
         select <- match(select, rns)
     }
     select
 }

