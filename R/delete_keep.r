#' Delete/retain rows in a data.table
#'
#' @description As of September of 2023, the **data.table** package does not have a function for removing/retaining rows by index. These functions do this job and are adapted from an \url{issue}{https://github.com/Rdatatable/data.table/issues/635} to that effect raised on the **data.table** *GitHub* page.
#'
#' @param DT A `data.table`.
#' @param removes,keeps Numeric, integer, or logical: Indices to rows to *remove* or *keep*.
#'
#' @returns A `data.table`.
#'
#' @example man/examples/ex_data_table.r
#'
#' @noRd
.delete <- function(DT, removes) {

	if (nrow(DT) > 0L) {

		if (is.logical(removes)) {
			if (length(removes) != nrow(DT)) {
				warning("Length of rows to delete is shorter than the number or rows. Values will be recycled.")

				removes <- rep(removes, length.out = nrow(DT))
			}
			removes <- which(removes)
		}

		# pls note 'removes' vs. 'keeps'
		# select row indexes to keep
		keeps <- setdiff(DT[, .I], removes)

		cols <- names(DT)
		
		# this is the subsetted table
		DT.subset <- data.table(DT[[1]][keeps]) 
		
		setnames(DT.subset, cols[1])

		if (length(cols) > 1L) {
			for (col in cols[2:length(cols)]) {
				DT.subset[, (col) := DT[[col]][keeps]]
				DT[ , (col) := NULL] # delete
			}
		}
		DT.subset
	
	} else if (nrow(DT) == 0L) {
		DT
	}

}

#' @noRd
.keep <- function(DT, keeps) {

	if (nrow(DT) > 0L) {

		if (is.logical(keeps)) {
			if (length(keeps) != nrow(DT)) {
				warning("Length of rows to delete is shorter than the number or rows. Values will be recycled.")

				keeps <- rep(keeps, length.out = nrow(DT))
			}

		}

		cols <- names(DT)

		# this is the subsetted table
		DT.subset <- data.table(DT[[1]][keeps])

		setnames(DT.subset, cols[1])

		if (length(cols) > 1L) {
			for (col in cols[2:length(cols)]) {
				DT.subset[, (col) := DT[[col]][keeps]]
				DT[, (col) := NULL] # delete
			}
		}
		DT.subset

	} else if (nrow(DT) == 0L) {
		DT
	}

}
