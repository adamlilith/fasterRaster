#' Replace NAs in a data.table column with another value
#'
#' @description This function replaces `NA`s in one or more `data.table` columns with a user-defined value.
#'
#' @param x A `data.table` or `data.frame`.
#'
#' @param cols `NULL`, character, numeric, integer, or logical vector: Indicates columns for which to replace `NA`s. If `NULL`, then all columns will have `NA`s replaced. If a character, this is the column name(s). If numeric or integer, this is the columns' indices. If logical, columns with `TRUE` have `NA`s replaced. If a logical vector has fewer than the total number of columns, it will be recycled.
#'
#' @param replace A value of any atomic class (numeric, integer, character, Date, etc.): Value to to replace `NA`s.
#'
#' @returns A `data.table` or `data.frame`.
#'
#' @example man/examples/ex_data_table.r
#'
#' @aliases replaceNAs
#' @rdname replaceNAs
#' @exportMethods replaceNAs
methods::setMethod(
	f = "replaceNAs",
	signature = c(x = "data.frame"),
	function(x, replace, cols = NULL) {
	
	if (length(replace) != 1L) stop("Argument ", sQuote("replace"), " must be a single value.")

	if (is.null(cols)) cols <- seq_len(ncol(x))

	if (is.logical(cols)) {
		if (length(cols) < ncol(x)) cols <- rep(cols, length.out = ncol(x))
		cols <- which(cols)
	}

	for (j in cols) {
	
		if (anyNA(x[ , j, drop = TRUE])) {
			x[is.na(x[ , j, drop = TRUE]), j] <- replace
		}
	
	}
	x

	} # EOF
)

#' @aliases replaceNAs
#' @rdname replaceNAs
#' @exportMethods replaceNAs
methods::setMethod(
    f = "replaceNAs",
    signature = c(x = "data.table"),
    function(x, replace, cols = NULL) {

		if (length(replace) != 1L) stop("Argument ", sQuote("replace"), " must be a single value.")

		if (is.null(cols)) cols <- seq_len(ncol(x))
        
		if (is.logical(cols)) {
            if (length(cols) < ncol(x)) cols <- rep(cols, length.out = ncol(x))
            cols <- which(cols)
        }

		# replace!
		if ((is.numeric(replace) | is.integer(replace)) && all(sapply(x, "class") == "numerfic")) {
		
   			data.table::setnafill(x, type = "const", fill = replace, cols = cols)
		
		} else {

			for (j in cols) {

				data.table::set(x, which(is.na(x[[j]])), j, replace)

			}
		}
		x

    } # EOF
)
