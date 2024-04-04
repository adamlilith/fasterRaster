#' Replace NAs in a data.table or data.frame column, or in a vector
#'
#' @description This function replaces `NA`s in one or more `data.table` columns with a user-defined value.
#'
#' @param x A `data.table` or `data.frame`, or a vector of numeric, integer, logical, or character values.
#'
#' @param replace A value of any atomic class (numeric, integer, character, Date, etc.): Value to to replace `NA`s.
#'
#' @param cols `NULL`, character, numeric, integer, or logical vector: Indicates columns for which to replace `NA`s. If `NULL`, then all columns will have `NA`s replaced. If a character, this is the column name(s). If numeric or integer, this is the columns' indices. If logical, columns with `TRUE` have `NA`s replaced. If a logical vector has fewer than the total number of columns, it will be recycled.
#'
#' @returns A `data.table` or `data.frame`.
#'
#' @example man/examples/ex_data_table.r
#'
#' @aliases replaceNAs
#' @rdname replaceNAs
#' @exportMethod replaceNAs
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
#' @exportMethod replaceNAs
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

#' @aliases replaceNAs
#' @rdname replaceNAs
#' @exportMethod replaceNAs
methods::setMethod(
    f = "replaceNAs",
    signature = c(x = "numeric"),
    function(x, replace) .replaceNAsAtomic(x, replace)
)

#' @aliases replaceNAs
#' @rdname replaceNAs
#' @exportMethod replaceNAs
methods::setMethod(
    f = "replaceNAs",
    signature = c(x = "integer"),
    function(x, replace) .replaceNAsAtomic(x, replace)
)

#' @aliases replaceNAs
#' @rdname replaceNAs
#' @exportMethod replaceNAs
methods::setMethod(
    f = "replaceNAs",
    signature = c(x = "logical"),
    function(x, replace) .replaceNAsAtomic(x, replace)
)

#' @aliases replaceNAs
#' @rdname replaceNAs
#' @exportMethod replaceNAs
methods::setMethod(
    f = "replaceNAs",
    signature = c(x = "character"),
    function(x, replace) .replaceNAsAtomic(x, replace)
)

#' Replace NAs in an atomic vector
#'
#' @param x A vector of numeric, integer, logical, or character values.
#' @param replace What to replace `NA` with.
#'
#' @returns A vector of (usually) the same class as the input.
#'
#' @noRd
.replaceNAsAtomic <- function(x, replace) {

	if (length(replace) != 1L) stop("Argument ", sQuote("replace"), " must be a single value.")
	x[is.na(x)] <- replace
	x

}
