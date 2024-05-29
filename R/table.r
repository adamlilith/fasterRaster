#' Add, replace, or remove a GVector's data table
#'
#' @description `GVector`s can have data tables associated with them, with one row per geometry (see [ngeom()]). This function can be used to attach or replace a data table, or to remove a data table.
#'
#' @param x A `GVector`.
#'
#' @param value A `data.frame`, `data.table`, `matrix`, a `GVector` with a data table, *or* `NULL`. If this is `NULL`, then any existing data table is removed. Otherwise, the existing table (if any) will be replaced with the new table. If `value` is a `GVector`, then its data table will be used to replace the one in `x`.
#'
#' @example man/examples/ex_cbind_rbind_table.r
#'
#' @returns A `GVector`.
#'
#' @seealso [rbind()], [colbind()]
#'
#' @aliases table<-
#' @rdname table
#' @exportMethod table<-
methods::setMethod(
	f = "table<-",
	signature = c(x = "GVector", value = "ANY"),
	function(x, value) {

	if (is.null(value)) {
		
		value <- data.table::data.table(NULL)
	
	} else if (inherits(value, "GVector")) {
		
		if (nrow(value) != ngeom(x)) stop("The data table must have the same number of rows as the GVector has geometries.")	
		value <- x@table
	
	} else {

		if (nrow(value) != ngeom(x)) stop("The data table must have the same number of rows as the GVector has geometries.")	
		if (!inherits(value, "data.table")) value <- data.table::as.data.table(value)

	}

	x@table <- value
	methods::validObject(x)
	x

	} # EOF
)
