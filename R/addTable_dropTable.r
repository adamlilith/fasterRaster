#' Attach or detach GVector's data table
#'
#' @description `addTable()` adds an entire table to a `GVector`. It will replace any existing table. There must be one row in the table for each geometry (see [ngeom()]). You can also add a table column-by-column using the \code{\link[fasterRaster]{$<-}} operator.
#'
#' `dropTable()` removes a data table associated with a `GVector`.
#'
#' @param x A `GVector`.
#' @param ... Other arguments (ignored).
#' @param value A `data.frame`, `data.table`, or `matrix`.
#'
#' @returns A `GVector`.
#'
#' @seealso \code{\link[fasterRaster]{$<-}}, [colbind()], [rbind()], [as.data.frame()], [as.data.table()]
#'
#' @example man/examples/ex_cbind_rbind_addTable_dropTable.r
#'
#' @aliases addTable<-,GRaster,data.frame-method
#' @rdname addTable
#' @exportMethod addTable<-
methods::setMethod(
	f = "addTable<-",
	signature = c(x = "GVector", value = "data.frame"),
	function(x, ..., value) .addTable(x = x, table = value)
)

#' @aliases addTable<-,GRaster,data.table-method
#' @rdname addTable
#' @exportMethod addTable<-
methods::setMethod(
	f = "addTable<-",
	signature = c(x = "GVector", value = "data.table"),
	function(x, ..., value) .addTable(x = x, table = value)
)

#' @aliases addTable<-,GRaster,matrix-method
#' @rdname addTable
#' @exportMethod addTable<-
methods::setMethod(
	f = "addTable<-",
	signature = c(x = "GVector", value = "matrix"),
	function(x, ..., value) .addTable(x = x, table = value)
)

#' Add a data table to a `GVector`
#'
#' @param A `GVector`.
#' @param table A `data.frame`, `data.table`, or `matrix`.
#'
#' @noRd
.addTable <- function(x, table) {

	if (nrow(table) != ngeom(x)) stop("The GVector must have one geometry per row in the table.")
	if (!inherits(table, "data.table")) table <- data.table::as.data.table(table)
	x@table <- table
	methods::validObject(x)
	x

}

#' @aliases dropTable
#' @rdname addTable
#' @exportMethod dropTable
methods::setMethod(
	f = "dropTable",
	signature = c(x = "GVector"),
	function(x) {
	
	out <- x
	out@table <- data.table::data.table(NULL)
	out

	} # EOF
)
