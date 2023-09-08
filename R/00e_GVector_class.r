#' @title Classes for fasterRaster locations, rasters, and vectors
#'
#' @aliases GVector
#' @rdname GSession
#' @exportClass GVector
GVector <- methods::setClass(
	"GVector",
	contains = "GSpatial",
	slots = list(
		projection = "character",
		nGeometries = "integer",
		geometry = "character",
		table = "data.table"
	),
	prototype = prototype(
		projection = NA_character_,
		geometry = NA_character_,
		nGeometries = NA_integer_,
		table = data.table::data.table(NULL)
	)
)


setValidity("GVector",
	function(object) {
		if (!all(object@geometry %in% c(NA_character_, "points", "lines", "polygons"))) {
			paste0("@geometry can only be NA, ", sQuote("points"), ", ", sQuote("lines"), ", or ", sQuote("polygons"), ".")
		} else if (nrow(object@table) > 0L && nrow(object@table) != object@nGeometries) {
			"The data.table in @table must be a NULL table (data.table(NULL)), or it must have the same number of rows as @nGeometries."
		} else {
			TRUE
		}
	} # EOF
)

#' Create a GVector
#'
#' @description Create a `GVector` from a vector existing in the current **GRASS** session.
#'
#' @param src Character: The name of the vector in **GRASS**.
#'
#' @param table A `data.table`. This can be `data.table(NULL)` if there is no table associated with the vector.
#'
#' @returns A `GVector`.
#'
#' @seealso [.makeGRaster()]
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @noRd
.makeGVector <- function(src, table = NULL) {

	if (is.null(table)) table <- data.table::data.table(NULL)
	if (!inherits(table, "data.table")) table <- data.table::as.data.table(table)

	info <- .vectInfo(src)
	new(
		"GVector",
		location = getFastOptions("location"),
		mapset = getFastOptions("mapset"),
		crs = crs(),
  		projection = info[["projection"]][1L],
		topology = info[["topology"]][1L],
		sources = src,
		geometry = info[["geometry"]][1L],
		nGeometries = info[["nGeometries"]],
		extent = c(info[["west"]][1L], info[["east"]][1L], info[["south"]][1L], info[["north"]][1L]),
		zextent = c(info[["zbottom"]], info[["ztop"]]),
		table = table
	)
	
}
