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
		nSubgeometries = "integer",
		geometry = "character",
		table = "data.table"
	),
	prototype = prototype(
		projection = NA_character_,
		geometry = NA_character_,
		nGeometries = NA_integer_,
		nSubgeometries = NA_integer_,
		table = data.table::data.table(NULL)
	)
)


setValidity("GVector",
	function(object) {
		if (!all(object@geometry %in% c(NA_character_, "points", "lines", "polygons"))) {
			paste0("@geometry can only be NA, ", sQuote("points"), ", ", sQuote("lines"), ", or ", sQuote("polygons"), ".")
		# } else if (length(unique(.vCats(object)) != object@nGeometries)) {
			# "The number of @nGeometries is not the same as the number of unique ", sQuote("cat"), " values in the vector attribute table in GRASS."
		} else if (object@nGeometries > object@nSubgeometries) {
			"The number of sub-geometries in @nSubgeometries must be <= the number of geometries in @nGeometries."
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
#' @param table A `data.table`, `data.frame`, or character. This can be `data.table(NULL)` or `data.frame(NULL)` if there is no table associated with the vector. If a character, this is interpreted as the name of the table in **GRASS**.
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

	if (any(names(table) == "frKEY")) {

		catTable <- table[ , "frKEY"]
		names(catTable) <- "cat"
		table$frKEY <- NULL
	
		### create GRASS attribute table
		### 1 Save table and format file
		### 2 Read in with db.in
		### 3 Attach to vector and set "cat" column
		tf <- tempfile(fileext = ".csv")
		tft <- paste0(tf, "t")
		utils::write.csv(catTable, tf, row.names = FALSE)
		keyTableType <- '"Integer"'
		write(keyTableType, tft)

		attsSrc <- .makeSourceName("db_in_ogr", "table")
		args <- list(
			cmd = "db.in.ogr",
			input = tf,
			output = attsSrc,
			key = "cat",
			flags = c("quiet", "overwrite")
		)
		do.call(rgrass::execGRASS, args = args)

		### attach table
		args <- list(
			cmd = "v.db.connect",
			map = src,
			table = attsSrc,
			key = "cat",
			flags = c("quiet", "overwrite")
		)
		do.call(rgrass::execGRASS, args = args)

		nGeoms <- nrow(catTable[ , .N, by = "cat"])

	# GRASS made its own cat column
	} else {

		nGeoms <- max(.vGeometries(src))

	}

	info <- .vectInfo(src)
	nSubgeoms <- info[["nGeometries"]]

	new(
		"GVector",
		location = getFastOptions("location"),
		mapset = getFastOptions("mapset"),
		crs = crs(),
  		projection = info[["projection"]][1L],
		topology = info[["topology"]][1L],
		sources = src,
		geometry = info[["geometry"]][1L],
		nGeometries = nGeoms,
		nSubgeometries = nSubgeoms,
		extent = c(info[["west"]][1L], info[["east"]][1L], info[["south"]][1L], info[["north"]][1L]),
		zextent = c(info[["zbottom"]], info[["ztop"]]),
		table = table
	)

}
