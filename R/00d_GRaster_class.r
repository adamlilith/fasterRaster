#' @title Classes for "fasterRaster" locations, rasters, and vectors
#'
#' @describeIn GSession
#'
#' @importFrom methods new
#' @exportClass GRaster
GRaster <- setClass(
	"GRaster",
	contains = "GRegion",
	slots = list(
		projection = "character",
		datatypeGRASS = "character",
		nLayers = "integer",
		names = "character",
		nCats = "integer",
		minVal = "numeric",
		maxVal = "numeric"
	),
	prototype = prototype(
		projection = NA_character_,
		datatypeGRASS = NA_character_,
		nLayers = NA_integer_,
		names = NA_character_,
		nCats = NA_integer_,
		minVal = NA_real_,
		maxVal = NA_real_
	)
)

setValidity("GRaster",
	function(object) {
		if (!all(object@datatypeGRASS %in% c("CELL", "FCELL", "DCELL"))) {
			paste0("@datatypeGRASS can only be NA, ", sQuote("CELL"), ", ", sQuote("FCELL"), ", or ", sQuote("DCELL"), ".")
		} else if (!is.na(object@dimensions[3L]) && object@dimensions[3L] <= 0L) {
			"Third value in @dimensions must be NA or a positive integer."
		} else if (!is.na(object@resolution[3L]) && object@resolution[3L] <= 0) {
			"Third value in @resolution must be NA or a positive real value."
		} else if (object@nLayers < 1) {
			"@nLayers must be a positive integer."
		} else if (object@nLayers != length(object@gnames)) {
			"@nLayers is different from the number of @gnames."
		} else if (object@nLayers != length(object@names)) {
			"@names must be @nLayers in length."
		} else if (object@nLayers != length(object@nCats)) {
			"@nCats must be @nLayers in length."
		} else if (object@nLayers != length(object@minVal)) {
			"@minVal must be @nLayers in length."
		} else if (object@nLayers != length(object@maxVal)) {
			"@maxVal must be @nLayers in length."
		} else {
			TRUE
		}
	} # EOF
)

#' Create a GRaster
#'
#' @description Create a `GRaster` from a raster existing in the current **GRASS** session.
#'
#' @param gn Character: The name of the raster in **GRASS**.
#' @param names Character: Name of the raster.
#'
#' @returns A `GRaster`.
#'
#' @seealso [.makeGVector()]
#'
#' @example man/examples/ex_GRaster_GVector.r
#'
#' @noRd
.makeGRaster <- function(gn, names = "raster") {

	info <- .rastInfo(gn)
	out <- new(
		"GRaster",
		location = getFastOptions("location"),
		mapset = getFastOptions("mapset"),
		crs = crs(),
		projection = info[["projection"]][1L],
		topology = info[["topology"]][1L],
		extent = c(info[["west"]][1L], info[["east"]][1L], info[["south"]][1L], info[["north"]][1L]),
		zextent = c(info[["zbottom"]][1L], info[["ztop"]][1L]),
		nLayers = length(info$gnames),
		dimensions = c(info[["rows"]][1L], info[["cols"]][1L], info[["depths"]][1L]),
		resolution = c(info[["ewres"]][1L], info[["nsres"]][1L], info[["tbres"]][1L]),
		gnames = gn,
		names = names,
		datatypeGRASS = info[["grassDataType"]],
		nCats = info[["nCats"]],
		minVal = info[["minVal"]],
		maxVal = info[["maxVal"]]
	)
	out <- .makeUniqueNames(out)
	out

}
