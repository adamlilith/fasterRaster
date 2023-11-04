.restore <- NULL

# bad form:
# require(data.table)
# require(sf)
# require(terra)

# creates conflicts with terra::shift and data.table::shift
# #' @import terra
# #' @import data.table
# #' @import terra

#' @import sf
#' @rawNamespace import(data.table, except = shift)
#' @rawNamespace import(terra, except = shift)
#' @importFrom methods new
#' @importFrom methods setMethod
#' @importFrom methods setValidity
#' @importFrom methods validObject

### data.table
.datatable.aware <- TRUE

if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(".", ".I", ".N", ".SD"), utils::packageName())
    utils::globalVariables(c("..ac", "..cols", "..i", "..j", "j", "i"), utils::packageName())
}

### GENERICS
#' @importFrom data.table :=
# if (!isGeneric("[")) methods::setGeneric(name = "[", def = function(x, ...) standardGeneric("["))
# if (!isGeneric("[<-")) methods::setGeneric(name = "[<-", def = function(x, ...) standardGeneric("[<-"))

# $ is already generic
# $<- is already generic
# [ is already generic
# [<- is already generic
# [[ is already generic
# [[<- is already generic

methods::setGeneric(name = ".copyGSpatial", def = function(x, ...) standardGeneric(".copyGSpatial"))
# methods::setGeneric(name = ".dbCats", def = function(x, ...) standardGeneric(".dbCats"))
# methods::setGeneric(name = ".dbToDataTable", def = function(x, ...) standardGeneric(".dbToDataTable"))
# methods::setGeneric(name = ".dbRemove", def = function(x, ...) standardGeneric(".dbRemove"))

# methods::setGeneric(name = "activeCat", package = "terra")
methods::setGeneric(name = "activeCat", def = function(x, ...) standardGeneric("activeCat"))
methods::setGeneric(name = "activeCat<-", package = "terra")
methods::setGeneric(name = "add<-", package = "terra")
methods::setGeneric(name = "addCats", package = "terra")
methods::setGeneric(name = "addCats<-", def = function(x, ..., value) standardGeneric("addCats<-"))
methods::setGeneric(name = "app", package = "terra")
# methods::setGeneric(name = "appFuns", def = function(x, ...) standardGeneric("appFuns"))
methods::setGeneric(name = "appCheck", def = function(x, fun, ...) standardGeneric("appCheck"))
methods::setGeneric(name = "as.contour", package = "terra")
setGeneric("as.data.frame")
methods::setGeneric(name = "as.data.table", package = "data.table")
methods::setGeneric(name = "as.doub", def = function(x, ...) standardGeneric("as.doub"))
methods::setGeneric(name = "as.float", def = function(x, ...) standardGeneric("as.float"))
# as.integer() is primitive
methods::setGeneric(name = "as.int", package = "terra")
setGeneric("as.lines", function(x,...) standardGeneric("as.lines"))
setGeneric("as.points", function(x,...) standardGeneric("as.points"))
setGeneric("as.polygons", function(x,...) standardGeneric("as.polygons"))
setGeneric("aggregate", function(x, ...) standardGeneric("aggregate"), package = "stats")

methods::setGeneric(name = "barplot", package = "terra")
methods::setGeneric(name = "bottom", def = function(x, ...) standardGeneric("bottom"))
methods::setGeneric(name = "buffer", package = "terra")

# c() is primitive
methods::setGeneric(name = "categories", package = "terra")
methods::setGeneric(name = "catNames", def = function(x, ...) standardGeneric("catNames"))
methods::setGeneric(name = "cats", package = "terra")
methods::setGeneric(name = "cleanGeom", def = function(x, ...) standardGeneric("cleanGeom"))
methods::setGeneric(name = "clump", def = function(x, ...) standardGeneric("clump"))
methods::setGeneric(name = "compareGeom", package = "terra")
methods::setGeneric(name = "connectors", def = function(x, y, ...) standardGeneric("connectors"))
setGeneric("cor", function(x, y, ...) standardGeneric("cor"), package = "stats")
setGeneric("cov", function(x, ...) standardGeneric("cov"), package = "stats")
methods::setGeneric(name = "convHull", package = "terra")
methods::setGeneric(name = "count", def = function(x, ...) standardGeneric("count"))
methods::setGeneric(name = "crds", package = "terra")
methods::setGeneric(name = "crs", package = "terra")
methods::setGeneric(name = "crop", package = "terra")


# dim() is primitive
methods::setGeneric(name = "datatype", def = function(x, ...) standardGeneric("datatype"))
methods::setGeneric(name = "delaunay", package = "terra")
methods::setGeneric(name = "denoise", def = function(x, ...) standardGeneric("denoise"))
methods::setGeneric(name = "disagg", package = "terra")
methods::setGeneric(name = "distance", def = function(x, y, ...) standardGeneric("distance"))
methods::setGeneric(name = "droplevels", package = "terra")
methods::setGeneric(name = "dropRows", def = function(x, ...) standardGeneric("dropRows"))

methods::setGeneric(name = "E", def = function(x, ...) standardGeneric("E"))
methods::setGeneric(name = "ext", package = "terra") 
methods::setGeneric(name = "extend", package = "terra")
methods::setGeneric(name = "extract", package = "terra")

methods::setGeneric(name = "fast", def = function(x, ...) standardGeneric("fast"))
methods::setGeneric(name = "focal", package = "terra")
methods::setGeneric(name = "fractalRast", def = function(x, ...) standardGeneric("fractalRast"))
methods::setGeneric(name = "freq", package = "terra")

methods::setGeneric(name = "geomtype", package = "terra")
methods::setGeneric(name = "global", package = "terra")

methods::setGeneric(name = "head", package = "utils")
methods::setGeneric(name = "hillshade", def = function(x, ...) standardGeneric("hillshade"))
methods::setGeneric(name = "hist", package = "terra")
methods::setGeneric(name = "horizonHeight", def = function(x, ...) standardGeneric("horizonHeight"))

methods::setGeneric(name = "is.2d", def = function(x) standardGeneric("is.2d"))
methods::setGeneric(name = "is.3d", def = function(x) standardGeneric("is.3d"))
if (!isGeneric("is.factor")) { methods::setGeneric(name = "is.factor", def = function(x) standardGeneric("is.factor")) }
# is.double() is primitive and cannot be made generic
methods::setGeneric(name = "is.cell", def = function(x) standardGeneric("is.cell"))
methods::setGeneric(name = "is.doub", def = function(x) standardGeneric("is.doub"))
methods::setGeneric(name = "is.float", def = function(x) standardGeneric("is.float"))
# methods::setGeneric(name = "is.int", def = function(x) standardGeneric("is.int"))
methods::setGeneric(name = "is.int", package = "terra")
methods::setGeneric(name = "is.lines", package = "terra")
methods::setGeneric(name = "is.lonlat", package = "terra")
if (!isGeneric("is.na")) { methods::setGeneric(name = "is.na", def = function(x) standardGeneric("is.na")) }
methods::setGeneric(name = "is.points", package = "terra")
methods::setGeneric(name = "is.polygons", package = "terra")
methods::setGeneric(name = "intercept", def = function(x, ...) standardGeneric("intercept"))

# methods::setGeneric(name = "keepRows", def = function(x, ...) standardGeneric("keepRows"))
methods::setGeneric(name = "kernel", def = function(x, ...) standardGeneric("kernel"))
methods::setGeneric(name = "kurtosis", def = function(x, ...) standardGeneric("kurtosis"))

# levels (in base) is generic
# levels<- (in base) is primitive
methods::setGeneric(name = "ln", def = function(x, ...) standardGeneric("ln"))
methods::setGeneric(name = "location", def = function(x) standardGeneric("location"))
methods::setGeneric(name = "longlat", def = function(x) standardGeneric("longlat"))

methods::setGeneric(name = "mapset", def = function(x) standardGeneric("mapset"))
methods::setGeneric(name = "mask", package = "terra")
methods::setGeneric(name = "minmax", package = "terra")
methods::setGeneric(name = "missingCats", def = function(x, ...) standardGeneric("missingCats"))
methods::setGeneric(name = "merge", package = "terra")
#' @importFrom terra median
methods::setGeneric(name = "median", def = function(x, na.rm) standardGeneric("median"))
methods::setGeneric(name = "mmode", def = function(x, ...) standardGeneric("mmode"))

# "names" (in base) is primitive
methods::setGeneric(name = "N", def = function(x, ...) standardGeneric("N"))
methods::setGeneric(name = "ncell", package = "terra")
methods::setGeneric(name = "ncell3d", def = function(x) standardGeneric("ncell3d"))
methods::setGeneric(name = "ncol", def = function(x) standardGeneric("ncol")) # in base
methods::setGeneric(name = "ndepth", def = function(x) standardGeneric("ndepth"))
methods::setGeneric(name = "new", package = "methods") 
methods::setGeneric(name = "nlyr", package = "terra")
methods::setGeneric(name = "nacell", def = function(x) standardGeneric("nacell"))
methods::setGeneric(name = "ngeom", def = function(x, ...) standardGeneric("ngeom"))
methods::setGeneric(name = "nlevels", package = "terra")
methods::setGeneric(name = "noise", def = function(x, ...) standardGeneric("noise"))
methods::setGeneric(name = "nonnacell", def = function(x) standardGeneric("nonnacell"))
methods::setGeneric(name = "not.na", def = function(x, ...) standardGeneric("not.na")) # in base
methods::setGeneric(name = "nrow", def = function(x) standardGeneric("nrow")) # in base
methods::setGeneric(name = "nsubgeom", def = function(x, ...) standardGeneric("nsubgeom"))
methods::setGeneric(name = "nunique", def = function(x, ...) standardGeneric("nunique"))

methods::setGeneric(name = "pairs", package = "terra")
methods::setGeneric(name = "pca", def = function(x, ...) standardGeneric("pca"))
methods::setGeneric(name = "plot", package = "terra")
methods::setGeneric(name = "plotRGB", package = "terra")
methods::setGeneric(name = "project", package = "terra")

methods::setGeneric(name = "quantile", package = "terra")

methods::setGeneric(name = "r2", def = function(x, ...) standardGeneric("r2"))
methods::setGeneric(name = "rast", package = "terra")
methods::setGeneric(name = "replaceNAs", def = function(x, ...) standardGeneric("replaceNAs"))
methods::setGeneric(name = "rnormRast", def = function(x, ...) standardGeneric("rnormRast"))
methods::setGeneric(name = "runifRast", def = function(x, ...) standardGeneric("runifRast"))
methods::setGeneric(name = "rvoronoi", def = function(x, ...) standardGeneric("rvoronoi"))

methods::setGeneric(name = "regionDim", def = function(x, ...) standardGeneric("regionDim"))
methods::setGeneric(name = "regionExt", def = function(x, ...) standardGeneric("regionExt"))
methods::setGeneric(name = "regionRes", def = function(x, ...) standardGeneric("regionRes"))
methods::setGeneric(name = "region", def = function(x, ...) standardGeneric("region"))
methods::setGeneric(name = "res", package = "terra")
methods::setGeneric(name = "resample", package = "terra")
methods::setGeneric(name = "res3d", def = function(x) standardGeneric("res3d"))

methods::setGeneric(name = "sampleRast", def = function(x, ...) standardGeneric("sampleRast"))
methods::setGeneric(name = "sdpop", def = function(x, ...) standardGeneric("sdpop"))
methods::setGeneric(name = "selectRange", def = function(x, ...) standardGeneric("selectRange"))
methods::setGeneric(name = "show", package = "methods")
methods::setGeneric(name = "simplifyGeom", package = "terra")
methods::setGeneric(name = "smoothGeom", def = function(x, ...) standardGeneric("smoothGeom"))
methods::setGeneric(name = "skewness", def = function(x, ...) standardGeneric("skewness"))
methods::setGeneric(name = "slope", def = function(x, ...) standardGeneric("slope"))
methods::setGeneric(name = "S", def = function(x, ...) standardGeneric("S"))
methods::setGeneric(name = "sources", package = "terra")
methods::setGeneric(name = "spatSample", package = "terra")
methods::setGeneric(name = "spDepRast", def = function(x, ...) standardGeneric("spDepRast"))
methods::setGeneric(name = "stretch", package = "terra")
methods::setGeneric(name = "st_as_sf", package = "sf")
methods::setGeneric(name = "st_bbox", package = "sf")
methods::setGeneric(name = "st_buffer", package = "sf")
methods::setGeneric(name = "st_crs", package = "sf")
methods::setGeneric(name = "st_distance", package = "sf")
methods::setGeneric(name = "st_distance", package = "sf")
methods::setGeneric(name = "summary", def = function(object, ...) standardGeneric("summary"))

methods::setGeneric(name = "tail", package = "utils")
methods::setGeneric(name = "terrain", package = "terra")
methods::setGeneric(name = "thinLines", def = function(x, ...) standardGeneric("thinLines"))
methods::setGeneric(name = "thinPoints", def = function(x, y, ...) standardGeneric("thinPoints"))
methods::setGeneric(name = "top", def = function(x, ...) standardGeneric("top"))
methods::setGeneric(name = "topology", def = function(x, ...) standardGeneric("topology"))
methods::setGeneric(name = "trim", package = "terra")
methods::setGeneric(name = "tvalue", def = function(x, ...) standardGeneric("tvalue"))

methods::setGeneric(name = "update", package = "terra")

methods::setGeneric(name = "varpop", def = function(x, ...) standardGeneric("varpop"))
methods::setGeneric(name = "vegIndex", def = function(x, ...) standardGeneric("vegIndex"))
methods::setGeneric(name = "vect", package = "terra")
methods::setGeneric(name = "viewshed", package = "terra")
methods::setGeneric(name = "voronoi", package = "terra")

methods::setGeneric(name = "W", def = function(x, ...) standardGeneric("W"))
methods::setGeneric(name = "writeRaster", package = "terra")
methods::setGeneric(name = "writeVector", package = "terra")

methods::setGeneric(name = "xres", package = "terra")

methods::setGeneric(name = "yres", package = "terra")

methods::setGeneric(name = "zext", def = function(x) standardGeneric("zext"))
methods::setGeneric(name = "zres", def = function(x) standardGeneric("zres"))
