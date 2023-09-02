.restore <- NULL
# require(data.table)
# require(sf)
# require(terra)

# #' @import data.table
# #' @import sf
# #' @import terra

### data.table
.datatable.aware <- TRUE

if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(".", ".I", ".N", ".SD"), utils::packageName())
}

### GENERICS
if (!isGeneric("[")) methods::setGeneric(name = "[", def = function(x, ...) standardGeneric("["))
if (!isGeneric("[<-")) methods::setGeneric(name = "[<-", def = function(x, ...) standardGeneric("[<-"))
if (!isGeneric("[[")) methods::setGeneric(name = "[[", def = function(x, ...) standardGeneric("[["))
if (!isGeneric("[[")) methods::setGeneric(name = "[[<-", def = function(x, ...) standardGeneric("[[<-"))
methods::setGeneric(name = ".maxVal", def = function(x, ...) standardGeneric(".maxVal"))
methods::setGeneric(name = ".minVal", def = function(x, ...) standardGeneric(".minVal"))

# #' @importFrom terra activeCat
methods::setGeneric(name = "activeCat", package = "terra")
# #' @importFrom terra activeCat<-
methods::setGeneric(name = "activeCat<-", package = "terra")
# #' @importFrom terra add<-
methods::setGeneric(name = "add<-", package = "terra")
# #' @importFrom terra app
methods::setGeneric(name = "app", package = "terra")
methods::setGeneric(name = "appFuns", def = function(show, ...) standardGeneric("appFuns"))
methods::setGeneric(name = "appCheck", def = function(x, fun, ...) standardGeneric("appCheck"))
# #' @importFrom terra as.contour
methods::setGeneric(name = "as.contour", package = "terra")
setGeneric("as.data.frame")
methods::setGeneric(name = "as.doub", def = function(x, ...) standardGeneric("as.doub"))
methods::setGeneric(name = "as.float", def = function(x, ...) standardGeneric("as.float"))
# as.integer() is primitive
methods::setGeneric(name = "as.int", package = "terra")
setGeneric("as.lines", function(x,...) standardGeneric("as.lines"))
setGeneric("as.points", function(x,...) standardGeneric("as.points"))
setGeneric("as.polygons", function(x,...) standardGeneric("as.polygons"))
setGeneric("aggregate", function(x, ...) standardGeneric("aggregate"), package = "stats")

methods::setGeneric(name = "bottom", def = function(x, ...) standardGeneric("bottom"))
methods::setGeneric(name = "buffer", package = "terra")

# c() is primitive
# #' @importFrom terra categories
methods::setGeneric(name = "categories", package = "terra")
# #' @importFrom terra cats
methods::setGeneric(name = "cats", package = "terra")
methods::setGeneric(name = "cleanGeom", def = function(x, ...) standardGeneric("cleanGeom"))
methods::setGeneric(name = "clump", def = function(x, ...) standardGeneric("clump"))
# #' @importFrom terra compareGeom
methods::setGeneric(name = "compareGeom", package = "terra")
methods::setGeneric(name = "connectors", def = function(x, y, ...) standardGeneric("connectors"))
methods::setGeneric(name = ".copyGSpatial", def = function(x, ...) standardGeneric(".copyGSpatial"))
# #' @importFrom terra convHull
methods::setGeneric(name = "convHull", package = "terra")
methods::setGeneric(name = "count", def = function(x, ...) standardGeneric("count"))
# #' @importFrom terra crds
methods::setGeneric(name = "crds", package = "terra")
# #' @importFrom terra crs
methods::setGeneric(name = "crs", package = "terra")
# #' @importFrom terra crop
methods::setGeneric(name = "crop", package = "terra")


# dim() is primitive
methods::setGeneric(name = "datatype", def = function(x, ...) standardGeneric("datatype"))
# #' @importFrom terra delaunay
methods::setGeneric(name = "delaunay", package = "terra")
methods::setGeneric(name = "distance", def = function(x, y, ...) standardGeneric("distance"))
methods::setGeneric(name = "droplevels", package = "terra")

methods::setGeneric(name = "east", def = function(x, ...) standardGeneric("east"))
# #' @importFrom terra ext
methods::setGeneric(name = "ext", package = "terra") 
# #' @importFrom terra extend
methods::setGeneric(name = "extend", package = "terra")
# #' @importFrom terra extract
methods::setGeneric(name = "extract", package = "terra")

methods::setGeneric(name = "fast", def = function(x, ...) standardGeneric("fast"))
# #' @importFrom terra focal
methods::setGeneric(name = "focal", package = "terra")
methods::setGeneric(name = "fractalRast", def = function(x, ...) standardGeneric("fractalRast"))
# #' @importFrom terra freq
methods::setGeneric(name = "freq", package = "terra")

# #' @importFrom terra geomtype
methods::setGeneric(name = "geomtype", package = "terra")
# #' @importFrom terra global
methods::setGeneric(name = "global", package = "terra")

# #' @importFrom utils head
methods::setGeneric(name = "head", package = "utils")
methods::setGeneric(name = "hillshade", def = function(x, ...) standardGeneric("hillshade"))
methods::setGeneric(name = "horizonHeight", def = function(x, ...) standardGeneric("horizonHeight"))

methods::setGeneric(name = "is.2d", def = function(x) standardGeneric("is.2d"))
methods::setGeneric(name = "is.3d", def = function(x) standardGeneric("is.3d"))
if (!isGeneric("is.factor")) { methods::setGeneric(name = "is.factor", def = function(x) standardGeneric("is.factor")) }
# is.double() is primitive
methods::setGeneric(name = "is.doub", def = function(x) standardGeneric("is.doub"))
methods::setGeneric(name = "is.float", def = function(x) standardGeneric("is.float"))
methods::setGeneric(name = "is.int", def = function(x) standardGeneric("is.int"))
# #' @importFrom terra is.lines
methods::setGeneric(name = "is.lines", package = "terra")
# #' @importFrom terra is.lonlat
methods::setGeneric(name = "is.lonlat", package = "terra")
if (!isGeneric("is.na")) { methods::setGeneric(name = "is.na", def = function(x) standardGeneric("is.na")) }
# #' @importFrom terra is.points
methods::setGeneric(name = "is.points", package = "terra")
# #' @importFrom terra is.polygons
methods::setGeneric(name = "is.polygons", package = "terra")
methods::setGeneric(name = "intercept", def = function(x, ...) standardGeneric("intercept"))

methods::setGeneric(name = "kurtosis", def = function(x, ...) standardGeneric("kurtosis"))

# levels (in base) is generic
# levels<- (in base) is primitive
methods::setGeneric(name = "ln", def = function(x, ...) standardGeneric("ln"))
methods::setGeneric(name = "location", def = function(x) standardGeneric("location"))
methods::setGeneric(name = "longlat", def = function(x) standardGeneric("longlat"))

methods::setGeneric(name = "mapset", def = function(x) standardGeneric("mapset"))
# #' @importFrom terra mask
methods::setGeneric(name = "mask", package = "terra")
# #' @importFrom terra minmax
methods::setGeneric(name = "minmax", package = "terra")
methods::setGeneric(name = "minmaxCat", def = function(x, ...) standardGeneric("minmaxCat"))
# #' @importFrom terra merge
methods::setGeneric(name = "merge", package = "terra")
#' @importFrom stats median
methods::setGeneric(name = "mmode", def = function(x, ...) standardGeneric("mmode"))

# "names" (in base) is primitive
# #' @importFrom terra ncell
methods::setGeneric(name = "ncell", package = "terra")
methods::setGeneric(name = "ncell3d", def = function(x) standardGeneric("ncell3d"))
methods::setGeneric(name = "ndepth", def = function(x) standardGeneric("ndepth"))
# #' @importFrom methods new
methods::setGeneric(name = "new", package = "methods") 
# #' @importFrom terra nlyr
methods::setGeneric(name = "nlyr", package = "terra")
methods::setGeneric(name = "nacell", def = function(x) standardGeneric("nacell"))
methods::setGeneric(name = "nlevels", def = function(x, ...) standardGeneric("nlevels"))
methods::setGeneric(name = "nonnacell", def = function(x) standardGeneric("nonnacell"))
methods::setGeneric(name = "north", def = function(x, ...) standardGeneric("north"))
# #' @importFrom terra not.na
methods::setGeneric(name = "not.na", def = function(x, ...) standardGeneric("not.na")) # in base
methods::setGeneric(name = "ncol", def = function(x) standardGeneric("ncol")) # in base
methods::setGeneric(name = "nrow", def = function(x) standardGeneric("nrow")) # in base
methods::setGeneric(name = "nunique", def = function(x, ...) standardGeneric("nunique"))

#' @importFrom terra plot
methods::setGeneric(name = "plot", package = "terra")
# #' @importFrom terra project
methods::setGeneric(name = "project", package = "terra")

# #' @importFrom stats quantile
methods::setGeneric(name = "quantile", package = "stats")

methods::setGeneric(name = "r2", def = function(x, ...) standardGeneric("r2"))
# #' @importFrom terra rast
methods::setGeneric(name = "rast", package = "terra")
methods::setGeneric(name = "rnormRast", def = function(x, ...) standardGeneric("rnormRast"))
methods::setGeneric(name = "runifRast", def = function(x, ...) standardGeneric("runifRast"))

methods::setGeneric(name = ".refresh", def = function(x, ...) standardGeneric(".refresh"))
methods::setGeneric(name = "regionDim", def = function(x, ...) standardGeneric("regionDim"))
methods::setGeneric(name = "regionExt", def = function(x, ...) standardGeneric("regionExt"))
methods::setGeneric(name = "regionRes", def = function(x, ...) standardGeneric("regionRes"))
methods::setGeneric(name = "region", def = function(x, ...) standardGeneric("region"))
# #' @importFrom terra res
methods::setGeneric(name = "res", package = "terra")
# #' @importFrom terra resample
methods::setGeneric(name = "resample", package = "terra")
methods::setGeneric(name = "res3d", def = function(x) standardGeneric("res3d"))

methods::setGeneric(name = "sdpop", def = function(x, ...) standardGeneric("sdpop"))
methods::setGeneric(name = "selectRange", def = function(x, ...) standardGeneric("selectRange"))
# #' @importFrom methods show
methods::setGeneric(name = "show", package = "methods")
# #' @importFrom terra simplifyGeom
methods::setGeneric(name = "simplifyGeom", package = "terra")
methods::setGeneric(name = "smoothGeom", def = function(x, ...) standardGeneric("smoothGeom"))
methods::setGeneric(name = "skewness", def = function(x, ...) standardGeneric("skewness"))
methods::setGeneric(name = "slope", def = function(x, ...) standardGeneric("slope"))
methods::setGeneric(name = "south", def = function(x, ...) standardGeneric("south"))
# #' @importFrom terra sources
methods::setGeneric(name = "sources", package = "terra")
# #' @importFrom terra spatSample
methods::setGeneric(name = "spatSample", package = "terra")
methods::setGeneric(name = "spDepRast", def = function(x, ...) standardGeneric("spDepRast"))
# #' @importFrom terra stretch
methods::setGeneric(name = "stretch", package = "terra")
# methods::setGeneric(name = "st_as_sf", package = "sf")
# methods::setGeneric(name = "st_bbox", package = "sf")
# methods::setGeneric(name = "st_bbox", def = function(obj, ...) standardGeneric("st_bbox"))

# methods::setGeneric(name = "st_buffer", package = "sf")
#' @importFrom sf st_crs
methods::setGeneric(name = "st_crs", def = function(x, ...) standardGeneric("st_crs"))
# methods::setGeneric(name = "st_distance", def = function(x, y, ...) standardGeneric("st_distance"))

# #' @importFrom utils tail
methods::setGeneric(name = "tail", package = "utils")
# #' @importFrom terra terrain
methods::setGeneric(name = "terrain", package = "terra")
methods::setGeneric(name = "thin", def = function(x, ...) standardGeneric("thin"))
methods::setGeneric(name = "top", def = function(x, ...) standardGeneric("top"))
methods::setGeneric(name = "topology", def = function(x, ...) standardGeneric("topology"))
# #' @importFrom terra trim
methods::setGeneric(name = "trim", package = "terra")
methods::setGeneric(name = "tvalue", def = function(x, ...) standardGeneric("tvalue"))

methods::setGeneric(name = "varpop", def = function(x, ...) standardGeneric("varpop"))
# #' @importFrom terra vect
methods::setGeneric(name = "vect", package = "terra")
# #' @importFrom terra viewshed
methods::setGeneric(name = "viewshed", package = "terra")
# #' @importFrom terra voronoi
methods::setGeneric(name = "voronoi", package = "terra")

methods::setGeneric(name = "west", def = function(x, ...) standardGeneric("west"))
# #' @importFrom terra writeRaster
methods::setGeneric(name = "writeRaster", package = "terra")
# #' @importFrom terra writeVector
methods::setGeneric(name = "writeVector", package = "terra")

# #' @importFrom terra xres
methods::setGeneric(name = "xres", package = "terra")

# #' @importFrom terra yres
methods::setGeneric(name = "yres", package = "terra")

methods::setGeneric(name = "zext", def = function(x) standardGeneric("zext"))
methods::setGeneric(name = "zres", def = function(x) standardGeneric("zres"))
