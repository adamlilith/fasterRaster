globalVariables(c('..int', '..char', '..num'))
.restore <- NULL
require(sf)
require(terra)

#' @importFrom terra app
methods::setGeneric(name = "appFuns", def = function(show, ...) standardGeneric("appFuns"))
methods::setGeneric(name = "appCheck", def = function(x, fun, ...) standardGeneric("appCheck"))
#' @importFrom terra as.contour
# # # methods::setGeneric(name = 'as.contour', package='terra')
methods::setGeneric(name = 'as.cell', def = function(x, ...) standardGeneric('as.cell'))
methods::setGeneric(name = 'as.fcell', def = function(x, ...) standardGeneric('as.fcell'))
methods::setGeneric(name = 'as.dcell', def = function(x, ...) standardGeneric('as.dcell'))
setGeneric('as.data.frame')
methods::setGeneric(name = 'aggregate', package='stats')

methods::setGeneric(name = 'bottom', def = function(x, ...) standardGeneric('bottom'))
# methods::setGeneric(name = 'buffer', package='terra')
# methods::setGeneric(name = 'buffer', package='terra')
#' @importFrom terra buffer

# 'c' already in base as S3 generic
methods::setGeneric(name = 'cleanGeom', def = function(x, ...) standardGeneric('cleanGeom'))
methods::setGeneric(name = 'clump', def = function(x, ...) standardGeneric('clump'))
#' @importFrom terra compareGeom
methods::setGeneric(name = 'connectors', def = function(x, y, ...) standardGeneric('connectors'))
methods::setGeneric(name = '.copyGSpatial', def = function(x, ...) standardGeneric('.copyGSpatial'))
#' @importFrom terra convHull
methods::setGeneric(name = 'count', def = function(x, ...) standardGeneric('count'))
#' @importFrom terra crds
#' @importFrom terra crs
#' @importFrom terra crop

# 'dim' already in base
methods::setGeneric(name = 'datatype', def = function(x, ...) standardGeneric('datatype'))
#' @importFrom terra delaunay
methods::setGeneric(name = 'distance', def = function(x, y, ...) standardGeneric('distance'))
methods::setGeneric(name = 'nunique', def = function(x, ...) standardGeneric('nunique'))

methods::setGeneric(name = 'east', def=function(x, ...) standardGeneric('east'))
#' @importFrom terra ext
#' @importFrom terra extend

methods::setGeneric(name = 'fast', def = function(x, ...) standardGeneric('fast'))
#' @importFrom terra focal
#' @importFrom terra freq

#' @importFrom terra geomtype
#' @importFrom terra global
methods::setGeneric(name = 'geomtype', package='terra')
methods::setGeneric(name = 'global', package='terra')
methods::setGeneric(name = '.gnames', def = function(x) standardGeneric('.gnames'))

#' @importFrom utils head
methods::setGeneric(name = 'hillshade', def=function(x, ...) standardGeneric('hillshade'))
methods::setGeneric(name = 'horizonHeight', def = function(x, ...) standardGeneric('horizonHeight'))

methods::setGeneric(name = 'is.2d', def = function(x) standardGeneric('is.2d'))
methods::setGeneric(name = 'is.3d', def = function(x) standardGeneric('is.3d'))
#' @importFrom terra is.lines
#' @importFrom terra is.points
#' @importFrom terra is.polygons
methods::setGeneric(name = 'intercept', def=function(x, ...) standardGeneric('intercept'))

methods::setGeneric(name = 'kurtosis', def=function(x, ...) standardGeneric('kurtosis'))

methods::setGeneric(name = 'location', def = function(x) standardGeneric('location'))
methods::setGeneric(name = 'longlat', def = function(x) standardGeneric('longlat'))

methods::setGeneric(name = 'mapset', def = function(x) standardGeneric('mapset'))
#' @importFrom terra mask
#' @importFrom terra minmax
#' @importFrom terra merge
#' @importFrom stats median
methods::setGeneric(name = 'mmode', def = function(x, ...) standardGeneric('mmode'))

# 'names' already in base
methods::setGeneric(name = 'ncat', def = function(x) standardGeneric('ncat'))
#' @importFrom terra ncell
methods::setGeneric(name = 'ncell3d', def = function(x) standardGeneric('ncell3d'))
methods::setGeneric(name = 'ndepth', def = function(x) standardGeneric('ndepth'))
#' @importFrom methods new
#' @importFrom terra nlyr
methods::setGeneric(name = 'nacell', def = function(x) standardGeneric('nacell'))
methods::setGeneric(name = 'nonnacell', def = function(x) standardGeneric('nonnacell'))
methods::setGeneric(name = 'north', def=function(x, ...) standardGeneric('north'))
methods::setGeneric(name = 'ncol', def = function(x) standardGeneric('ncol')) # in base
methods::setGeneric(name = 'nrow', def = function(x) standardGeneric('nrow')) # in base

#' @importFrom terra origin

#' @importFrom terra project

#' @importFrom stats quantile

methods::setGeneric(name = 'r2', def=function(x, ...) standardGeneric('r2'))
#' @importFrom terra rast
methods::setGeneric(name = '.refresh', def=function(x, ...) standardGeneric('.refresh'))
methods::setGeneric(name = 'regionDim', def=function(x, ...) standardGeneric('regionDim'))
methods::setGeneric(name = 'regionExt', def=function(x, ...) standardGeneric('regionExt'))
methods::setGeneric(name = 'regionRes', def=function(x, ...) standardGeneric('regionRes'))
methods::setGeneric(name = 'regionRes3d', def=function(x, ...) standardGeneric('regionRes3d'))
methods::setGeneric(name = 'region', def=function(x, ...) standardGeneric('region'))
methods::setGeneric(name = 'regionZDim', def=function(x, ...) standardGeneric('regionZDim'))
methods::setGeneric(name = 'regionZExt', def=function(x, ...) standardGeneric('regionZExt'))
methods::setGeneric(name = 'regionZRes', def=function(x, ...) standardGeneric('regionZRes'))
#' @importFrom terra res
#' @importFrom terra resample
methods::setGeneric(name = 'res3d', def = function(x) standardGeneric('res3d'))

methods::setGeneric(name = 'sdpop', def=function(x, ...) standardGeneric('sdpop'))
#' @importFrom methods show
#' @importFrom terra simplifyGeom
methods::setGeneric(name = 'smoothGeom', def=function(x, ...) standardGeneric('smoothGeom'))
methods::setGeneric(name = 'skewness', def=function(x, ...) standardGeneric('skewness'))
methods::setGeneric(name = 'slope', def=function(x, ...) standardGeneric('slope'))
methods::setGeneric(name = 'south', def=function(x, ...) standardGeneric('south'))
#' @importFrom terra spatSample
#' @importFrom terra stretch
methods::setGeneric(name = 'st_as_sf', def=function(x, ...) standardGeneric('st_as_sf'))
if (!isGeneric('st_bbox')) methods::setGeneric(name = 'st_bbox', def=function(obj, ...) standardGeneric('st_bbox'))
methods::setGeneric(name = 'st_buffer', def=function(x, ...) standardGeneric('st_buffer'))
if (!isGeneric('st_crs')) methods::setGeneric(name = 'st_crs', def=function(x, ...) standardGeneric('st_crs'))
methods::setGeneric(name = 'st_distance', def=function(x, y, ...) standardGeneric('st_distance'))

#' @importFrom utils tail
#' @importFrom terra terrain
methods::setGeneric(name = 'top', def=function(x, ...) standardGeneric('top'))
methods::setGeneric(name = 'topology', def=function(x, ...) standardGeneric('topology'))
methods::setGeneric(name = 'tvalue', def=function(x, ...) standardGeneric('tvalue'))

methods::setGeneric(name = 'varpop', def=function(x, ...) standardGeneric('varpop'))
#' @importFrom terra vect
#' @importFrom terra viewshed
#' @importFrom terra voronoi

methods::setGeneric(name = 'west', def=function(x, ...) standardGeneric('west'))
#' @importFrom terra writeRaster
#' @importFrom terra writeVector

#' @importFrom terra xres

#' @importFrom terra yres

methods::setGeneric(name = 'zext', def = function(x) standardGeneric('zext'))
methods::setGeneric(name = 'zres', def = function(x) standardGeneric('zres'))
