#' 'fasterRaster': Faster raster and spatial vector processing using 'GRASS GIS'
#'
#' # Most useful functions/links:
#' * A [tutorial](quick-start tutorial)
#' * [startFast()]: Initiate a **GRASS** session
#' * [fast()]: Create `GRaster`s or `GVector`s from `SpatRaster`s, `SpatVector`s, or `sf` or `stars` objects, or from files
#' * [rast()], [vect()], [st_as_sf()], and [stars()]: Convert `GRaster`s and `GVector`s to `SpatRaster`s, `SpatVector`s, `sf` vectors, or `stars` rasters.
#' * [writeRaster()] and [writeVector()]: Save `GRaster`s or `GVector`s to disk.
#' * [setFastOptions()] and [getFastOptions()]: Set options for working with **fasterRaster**
#'
#' # Properties of **GRASS** rasters
#' * [dim()]: Number of rows and columns
#' * [ext()]: Spatial extent
#' * [minmax()]: Minimum and maximum values across all non-`NA` cells
#' * [ncol()]: Number of columns
#' * [ncell()]: Number of cells
#' * [nlyr()]: Number of layers
#' * [nrow()]: Number of rows
#' * [res()]: Spatial resolution
#' * [topology()]: Dimensions of raster coordinates (2D or 3D)
#' * [zExt()]: Vertical extent
#' 
#' # Functions that operate on **GRASS** rasters
#' * [as.contour()]: Contour lines from a `GRaster`
#'
#' # Properties of **GRASS** vectors
#' * [ext()]: Spatial extent
#' * [topology()]: Dimensions of vector coordinates (2D or 3D)
#' * [zExt()]: Vertical extent
#' 
#' # Functions that operate on **GRASS** vectors
#'
#'
#'
#' # Converting between data types
#' * [as.points()], [as.lines()], and [as.polygons()]: Convert a `GRaster` to a `GVector`
#' * [rasterize()]: Convert a `GVector` to a `GRaster`
#' * [fast()]: Create `GRaster`s or `GVector`s from `SpatRaster`s, `SpatVector`s, or `sf` or `stars` objects, or from files
#' * [rast()]: Convert a `GRaster` to a `SpatRaster`
#' * [vect()]: Convert a `GVector` to a `SpatVector`
#' * [st_as_sf()]: Convert a `GVector` to a `sf` vectors
#' * [stars()]: Convert a `GVector` to a `stars` raster
#' 
#' # General purpose functions
#' * [compareFloat()]: Compare values accounting for differences due to floating point precision
#' * [fastRestore()]: Restore a previous **GRASS** session or switch **GRASS** locations/mapsets.
#'
#' # Functions that operate on **GRASS** "regions" (seldom used by most users):
#' * [regionDim()]: Change or reprot the active region's resolution
#' * [regionExt()]: Change or report the active region's extent
#' * [regionRes()]: Change or reprot the active region's dimensions
#' * [regionReshape()]: Change or report the active region's extent and resolution
#'
