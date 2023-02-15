#' @title fasterRaster: Faster raster processing using GRASS GIS
#'
#' @description This package is meant as an add-on to the \pkg{terra} and \pkg{sf} packages. It uses \href{https://grass.osgeo.org/}{\code{GRASS GIS}} version 8+ to do the more laborious operations. For rasters and vectors that are small-to-moderate in size, functions in the \pkg{terra} or \pkg{sf} packages will almost always be faster. To use many of the functions in this package you must have the stand-alone version of \code{GRASS} 8+ installed on your system (i.e., do not use the \code{OSGeoW} installer to install \code{GRASS}). \cr
#'
#' \pkg{fasterRaster} makes heavy use of the \pkg{rgrass} package by Roger Bivand and others, the \pkg{terra} package by Robert Hijmans, the \pkg{sf} package by Edzer Pebesma and others, and of course \code{GRASS GIS}, so is greatly indebted to all of these creators.\cr
#'
#' If you find an error, please create an issue on \href{https://github.com/adamlilith/fasterRaster/issues}{GitHub}.
#'
#' @section How to get help:
#' You can type \code{?fasterRaster} to see this page. To look up functions by their \pkg{fasterRaster}, \pkg{terra}, \pkg{sf}, or \code{GRASS} equivalents, you can use \code{\link{fasterHelp}}. Examples:\cr
#'
#'		\code{fasterHelp('fasterExt') # lookup by fasterRaster function name (default)} \cr
#'		\code{fasterHelp('ext', 'terra') # lookup terra function name} \cr
#'		\code{fasterHelp('st_bbox', 'sf') # lookup sf function name} \cr
#'		\code{fasterHelp('g.region', 'GRASS') # lookup by GRASS module name} \cr
#'		\code{fasterHelp('g.r', 'GRASS', approx=TRUE) # lookup GRASS function name with fuzzy matching} \cr
#'		\code{fasterHelp() # list of all functions} \cr
#'
#' Most functions are named as \code{faster}\emph{XYZ}, where \emph{XYZ} is the \pkg{terra} equivalent function, if there is any. Note though, that functions may work differently than they do in \pkg{terra}. Also see the \code{\link{setGrassVer}} function.
#'
#' @section Functions that do operations on rasters:
#' 		\code{\link{fasterApp}}: Apply user-defined function to one or more rasters (using \code{GRASS}; see also \code{fasterFocal}).\cr
#' 		\code{\link{fasterAsPoints}}: Convert raster to a points vector.\cr
#' 		\code{\link{fasterAsLines}}: Convert raster to a lines vector.\cr
#' 		\code{\link{fasterAsPolygons}}: Convert raster to a polygons vector.\cr
#' 		\code{\link{fasterBufferRast}}: Add buffer to cells in a raster.\cr
#' 		\code{\link{fasterContour}}: Calculate contour vectors from a raster.\cr
#' 		\code{\link{fasterConvertDegree}}: Convert degrees from \code{GRASS} format (0 = east, 90 = north) to standard (0 = north, 90 = east).\cr
#' 		\code{\link{fasterCropRast}}: Crop a raster to the region of overlap with another raster or vector.\cr
#' 		\code{\link{fasterDelaunay}}: Delaunay triangulation for points.\cr
#' 		\code{\link{fasterDim}}: Number of rows and columns of a raster.\cr
#' 		\code{\link{fasterDistanceRast}}: Distance from cells with \code{NA}s to closest non-\code{NA} cell (or the inverse of this).\cr
#' 		\code{\link{fasterFocal}}: Calculations on neighboring cells of a raster.\cr
#' 		\code{\link{fasterFragmentation}}: Landscape fragmentation indices following Riitters et al. (2000 \emph{Conservation Ecology} 4:3; using multi-core).\cr
#' 		\code{\link{fasterGlobal}}: Statistics summarizing all values of a raster.\cr
#' 		\code{\link{fasterHorizon}}: Horizon angle height from an elevation raster.\cr
#' 		\code{\link{fasterInfo}}: Information on rasters and/or vectors.\cr
#' 		\code{\link{fasterLongLatRasts}}: Create rasters with values equal to cell longitude and latitude.\cr
#' 		\code{\link{fasterMakeMask}}: Create a mask for affecting subsequent raster operations.\cr
#' 		\code{\link{fasterMaskRast}}: Mask values in a raster using another raster or vector.\cr
#' 		\code{\link{fasterMosaic}}: Combine rasters that do not perfectly overlap.\cr
#' 		\code{\link{fasterNcell}}: Number of cells of a raster.\cr
#' 		\code{\link{fasterNcol}}: Number of columns of a raster.\cr
#' 		\code{\link{fasterNrow}}: Number of rows of a raster.\cr
#' 		\code{\link{fasterProjectRast}}: Project and resample a raster.\cr
#' 		\code{\link{fasterQuantile}}: Quantiles of values in a raster.\cr
#' 		\code{\link{fasterRes}}: Spatial resolution of a raster.\cr
#' 		\code{\link{fasterSun}}: Solar irradiance and radiance.\cr
#' 		\code{\link{fasterFractalRast}}: Generate a raster with a fractal pattern.\cr
#' 		\code{\link{fasterTerrain}}: Slope, aspect, and curvature.\cr
#' 		\code{\link{fasterTWI}}: Topographic wetness index.\cr
#' 		\code{\link{fasterTrimRast}}: Remove all rows and columns of a raster that are entirely NA.\cr
#' 		\code{\link{rastfromGrass}}: Get a rasters from \code{GRASS}.\cr

#' @section Functions that do operations on vectors:
#' 		\code{\link{fasterBufferVect}}: Buffer a spatial vector.\cr
#' 		\code{\link{fasterConvHull}}: Minimum convex hull around a spatial vector.\cr
#' 		\code{\link{fasterInfo}}: Information on rasters and/or vectors.\cr
#' 		\code{\link{fasterProjectVect}}: Project a spatial vector.\cr
#' 		\code{\link{fasterRasterize}}: Convert vector to a raster.\cr
#' 		\code{\link{fasterVoronoi}}: Voronoi diagrams for points or polygons.\cr
#' 		\code{\link{fasterWriteVector}}: Save one or more vectors to disk directly from a \code{GRASS} session.\cr
#' 		\code{\link{vectFromGrass}}: Get a spatial vector from \code{GRASS}.\cr
#' 		\code{\link{vectTopo}}: Get topological type (point, line, polygon) for one or more spatial vectors.\cr
#'
#' @section Functions that do operations on rasters and vectors simultaneously:
#' 		\code{\link{faster}}: Generic call to a \code{GRASS} module.\cr
#' 		\code{\link{fasterVectToRastDistance}}: Distance between raster cells and a vector.\cr
#'
#' @section Generic "faster" operations:
#' 		\code{\link{faster}}: Generic call to a \code{GRASS} module.\cr
#'
#' @section Utility functions:
#' 		\code{\link{compareFloat}}: Compare values accounting for differences due to floating point precision.\cr
#' 		\code{\link{fasterCopy}}: Make a copy of a raster or vector.\cr
#' 		\code{\link{fasterGetOptions}}: Get current values of arguments shared across \pkg{fasterRaster} functions.\cr
#' 		\code{\link{fasterCRS}}: Coordinate reference system of the active \code{GRASS} session.\cr
#' 		\code{\link{fasterExt}}: Spatial extent of a raster or vector.\cr
#' 		\code{\link{fasterExists}}: Is there a raster or vector of the given name in the active \code{GRASS} session?\cr
#' 		\code{\link{fasterInfo}}: Information on one or more rasters and/or vectors.\cr
#' 		\code{\link{fasterLs}}: Names of all rasters and/or vectors in the active \code{GRASS} session.\cr
#' 		\code{\link{fasterRast}}: Export raster from \code{R} or disk to an active \code{GRASS} session.\cr
#' 		\code{\link{fasterRm}}: Remove raster(s) and/or vector(s) from a \code{GRASS} session.\cr
#' 		\code{\link{fasterSetOptions}}: Set values of arguments shared across \pkg{fasterRaster} functions.\cr
#' 		\code{\link{fasterVect}}: Export vector from \code{R} or disk to an active \code{GRASS} session.\cr
#' 		\code{\link{importFromGrass}}: Import a raster or vector from a GRASS session into \code{R}.\cr
#' 		\code{\link{startFaster}}: Initialize a \code{GRASS} session using a raster or a vector as a template.\cr
#' 		\code{\link{writeRaster4}} and \code{\link{writeRaster8}}: Save a raster exported from \code{GRASS} to disk.\cr
#'
#' @section Functions affecting \code{GRASS} \link{region}s (dangerous! rarely used...):
#' 		\code{\link{regionDim}}: Get or set number of rows and columns of a region.\cr
#' 		\code{\link{regionExt}}: Get or set spatial extent of a region.\cr
#' 		\code{\link{regionFit}}: Simultaneously change resolution and extent of a GRASS region.\cr
#' 		\code{\link{regionRes}}: Get or set spatial resolution of a region.\cr
#'
#' @section Data:
#'		\code{\link{fasterData}}: Load any of the example rasters or spatial vectors.\cr
#'		\code{\link{madCoast0}}: Borders of Madagascar (spatial polygon).\cr
#'		\code{\link{madCoast4}}: Borders of selected districts of Madagascar (spatial polygons).\cr
#'		\code{\link{madChelsa}}: Bioclimatic rasters for a portion of Madagascar (raster "stack").\cr
#'		\code{\link{madDypsis}}: Records of the genus \emph{Dypsis} (spatial points).\cr
#'		\code{\link{madElev}}: Elevation raster for a portion of Madagascar.\cr
#'		\code{\link{madElevAnt}}: Elevation raster for the Antanambe Commune of Madagascar.\cr
#'		\code{\link{madElevMan}}: Elevation raster for the Manompana Commune of Madagascar.\cr
#'		\code{\link{madForest2000}}: Raster of occurrence/non-occurrence of forest in Madagascar.\cr
#'		\code{\link{madRivers}}: Major rivers of an eastern portion of Madagascar (spatial lines).\cr
#'
#' @docType package
#' @author Adam B. Smith
#' @name fasterRaster
NULL
