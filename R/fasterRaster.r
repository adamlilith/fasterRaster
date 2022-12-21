#' @title fasterRaster: Faster raster processing using GRASS GIS
#'
#' @description This package is meant as an add-on to the \pkg{terra} package, and to a lesser extent the \pkg{sf} package. It uses \href{https://grass.osgeo.org/}{\code{GRASS GIS}} version 8+ to do some of the more laborious operations. For rasters and vectors that are small in memory, functions in the \pkg{terra} or \pkg{sf} packages will likely be faster. To use many of the functions in this package you must have the stand-alone version of \code{GRASS} 8+ installed on your system (i.e., do not use the \code{OSGeoW} installer to install \code{GRASS}). \cr
#'
#' If you find an error, please create an issue on \href{https://github.com/adamlilith/fasterRaster/issues}{GitHub}.
#'
#' @section Getting started:
#'
#' This is a brief tutorial on how to get started in \pkg{fasterRaster}. First, let's load the requisite packages: \cr\cr
#' \code{library(fasterRaster)} \cr
#' \code{library(terra) # for rasters} \cr
#' \code{library(sf) # for spatial vectors (can also use terra's "SpatVector" class)} \cr
#'
#' You will need to have \code{GRASS} 8+ installed on your system and know the path where it is installed. Depending on your system, this may look something like: \cr\cr
#' \code{grassDir <- 'C:/Program Files/GRASS GIS 8.2' # for a PC} \cr
#' \code{grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac } \cr
#' \code{grassDir <- '/usr/local/grass' # Linux... maybe} \cr
#'
#' Now, load a raster and spatial vector to use in the examples: \cr\cr
#' \code{madElev <- fasterData('madElev') # elevation raster} \cr
#' \code{madRivers <- fasterData('madRivers') # spatial vector of rivers} \cr
#'
#' To call most \pkg{fasterRaster} functions, you will need to supply the install path to \code{GRASS} using the \code{grassDir} argument. In this example, we are calculating the distance from rivers then getting the quantiles of these distances. \cr\cr
#' \code{distToRiver <- fasterVectToRastDistance(madElev, madRivers, grassDir = grassDir)} \cr
#' \code{quants <- fasterQuantile(distToRiver, probs=c(0.05, 0.5, 0.95), grassDir = grassDir)} \cr
#' \code{quants} \cr
#'
#' Supplying the \code{grassDir} argument every time can get somewhat tedious. Instead, you can provide \code{grassDir} to all functions that need it by setting it as an option: \cr\cr
#' \code{options(grassDir = grassDir)} \cr
#'
#' Now, you can simply do: \cr\cr
#' \code{distToRiver <- fasterVectToRastDistance(madElev, madRivers)} \cr
#' \code{quants <- fasterQuantile(distToRiver, probs=c(0.05, 0.5, 0.95))} \cr
#' \code{quants} \cr
#'
#' @section "Chaining" fasterRaster functions for speed:
#' Under the hood, many of the \code{fasterRaster} functions are: \cr\cr
#' 1) starting a \code{GRASS} session; \cr
#' 2) writing a raster and/or vector to that session; \cr
#' 3) doing the requested operation; and then \cr
#' 4) exporting the raster/vector output to \code{R}. \cr
#'
#' Note that you really probably are not interested in the import/export steps (numbers 1 and 4), but they create a computing overhead. However, it is possible to "chain" \pkg{fasterRaster} functions together so that do not need to import/export every time. Rather, you can use the output of  previous functions as input to subsequent functions, all the while keeping the rasters or vectors of interest in \code{GRASS} and not re-importing/exporting them each time. You can then export the final output when you need it after all the chained functions are done.\cr
#'
#' To chain \pkg{fasterRaster} functions together you need to:
#' \itemize{
#'		\item Supply the requisite raster(s) and/or vector(s) to the first function, but in its arguments, set \code{grassToR} equal to \code{FALSE} (it is \code{TRUE} by default). This keeps the function from exporting the output back to \code{R}. If you want, specify the name of the output in \code{GRASS} using the argument \code{outGrassName} argument, or use the default value.
#'		\item In the subsequent functions, use the name given by \code{outGrassName} as the input. Repeat for as many functions as you need, reusing any raster/vectors that were imported or created in a \pkg{fasterRaster} function. In each, set \code{grassToR} to \code{FALSE} until the last function in the series, where it should be \code{TRUE} so you can get the output.
#' }
#'
#' Here is an example in which we will chain the \code{fasterVectToRastDistance} and \code{fasterQuantile} functions together: \cr
#'
#' \code{fasterVectToRastDistance(madElev, madRivers, outGrassName='distToVect', grassDir=grassDir, grassToR=FALSE)} \cr
#' \code{quants <- fasterQuantile('distToVect', probs=c(0.05, 0.5, 0.95), grassDir=grassDir)} \cr
#' \code{quants} \cr
#'
#' That is how to make \code{fasterRaster} \emph{faster!} You can chain any raster/vector that was created or imported into \code{GRASS} through a \pkg{fasterRaster} function that uses \code{GRASS} (most of them do). That is, you do not always need to use the value in \code{outGrassName} as the input to the next function--just so long as there is a raster/vector in \code{GRASS} with the given name, you can use it in subsuquent functions without paying the price of importing it.
#'
#' @section The generic faster() function:
#' The \code{faster} function is wrapper for \code{\link[rgrass]{execGRASS}}, plus code necessary to initiate a \code{GRASS} session. Many of the functions in \pkg{fasterRaster} actually utilize \code{faster} under the hood.  This function works best for modules that take one raster and/or one vector as input and produce one raster or vector as output. Here is an example: \cr
#'
#' \code{latRast <- faster('r.latlong', rast=madElev, outType='rast', flags=c('quiet', 'overwrite'))} \cr
#' \code{longRast <- faster('r.latlong', rast=madElev, outType='rast', flags=c('quiet', 'overwrite', 'l'))} \cr
#'
#' In this case, the above is nearly the same as: \cr\cr
#' \code{longLat <- fasterLongLatRasts(madElev)} \cr
#'
#' You can also chain the \code{faster} function: \cr
#'
#' \code{latRast <- faster('r.latlong', rast=madElev, outType='rast', flags=c('quiet', 'overwrite'), outGrassName='lat')} \cr
#' \code{longRast <- faster('r.latlong', input='lat', outType='rast', flags=c('quiet', 'overwrite', 'l'))} \cr
#'
#' Here, we used the product of the first \code{faster} call, which is named \code{'lat'}, as the input to the second \code{faster} call. Note that we could have also used \code{'madElev'} as the input to the second, since it was also already in \code{GRASS}.
#'
#' @section How to get help:
#'		You can type \code{?fasterRaster} to see this page. To look up functions by their \pkg{fasterRaster}, \code{GRASS}, or \pkg{terra} name and their equivalents in the other two, you can use \code{\link{fasterHelp}}. Examples:\cr\cr
#'		\code{fasterHelp('fasterContour')} \cr
#'		\code{fasterHelp('contour', terra = TRUE)} \cr
#'		\code{fasterHelp('r.contour', grass = TRUE)} \cr
#'
#' @section Useful options:
#' 		\code{\link{fasterOptions}}: Set the argument \code{grassDir} for all functions at once so you do not have to do it every time.\cr
#'
#' @section Functions that do operations on rasters:
#' 		\code{\link{fasterApp}}: Apply user-defined function to one or more rasters (using \code{GRASS}; see also \code{fasterFocal}).\cr
#' 		\code{\link{fasterBufferRast}}: Add buffer to cells in a raster (using \code{GRASS}).\cr
#' 		\code{\link{fasterContour}}: Calculate contour vectors from a raster.\cr
#' 		\code{\link{fasterConvertDegree}}: Convert degrees from \code{GRASS} format (0 = east, 90 = north) to standard (0 = north, 90 = east) (using \code{GRASS}).\cr
#' 		\code{\link{fasterFocal}}: Faster focal calculations (using multi-core).\cr
#' 		\code{\link{fasterFragmentation}}: Landscape fragmentation indices following Riitters et al. (2000 \emph{Conservation Ecology} 4:3; using multi-core).\cr
#' 		\code{\link{fasterHorizon}}: Horizon angle height from a DEM (using \code{GRASS}).\cr
#' 		\code{\link{fasterInfoRast}}: Information on a raster in a \code{GRASS} session.\cr
#' 		\code{\link{fasterLongLatRasts}}: Create rasters with values equal to cell longitude and latitude (using \code{GRASS}).\cr
#' 		\code{\link{fasterProjectRast}}: Project and resample raster (using \code{GRASS}).\cr
#' 		\code{\link{fasterQuantile}}: Quantiles of values in a raster (using \code{GRASS}).\cr
#' 		\code{\link{fasterRastDistance}}: Distance from cells with \code{NA}s to closest non-\code{NA} cell (or the inverse of this) (using \code{GRASS}).\cr
#' 		\code{\link{fasterSun}}: Solar irradiance and radiance (using \code{GRASS}).\cr
#' 		\code{\link{fasterSurfFractal}}: Generate a raster with a fractal pattern (using \code{GRASS}).\cr
#' 		\code{\link{fasterTerrain}}: Slope, aspect, and curvature (using \code{GRASS}).\cr
#' 		\code{\link{fasterTopidx}}: Topographic wetness index (using \code{GRASS}).\cr
#' 		\code{\link{fasterVectorize}}: Convert raster to spatial points, lines, or polygons (using \code{GRASS}).\cr

#' @section Functions that do operations on vectors:
#' 		\code{\link{fasterInfoVect}}: Information on a vector in a \code{GRASS} session.\cr
#' 		\code{\link{fasterRasterize}}: Convert vector to a raster (using \code{GRASS}).\cr
#'
#' @section Functions that do operations on rasters and vectors simultaneously:
#' 		\code{\link{fasterVectToRastDistance}}: Distance between raster cells and a vector (using \code{GRASS}).\cr
#'
#' @section Generic "faster" operations:
#' 		\code{\link{faster}}: Generic call to a \code{GRASS} module.\cr
#'
#' @section Utility functions:
#' 		\code{\link{exportRastToGrass}}: Export raster to an open \code{GRASS} session with support for large rasters/vectors.\cr
#' 		\code{\link{exportVectToGrass}}: Export vector object to an open \code{GRASS} session with support for large rasters/vectors.\cr
#' 		\code{\link{initGrass}}: Initialize a \code{GRASS} session using a raster or a vector as a template.\cr
#'
#' @section Data:
#'		\code{\link{fasterData}}: Load any of the example rasters or spatial vectors.\cr
#'		\code{\link{madCoast0}}: Borders of Madagascar.\cr
#'		\code{\link{madCoast4}}: Borders of selected districts of Madagascar.\cr
#'		\code{\link{madElev}}: Elevation raster for a portion of Madagascar.\cr
#'		\code{\link{madForest2000}}: Raster of occurrence/non-occurrence of forest in Madagascar.\cr
#'		\code{\link{madRivers}}: Major rivers of Madagascar.\cr
#'
#' @docType package
#' @author Adam B. Smith
#' @name fasterRaster
NULL
