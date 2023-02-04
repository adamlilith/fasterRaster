#' @title fasterRaster: Faster raster processing using GRASS GIS
#'
#' @description This package is meant as an add-on to the \pkg{terra} package, and to a lesser extent the \pkg{sf} package. It uses \href{https://grass.osgeo.org/}{\code{GRASS GIS}} version 8+ to do the more laborious operations. For rasters and vectors that are small in memory, functions in the \pkg{terra} or \pkg{sf} packages will likely be faster. To use many of the functions in this package you must have the stand-alone version of \code{GRASS} 8+ installed on your system (i.e., do not use the \code{OSGeoW} installer to install \code{GRASS}). \cr\cr
#' \pkg{fasterRaster} makes heavy use of the \pkg{rgrass} package by Roger Bivand and others, the \pkg{terra} package by Robert Hijmans, the \pkg{sf} package by Edzer Pebesma and others, and of course \code{GRASS GIS}, so is greatly indebted to all of these creators.\cr
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
#' \code{grassDir <- 'C:/Program Files/GRASS GIS 8.2' # for Windows} \cr
#' \code{grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac } \cr
#' \code{grassDir <- '/usr/local/grass' # Linux... maybe} \cr
#'
#' Now, let's load a raster and spatial vector to use in the examples: \cr\cr
#' \code{madElev <- fasterData('madElev') # elevation raster} \cr
#' \code{madRivers <- fasterData('madRivers') # spatial vector of rivers} \cr
#'
#' To call most \pkg{fasterRaster} functions, you will need to supply the install path to \code{GRASS} using the \code{grassDir} argument. In this example, we are calculating the distance from rivers then getting the quantiles of these distances. \cr\cr
#' \code{distToRiver <- fasterVectToRastDistance(madElev, madRivers, grassDir = grassDir)} \cr
#' \code{quants <- fasterQuantile(distToRiver, probs = c(0.05, 0.5, 0.95), grassDir = grassDir)} \cr
#' \code{quants} \cr
#'
#' Supplying the \code{grassDir} argument every time can get somewhat tedious. Instead, you can provide \code{grassDir} to all functions that need it by setting it as an option: \cr\cr
#' \code{fasterSetOptions(grassDir = 'C:/Program Files/GRASS GIS 8.2') # for Windows} \cr
#' \code{fasterSetOptions(grassDir = "/Applications/GRASS-8.2.app/Contents/Resources") # for a Mac} \cr
#' \code{fasterSetOptions(grassDir = '/usr/local/grass') # Linux... maybe} \cr
#'
#' Now, you can simply do: \cr\cr
#' \code{distToRiver <- fasterVectToRastDistance(madElev, madRivers)} \cr
#' \code{quants <- fasterQuantile(distToRiver, probs = c(0.05, 0.5, 0.95))} \cr
#' \code{quants} \cr
#'
#' @section "\link{chaining}{Chaining}" \pkg{fasterRaster} functions for speed:
#' Under the hood, many of the \code{fasterRaster} functions are: \cr\cr
#' 1. Exporting a \code{GRASS} session; \cr
#' 2. Writing a raster and/or vector to that session; \cr
#' 3. Doing the requested operation; and then \cr
#' 4. Importing the raster or vector output to \code{R}. \cr
#'
#' Note that you really probably are not interested in the export/import steps (1 and 4), but they create a computing overhead and can make \pkg{fasterRaster} functions very slow. However, it is possible to "chain" \pkg{fasterRaster} functions together so that do not need to export/import every time. Rather, you can use the output of  previous functions as input to subsequent functions, all the while keeping the rasters or vectors of interest in \code{GRASS} and not re-exporting/importing them each time. You can then import the final output back to \code{R} when you need it after all the chained functions are done.\cr
#'
#' To chain \pkg{fasterRaster} functions together you need to:
#'	1. Supply the requisite raster(s) and/or vector(s) to the first function. However, in this first function's arguments, set \code{grassToR} equal to \code{FALSE} (it is \code{TRUE} by default). This keeps the function from exporting the output back to \code{R}. If you want, specify the name of the output in \code{GRASS} using the argument \code{outGrassName} argument, or use the default value.\cr
#'  2. In the subsequent functions, use the name given by \code{outGrassName} from the previous function in the chain as the input. Note that you supply the name of the raster or vector as a character because it is already in \code{GRASS}.\cr
#'  3. Repeat step 2 for as many functions as you need, reusing any raster/vectors that were imported or created in a \pkg{fasterRaster} function. In each function, set \code{grassToR} to \code{FALSE}. If you need other rasters or vectors in the \code{GRASS} session, you can export them from \code{R} or load them from disk directly into \code{GRASS} using \code{\link{fasterRast}} or \code{\link{fasterVect}}.\cr
#'  4. Set \code{grassToR} to \code{TRUE} in the last function in the series to get the function to return the output to \code{R}. Alternatively, save the output directly from \code{GRASS} to disk using \code{\link{fasterWriteRaster}} or \code{\link{fasterWriteVector}}.
#'
#' Here is an example in which we will chain the \code{fasterVectToRastDistance} and \code{fasterQuantile} functions together: \cr
#'
#' \code{fasterVectToRastDistance(madElev, madRivers, outGrassName='distToVect', grassDir=grassDir, grassToR=FALSE)} \cr
#' \code{quants <- fasterQuantile('distToVect', probs=c(0.05, 0.5, 0.95), grassDir=grassDir)} \cr
#' \code{quants} \cr
#'
#' That is how to make \code{fasterRaster} \emph{faster!} You can chain using any raster/vector that was created or imported into \code{GRASS} by any previous function (not necessarily the immediately previous function). Just so long as there is a raster/vector in \code{GRASS} with the given name, you can use it in subsequent functions without paying the price of importing it. You can see a lost of rasters and vectors in the \code{GRASS} session using \code{\link{fasterls}}.\cr
#'
#' Note that you can set the argument \code{grassToR} for all functions automatically using \code{fasterSetOptions}, so you don't need to specify it every time. This is helpful is you are chaining a lot of functions. In the last function, \emph{do} use \code{grassToR = TRUE} to get the outpt (defining it in a function call overrides the global option for tha function).
#'
#' @section The generic \code{faster()} function:
#' The \code{faster} function is wrapper for \code{\link[rgrass]{execGRASS}} i the \pkg{rgrass} package, plus code necessary to initiate a \code{GRASS} session and export a raster and/or vector to it to act as input. Many of the functions in \pkg{fasterRaster} actually utilize \code{faster} under the hood.  This function works best for modules that take one raster and/or one vector as input and produce one raster or vector as output. Here is an example: \cr
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
#' @section Saving rasters:
#'
#' For some reason, if you create or export a raster to \code{GRASS}, then import it back to \code{R}, saving it using \pkg{terra}'s \code{\link[terra]{writeRaster}} function automatically forces the \code{datatype} of the raster to a low-bit integer. As a result, values are truncated to integers and forced to a specific range (or coerced to \code{NA}). You can overcome this simply by saving these rasters using, for example:\cr
#'
#' \code{writeRaster(rasterToSave, 'C:/pathToSave/fileName.tif', datatype='FLT4S')}\cr
#' \code{writeRaster(rasterToSave, 'C:/pathToSave/fileName.tif', datatype='FLT8S')}\cr
#'
#' Here, the important part is the \code{datatype} argument, which in these two examples says to save the raster values as a 32-bit or 64-bit floating point values that are signed (can be negative or positive). Alternatively, you can also use these functions as shorthand to save in 32- or 64-bit signed, floating format:\cr
#'
#' \code{writeRaster4(rasterToSave, 'C:/pathToSaveTo/fileName.tif')}\cr
#' \code{writeRaster8(rasterToSave, 'C:/pathToSaveTo/fileName.tif')}\cr
#'
#' @section How to get help:
#' You can type \code{?fasterRaster} to see this page. To look up functions by their \pkg{fasterRaster}, \pkg{terra}, \pkg{sf}, or \code{GRASS} equivalents, you can use \code{\link{fasterHelp}}. Examples:\cr\cr
#'		\code{fasterHelp('fasterExt') # lookup fasterRaster function name (default)} \cr
#'		\code{fasterHelp('ext', 'terra') # lookup terra function name} \cr
#'		\code{fasterHelp('st_bbox', 'sf') # lookup sf function name} \cr
#'		\code{fasterHelp('g.region', 'GRASS') # lookup GRASS function name} \cr
#'		\code{fasterHelp('g.r', 'GRASS', approx=TRUE) # lookup GRASS function name with fuzzy matching} \cr
#'		\code{fasterHelp() # list of all functions} \cr
#'
#' Most functions are named as \code{faster}\emph{XYZ}, where \emph{XYZ} is the \pkg{terra} equivalent function, if there is any. Note though, that functions may work differently than they do in \pkg{terra}. Also see the \code{\link{setGrassVer}} function.
#'
#' @section Defining arguments for multiple functions at once:
#' Across most most \pkg{fasterRaster} functions there are several common arguments. These can be specified when you call the function or set for all functions at once using \code{\link{fasterSetOptions}}. You can see the values of these arguments using \code{\link{fasterGetOptions}}. They include:
#' \itemize{
#'		\item \code{grassDir}: The install directory of \code{GRASS} on your computer.
#'		\item \code{replace}: Whether or not to allow overwriting existing rasters or vectors in a \code{GRASS} session.
#'		\item \code{grassToR}: If \code{TRUE} (the default for all functions), then the function will always import a raster or vector it created or modified back to \code{R}. This is a time-consuming operation that is not necessary if you are \link{chaining} functions in \code{GRASS}.
#'		\item \code{outVectClass}: By default, functions that output a spatial vector will return \code{SpatVector} objects, but they can be made to return \code{sf} objects instead.
#'		\item \code{grassVer}: Sets the version of \code{GRASS} so that URLs in \code{\link{fasterHelp}} will be valid for your version of \code{GRASS}.
#' }
#' See \code{\link{fasterSetOptions}} for instructions on how to set each of these arguments for all functions at once.
#'
#' @section Functions that do operations on rasters:
#' 		\code{\link{fasterApp}}: Apply user-defined function to one or more rasters (using \code{GRASS}; see also \code{fasterFocal}).\cr
#' 		\code{\link{fasterBufferRast}}: Add buffer to cells in a raster.\cr
#' 		\code{\link{fasterContour}}: Calculate contour vectors from a raster.\cr
#' 		\code{\link{fasterConvertDegree}}: Convert degrees from \code{GRASS} format (0 = east, 90 = north) to standard (0 = north, 90 = east).\cr
#' 		\code{\link{fasterCropRast}}: Crop a raster to the region of overlap with another raster or vector.\cr
#' 		\code{\link{fasterDelaunay}}: Delauney triangulation for points.\cr
#' 		\code{\link{fasterDim}}: Number of rows and columns of a raster.\cr
#' 		\code{\link{fasterFocal}}: Calculations on neighboring cells of a raster.\cr
#' 		\code{\link{fasterFocalCircle}}: Calculations on cells in a circular neighborhood on a raster.\cr
#' 		\code{\link{fasterFocalDecay}}: Calculations on neighboring cells with exponential or Gaussian weights.\cr
#' 		\code{\link{fasterFragmentation}}: Landscape fragmentation indices following Riitters et al. (2000 \emph{Conservation Ecology} 4:3; using multi-core).\cr
#' 		\code{\link{fasterGlobal}}: Statistics summarizing all values of a raster.\cr
#' 		\code{\link{fasterHorizon}}: Horizon angle height from an elevation raster.\cr
#' 		\code{\link{fasterLongLatRasts}}: Create rasters with values equal to cell longitude and latitude.\cr
#' 		\code{\link{fasterMakeMask}}: Create a mask for affecting subsequent raster operations.\cr
#' 		\code{\link{fasterMask}}: Mask values in a raster using another raster or vector.\cr
#' 		\code{\link{fasterMosaic}}: Combine rasters that do not perfectly overlap.\cr
#' 		\code{\link{fasterNcell}}: Number of cells of a raster.\cr
#' 		\code{\link{fasterNcol}}: Number of columns of a raster.\cr
#' 		\code{\link{fasterNrow}}: Number of rows of a raster.\cr
#' 		\code{\link{fasterProjectRast}}: Project and resample a raster.\cr
#' 		\code{\link{fasterQuantile}}: Quantiles of values in a raster.\cr
#' 		\code{\link{fasterRastDistance}}: Distance from cells with \code{NA}s to closest non-\code{NA} cell (or the inverse of this).\cr
#' 		\code{\link{fasterRes}}: Spatial resolution of a raster.\cr
#' 		\code{\link{fasterSun}}: Solar irradiance and radiance.\cr
#' 		\code{\link{fasterFractalRast}}: Generate a raster with a fractal pattern.\cr
#' 		\code{\link{fasterTerrain}}: Slope, aspect, and curvature.\cr
#' 		\code{\link{fasterTWI}}: Topographic wetness index.\cr
#' 		\code{\link{fasterTrimRast}}: Remove all rows and columns of a raster that are entirely NA.\cr
#' 		\code{\link{fasterVectorize}}: Convert raster to spatial points, lines, or polygons.\cr
#' 		\code{\link{rastToR}}: Get a rasters from \code{GRASS}.\cr

#' @section Functions that do operations on vectors:
#' 		\code{\link{fasterBufferVect}}: Buffer a spatial vector.\cr
#' 		\code{\link{fasterConvHull}}: Minimum convex hull around a spatial vector.\cr
#' 		\code{\link{fasterInfoVect}}: Information on a vector in a \code{GRASS} session.\cr
#' 		\code{\link{fasterProjectVect}}: Project a spatial vector.\cr
#' 		\code{\link{fasterRasterize}}: Convert vector to a raster.\cr
#' 		\code{\link{fasterVoronoi}}: Voronoi diagrams for points or polygons.\cr
#' 		\code{\link{fasterWriteVector}}: Save one or more vectors to disk directly from a \code{GRASS} session.\cr
#' 		\code{\link{vectToR}}: Get  vector from \code{GRASS}.\cr
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
#' 		\code{\link{initGrass}}: Initialize a \code{GRASS} session using a raster or a vector as a template.\cr
#' 		\code{\link{writeRaster4}} and \code{\link{writeRaster8}}: Save a raster exported from \code{GRASS} to disk.\cr
#'
#' @section Functions affecting \code{GRASS} \link{region}s (rarely used by most users):
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
