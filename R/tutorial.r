#' @name tutorial
#'
#' @title Tutorials on getting started, saving rasters, and chaining functions
#'
#' @description
#' This is a brief tutorial on how to get started in \pkg{fasterRaster}. First, let's load the requisite packages: \cr\cr
#' \code{library(fasterRaster)} \cr
#' \code{library(terra) # for rasters} \cr
#' \code{library(sf) # for spatial vectors (can also use terra's "SpatVector" class)} \cr
#'
#' You will need to have \code{GRASS} version 8 or above installed on your system and know the path where it is installed. Depending on your system, this may look something like: \cr\cr
#' \code{grassDir <- 'C:/Program Files/GRASS GIS 8.2' # for Windows} \cr
#' \code{grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac... note the double quotes } \cr
#' \code{grassDir <- '/usr/local/grass' # Linux} \cr
#'
#' Now, let's load a raster and spatial vector to use in the examples: \cr\cr
#' \code{madElev <- fasterData('madElev') # elevation raster} \cr
#' \code{madRivers <- fasterData('madRivers') # spatial vector of rivers} \cr
#'
#' To call most \pkg{fasterRaster} functions, you will need to supply the install path to \code{GRASS} using the \code{grassDir} argument. In this example, we are calculating the distance from rivers then getting the quantiles of these distances. \cr
#'
#' \code{distToRiver <- fasterVectToRastDistance(madElev, madRivers, grassDir = grassDir)} \cr
#' \code{quants <- fasterQuantile(distToRiver, probs = c(0.05, 0.5, 0.95), grassDir = grassDir)} \cr
#' \code{quants} \cr
#'
#' Supplying the \code{grassDir} argument every time can get somewhat tedious. Instead, you can provide \code{grassDir} to all functions that need it by setting it as an option: \cr
#'
#' \code{fasterSetOptions(grassDir = 'C:/Program Files/GRASS GIS 8.2') # for Windows} \cr
#' \code{fasterSetOptions(grassDir = "/Applications/GRASS-8.2.app/Contents/Resources") # for a Mac} \cr
#' \code{fasterSetOptions(grassDir = '/usr/local/grass') # Linux... maybe} \cr
#'
#' Now, you can simply do: \cr\cr
#' \code{distToRiver <- fasterVectToRastDistance(madElev, madRivers)} \cr
#' \code{quants <- fasterQuantile(distToRiver, probs = c(0.05, 0.5, 0.95))} \cr
#' \code{quants} \cr
#'
#' @section Defining arguments for multiple functions at once:
#' Across most most \pkg{fasterRaster} functions there are several common arguments. These can be specified when you call the function or set for all functions at once using \code{\link{fasterSetOptions}}. You can see the values of these arguments using \code{\link{fasterGetOptions}}. They include:
#' \itemize{
#'		\item \code{grassDir}: The install directory of \code{GRASS} on your computer.
#'		\item \code{cores}: Number of processor cores to use on processor-intensive functions. Default is 1.
#'		\item \code{memory}: Memory to allocate to memory-intensive tasks, in MB. Default is 300 MB.
#'		\item \code{replace}: Whether or not to allow overwriting existing rasters or vectors in a \code{GRASS} session. Default is \code{FALSE}.
#'		\item \code{grassToR}: If \code{TRUE} (the default for all functions), then the function will always import a raster or vector it created or modified back to \code{R}. This is a time-consuming operation that is not necessary if you are \link{chaining} functions in \code{GRASS}. The default is \code{TRUE} (i.e., move all products back to \code{R}).
#'		\item \code{outVectClass}: By default, functions that output a spatial vector will return \code{SpatVector} objects, but they can be made to return \code{sf} objects instead.
#'		\item \code{trimRast}: Determines if rasters imported back to \code{R} have all rows and columns that are entirely \code{NA} removed first (default is \code{TRUE}).
#'		\item \code{grassVer}: Sets the version of \code{GRASS} so that URLs in \code{\link{fasterHelp}} will be valid for your version of \code{GRASS}.
#' }
#' See \code{\link{fasterSetOptions}} for instructions on how to set each of these arguments for all functions at once.
#'
#' @section "Chaining" \pkg{fasterRaster} functions for speed
#'
#' Under the hood, many of the \code{fasterRaster} functions are: \cr
#'
#' 1. Exporting a \code{GRASS} session; \cr
#' 2. Writing a raster and/or vector to that session; \cr
#' 3. Doing the requested operation; and then \cr
#' 4. Importing the raster or vector output to \code{R}. \cr
#'
#' Note that you really probably are not interested in the export/import steps (1 and 4), but they create a lot of computing overhead and can make \pkg{fasterRaster} functions very slow. However, it is possible to "chain" \pkg{fasterRaster} functions together so that do not need to export/import every time. Rather, you can use the output of  previous functions as input to subsequent functions, all the while keeping the rasters or vectors of interest in \code{GRASS} and not re-exporting/importing them each time. You can then import the final output back to \code{R} when you need it after all the chained functions are done.\cr
#'
#' To chain \pkg{fasterRaster} functions together you need to:\cr
#'
#'	1. Supply the requisite raster(s) and/or vector(s) to the first function. However, in this first function's arguments, set \code{grassToR} equal to \code{FALSE} (it is \code{TRUE} by default). This keeps the function from exporting the output back to \code{R}. If you want, specify the name of the output in \code{GRASS} using the argument \code{outGrassName} argument, or use the default value.\cr
#'  2. In the subsequent functions, use the name given by \code{outGrassName} from the previous function in the chain as the input. Note that you supply the name of the raster or vector as a character because it is already in \code{GRASS}.\cr
#'  3. Repeat step 2 for as many functions as you need, reusing any raster/vectors that were imported or created in a \pkg{fasterRaster} function. In each function, set \code{grassToR} to \code{FALSE}. If you need other rasters or vectors in the \code{GRASS} session, you can export them from \code{R} or load them from disk directly into \code{GRASS} using \code{\link{fasterRast}} or \code{\link{fasterVect}}.\cr
#'  4. Set \code{grassToR} to \code{TRUE} in the last function in the series to get the function to return the output to \code{R}. Alternatively, save the output directly from \code{GRASS} to disk using \code{\link{fasterWriteRaster}} or \code{\link{fasterWriteVector}}.\cr
#'
#' Here is an example in which we will chain the \code{fasterVectToRastDistance} and \code{fasterQuantile} functions together: \cr
#'
#' \code{fasterVectToRastDistance(madElev, madRivers, outGrassName='distToVect', grassDir=grassDir, grassToR=FALSE)} \cr
#' \code{quants <- fasterQuantile('distToVect', probs=c(0.05, 0.5, 0.95), grassDir=grassDir)} \cr
#' \code{quants} \cr
#'
#' That is how to make \code{fasterRaster} \emph{faster!} You can chain using any raster/vector that was created or imported into \code{GRASS} by any previous function (not necessarily the immediately previous function). Just so long as there is a raster/vector in \code{GRASS} with the given name, you can use it in subsequent functions without paying the price of importing it. You can see a lost of rasters and vectors in the \code{GRASS} session using \code{\link{fasterls}}.\cr
#'
#' Note that you can set the argument \code{grassToR} for all functions automatically using \code{fasterSetOptions}, so you don't need to specify it every time. This is helpful is you are chaining a lot of functions. In the last function in a series of chained functions, \emph{do} use \code{grassToR = TRUE} to get the output (defining it in a function call overrides the global option for that function).
#'
#' @section Saving rasters and vectors
#' 
#' \subsection{Saving rasters and vectoes directly from \code{GRASS}}:
#' Saving rasters or vectors directly from \code{GRASS} onto disk is much faster than importing them into \code{R}. To save a raster or vector from \code{GRASS}, simply use:\cr
#'
#' \code{fasterWriteRaster('rastName', 'filename')}\cr
#' \code{fasterWriteVector('vectName', 'filename')}\cr
#' 
#' Note that for rasters, the \code{datatype} argument can be important for reducing file size. Please see the help for \code{\link{fasterWriteRaster}}.\cr
#'
#' \subsection{Importing a raster or vector from \code{GRASS} to \code{R}}
#' To import a raster or vector to \code{R}, simply use:\cr
#'
#' \code{raster <- rastFromGrass('rastName')}\cr
#' \code{vector <- vectFromGrass('vectName')}\cr
#'
#' \subsection{Saving a vector in \code{R} that has been imported from \code{GRASS}}
#' Saving a vector that has been imported from \code{GRASS} can be done the normal way you would save a vector in \code{R}: use \code{\link[terra]{writeVector}} (\pkg{terra} package), \code{\link{st_write}} (\pkg{sf} package), or the generic \code{\link{save}} or \code{\link[base]{saveRDS}} functions (see also the \code{\link[terra]{saveRDS}} function in \pkg{terra}).\cr
#'
#' \subsection{Saving a raster in \code{R} that has been imported from \code{GRASS}}:
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
#' @section The generic \code{faster()} function:
#' The \code{\link{faster}} function is wrapper for \code{\link[rgrass]{execGRASS}} from the \pkg{rgrass} package, plus code necessary to initiate a \code{GRASS} session and export a raster and/or vector to it to act as input.This function works best for modules that take one raster and/or one vector as input and produce one raster or one vector as output. Here is an example: \cr
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
#' @keywords tutorial
NULL
