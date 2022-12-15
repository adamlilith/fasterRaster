#' @name fasterOptions
#'
#' @title Set GRASS directory for fasterRaster functions
#' 
#' @description Most functions in \pkg{fasterRaster} use an argument named "\code{grassDir}" for specifying where \pkg{GRASS} is installed. However, defining this argument every time can be cumbersome if you are using a lot of \code{fasterRaster} functions. Instead, you can define \code{grassDir} just once using \code{\link{options}}, and subsequent calls to \pkg{fasterRaster} functions will automatically use the directory defined there.
#'
#' @section Usage:
#' To set the \code{grassDir} argument for all \pkg{fasterRaster} functions at once, simply do this: \cr
#' 
#' \code{grassDir <- 'C:/Program Files/GRASS GIS 8.2'} # example path for Windows \cr
#' \code{grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources"} # example path for a Mac \cr
#'
#' \code{options(grassDir = grassDir)} \cr
#'
#' To remove the \pkg{GRASS} path, simply do this: \cr
#'
#' \code{settings(grassDir = NULL)} \cr
#'
#' @keywords options
NULL
