#' @name fasterFunctions
#'
#' @title Table of functions in fasterRaster and equivalents in GRASS and terra
#'
#' @description Table of functions in \pkg{fasterRaster} and equivalents in GRASS and \pkg{terra}, plus a link to the GRASS function.
#'
#' @docType data
#'
#' @usage data(fasterFunctions)
#'
#' @format An object of class \code{'data.table'}.
#'
#' @keywords functions
#'
#' @examples
#'
#' The next two lines return the same thing.
#' data(fasterFunctions)
#' fasterLookup()
#'
#' fasterLookup('fasterRasterize') # lookup by fasterRaster name
#' fasterLookup('rasterize', terra = TRUE) # lookup by terra name
#' fasterLookup('v.to.rast', grass = TRUE) # lookup by GRASS name
#' fasterLookup('v.to', grass = TRUE, approx = TRUE) # approximate lookup
#'
NULL
