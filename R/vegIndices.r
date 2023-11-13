#' @name vegIndices
#'
#' @title Table of vegetation indices that can be calculated from remote sensing surface reflectance data using [vegIndex()]. A near-comprehensive table of indices can be found on the [Index Database: A Database for Remote Sensing Indices](https://www.indexdatabase.de).
#'
#' @description A table of vegetation indices that ca be calculated using [vegIndex()]. Columns include:
#' * `index``: Abbreviation of the index.
#' * `definition`: Index name
#' * `R`, `G`, `B`, `NIR`, `channel5`, `channel7`: Whether or not the index uses the red, green, blue, or near-infrared channels, and channels 5 and 7.
#' * `soilLineslope`, `soilIntercept`, `soilNR`: Whether or not the index requires soil line slope, soil intercept, and a soil noise reduction factor.
#'
#' @docType data
#'
#' @format An object of class `data.frame`.
#'
#' @keywords Remote sensing
#'
#' @seealso [vegIndex()]
#'
#' @example man/examples/ex_fastData.r
NULL
