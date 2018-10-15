#' Forest cover in 2000 for a portion of Madagascar
#'
#' Raster of occurrence/non-occurrence of forest cover in a portion of Madagascar. Cells are 30-m in resolution. Values represent forest (1) or non-forest (\code{NA}).
#'
#' @docType data
#'
#' @usage data(madForest2000)
#'
#' @format An object of class \code{'raster'}. See \code{\link[raster]{raster}}. Values are forest (1) or not forest (\code{NA}).
#'
#' @keywords datasets
#'
#' @references Vielledent, G., Grinand, C., Rakotomala, F.A., Ranaivosoa, R., Rakotoarijaona, J-R., Allnutt, T.F., and Achard, F.  2018.  Combining global tree cover loss data with historical national forest cover maps to look at six decades of deforestation and forest fragmentation in Madagascar.  Biological Conservation 222:189-197. (\href{https://www.ncbi.nlm.nih.gov/pubmed/23979570}{PubMed})
#'
#' @source \href{https://doi.org/10.1016/j.biocon.2018.04.008}{Viellendent et al. 2018}
#'
#' @examples
#' data(madForest2000)
#' plot(madForest2000)
'madForest2000'
