#' Setup fasterRaster for ABS
#'
#' This is a secret function for ABS's machine to be used for faster development of **fasterRaster**.
#'
#' @noRd
.backdoor <- function() faster(grassDir = "C:/Program Files/GRASS GIS 8.3", memory = 1024 * 8, cores = 2, verbose = TRUE)
