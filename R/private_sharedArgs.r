#' @name .sharedArgs_rast
#' @title Shared argument(s)
#'
#' @param rast A \code{SpatRaster} or the name of a raster already in an existing \code{GRASS} session.
NULL

#' @name .sharedArgs_vect
#' @title Shared argument(s)
#' @param vect A \code{Spatvector}, an \code{sf} object, or the name of a spatial vector already in an existing \code{GRASS} session.
NULL

#' @name .sharedArgs_inRastName
#' @title Shared argument(s)
#'
#' @param inRastName The name of the input raster in \code{GRASS}. This is useful only if you are "chaining" \code{fasterRaster} functions together and wish to refer to the input raster later. The default value is the name of the raster (i.e., \code{names(rast)}).
NULL

#' @name .sharedArgs_inVectName
#' @title Shared argument(s)
#'
#' @param inVectName The name of the input vector in \code{GRASS}. This is useful only if you are "chaining" \code{fasterRaster} functions together and wish to refer to the input vector later. The default value is \code{'vect'}.
NULL

#' @name .sharedArgs_grassDir_grassToR_outGrassName
#' @title Shared argument(s)
#'
#' @param grassDir Name of the folder path in which \code{GRASS} is installed. For a Windows system, this might be something like \code{'C:/Program Files/GRASS GIS 8.2'}. For a Mac system, this might be like \code{"/Applications/GRASS-8.2.app/Contents/Resources"}. If you are using a lot of \pkg{fasterRaster} functions, it can be easier to set this once using \code{\link{fasterOptions}}. By default, this is \code{NULL}, in which case the install path for \code{GRASS} will be searched for, but if not found, will result in an error.
#'
#' @param grassToR If \code{TRUE} (default) then the output will be returned to \pkg{R}. If \code{FALSE}, then the output is left in the \code{GRASS} session and named the value in \code{outGrassName}. You can then refer to the output in subsequent \pkg{fasterRaster} functions and avoid the need to re-import it. This is called "chaining" functions together, and it can save a lot of computational time.
#'
#' @param outGrassName Character. Name of output in \code{GRASS}. This is useful if you want to refer to the output object in \code{GRASS} later in a session by "chaining" functions.
NULL

#' @name .sharedArgs_grassDir_grassToR
#' @title Shared argument(s)
#'
#' @param grassDir Name of the folder path in which \code{GRASS} is installed. For a Windows system, this might be something like \code{'C:/Program Files/GRASS GIS 8.2'}. For a Mac system, this might be like \code{"/Applications/GRASS-8.2.app/Contents/Resources"}. If you are using a lot of \pkg{fasterRaster} functions, it can be easier to set this once using \code{\link{fasterOptions}}. By default, this is \code{NULL}, in which case the install path for \code{GRASS} will be searched for, but if not found, will result in an error.
#'
#' @param grassToR If \code{TRUE} (default) then the output will be returned to \pkg{R}. If \code{FALSE}, then the output is left in the \code{GRASS} session and named the value in \code{outGrassName}. You can then refer to the output in subsequent \pkg{fasterRaster} functions and avoid the need to re-import it. This is called "chaining" functions together, and it can save a lot of computational time.
NULL

#' @name .sharedArgs_grassDir
#' @title Shared argument(s)
#'
#' @param grassDir Name of the folder path in which \code{GRASS} is installed. For a Windows system, this might be something like \code{'C:/Program Files/GRASS GIS 8.2'}. For a Mac system, this might be like \code{"/Applications/GRASS-8.2.app/Contents/Resources"}. If you are using a lot of \pkg{fasterRaster} functions, it can be easier to set this once using \code{\link{fasterOptions}}. By default, this is \code{NULL}, in which case the install path for \code{GRASS} will be searched for, but if not found, will result in an error.
NULL

#' @name .sharedArgs_grassToR
#' @title Shared argument(s)
#'
#' @param grassToR If \code{TRUE} (default) then the output will be returned to \pkg{R}. If \code{FALSE}, then the output is left in the \code{GRASS} session and named the value in \code{outGrassName}. You can then refer to the output in subsequent \pkg{fasterRaster} functions and avoid the need to re-import it. This is called "chaining" functions together, and it can save a lot of computational time.
NULL

#' @name .sharedArgs_outGrassName
#' @title Shared argument(s)
#'
#' @param outGrassName Character. Name of output in \code{GRASS}. This is useful if you want to refer to the output object in \code{GRASS} later in a session by "chaining" functions.
NULL
