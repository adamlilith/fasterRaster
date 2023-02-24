#' @name .sharedArgs_rast
#' @title Shared argument(s)
#'
#' @param rast A \code{SpatRaster} or the name of a raster already in an existing \code{GRASS} session.
NULL

#' @name .sharedArgs_rast_multiple
#' @title Shared argument(s)
#'
#' @param rast A \code{SpatRaster} with one or more layers, \emph{or} the name(s) of one or more raster in the active \code{GRASS} session.
NULL

#' @name .sharedArgs_vect
#' @title Shared argument(s)
#' @param vect A \code{SpatVector}, an \code{sf} object, or the name of a spatial vector in the active \code{GRASS} session.
#' @keywords internal
NULL

#' @name .sharedArgs_inRastName
#' @title Shared argument(s)
#'
#' @param inRastName Character, missing, or `NULL`. The name of the input raster in \code{GRASS}. If this is missing, the name of the raster or raster file will be used. If `NULL`, then "\code{inputRast}" will be used. This argument is useful if you are \link{chaining} \pkg{fasterRaster} functions together and wish to refer to the input raster later in the \code{GRASS} session.
#' @keywords internal
NULL

#' @name .sharedArgs_inRastName_multiple
#' @title Shared argument(s)
#'
#' @param inRastName The name of the input raster(s) in \code{GRASS}. If \code{rast} is a multi-layer raster, then there must be one name per layer. If missing or `NULL`, the name(s) of the raster(s) will be used (i.e., \code{names(rast)}). This argument is useful if you are \link{chaining} \pkg{fasterRaster} functions together and wish to refer to the raster(s) later in the \code{GRASS} session.
#' @keywords internal
NULL

#' @name .sharedArgs_inVectName
#' @title Shared argument(s)
#'
#' @param inVectName Character. The name of the input vector in \code{GRASS}. If missing or `NULL`, \"code{inputVect}" will be used. This is useful if you are \link{chaining} \pkg{fasterRaster} functions together and wish to refer to the vector later in the \code{GRASS} session.
#' @keywords internal
NULL

#' @name .sharedArgs_cores
#' @title Shared argument(s)
#'
#' @param cores Number of processor cores to use. This must be a positive integer. The default is 1. You can set this for all functions at once using \code{\link{fasterSetOptions}}.
#' @keywords internal
NULL

#' @name .sharedArgs_memory
#' @title Shared argument(s)
#'
#' @param memory Amount of memory to allocate to the job, in MB. The default is 300 MB. You can set this for all functions at once using \code{\link{fasterSetOptions}}.
#' @keywords internal
NULL

#' @name .sharedArgs_replace
#' @title Shared argument(s)
#'
#' @param replace Logical or `NULL`. If \code{FALSE} or `NULL` (default), then attempting to overwrite rasters or vectors already in a \code{GRASS} session will result in an error. You can allow overwriting by changing this to \code{TRUE}, or by setting \code{inRastName}, \code{inVectName}, and/or \code{outGrassName} to values different from rasters or vectors that already exist in the session (not all functions have these arguments). Use \code{\link{fasterLs}} to see existing rasters and vectors.  You can set this for all functions at once using \code{\link{fasterSetOptions}}.
#' @keywords internal
NULL

#' @name .sharedArgs_grassDir
#' @title Shared argument(s)
#'
#' @param grassDir Character or `NULL`. Name of the path in which \code{GRASS} is installed. For a Windows system, this might be something like \code{'C:/Program Files/GRASS GIS 8.3'}, for a Mac, like \code{"/Applications/GRASS-8.3.app/Contents/Resources"}, and for Linux like \code{'/usr/local/grass'}. If you are using a lot of \pkg{fasterRaster} functions, you can set this for all functions at once using \code{\link{fasterSetOptions}}. By default, this is `NULL`, in which case the install path for \code{GRASS} will be searched for, but if not found, will result in an error (usually the case).
#' @keywords internal
NULL

#' @name .sharedArgs_grassToR
#' @title Shared argument(s)
#'
#' @param grassToR Logical or `NULL`. If \code{TRUE} or `NULL` (default) then the output will be returned to \code{R}. If \code{FALSE}, then the output is left in the \code{GRASS} session (with the name given by \code{outGrassName}). You can set this argument for all functions at once using \code{\link{fasterSetOptions}}.
#' @keywords internal
NULL

#' @name .sharedArgs_trimRast
#' @title Shared argument(s)
#'
#' @param trimRast Logical. If \code{TRUE} (default) and rasters are imported from \code{GRASS} to \code{R}, then all rows and columns that are entirely \code{NA} will be removed. If \code{FALSE}, then rasters may be "padded" with empty rows and columns. This is may be especially important if you are using rasters and vectors of very different sizes. Within \code{GRASS}, empty rows and columns can be added automatically by manipulation of the \link{region}. You can set this argument for all functions at once using \code{\link{fasterSetOptions}}.
#' @keywords internal
NULL

#' @name .sharedArgs_outVectClass
#' @title Shared argument(s)
#'
#' @param outVectClass Either \code{'SpatRaster'} or \code{'sf'}. This determines the class of spatial vectors imported back to \code{R} from \code{GRASS}. If \code{'SpatRaster'} (default), a \code{SpatVector} from the \pkg{terra} package is returned. If \code{'sf'}, an \code{sf} object from the \pkg{sf} package is returned. This argument is ignored if \code{grassToR} is \code{FALSE}. You can set this argument for all functions at once using \code{\link{fasterSetOptions}}.
#' @keywords internal
NULL

#' @name .sharedArgs_outGrassName
#' @title Shared argument(s)
#'
#' @param outGrassName Character. Name of output in \code{GRASS}. This is useful if you want to refer to the output object in \code{GRASS} later in a session by \link{chaining} functions.
#' @keywords internal
NULL

#' @name .sharedArgs_outGrassName_multiple
#' @title Shared argument(s)
#'
#' @param outGrassName Character. One name per object created in \code{GRASS} (i.e., one per raster layer or vector). This is useful if you want to refer to the output object(s) in \code{GRASS} later in a session by \link{chaining} functions.
#' @keywords internal
NULL

#' @name .sharedArgs_autoRegion
#' @title Shared argument(s)
#'
#' @param autoRegion Logical or `NULL`. If \code{TRUE} or `NULL` (default), then during execution of this function, the \code{GRASS} \link{region} will be resized to encompass the raster/vector/area of interest, and if relevant, resampled to match its spatial resolution. If \code{FALSE}, the the current region's extent and resolution will be used. Most users will want to keep this \code{TRUE} or `NULL` (same as \code{TRUE}). If you change this to \code{FALSE}, you will need to keep track of the \code{GRASS} \link{region} properties. This argument can be set for all functions at once using \code{\link{fasterSetOptions}}.
#' @keywords internal
NULL

#' @name .sharedArgs_dots_forInitGrass
#' @title Shared argument(s)
#'
#' @param ... Arguments to pass to \code{\link{startFaster}}.
NULL

#' @name .sharedArgs_dots_forInitGrass_andGrassModule
#' @title Shared argument(s)
#'
#' @param ... Arguments to pass to \code{\link{startFaster}} and/or to the respective \code{GRASS} module (see \code{\strong{See Also}} section).
NULL
