#' Change the working directory of a 'GRASS' session
#'
#' @description **GRASS** sessions must have a working directory where the rasters and vectors are stored. Typiclaly, this is set when you use [startFast()] to initiate a **GRASS** session. By default, this is a temporary directory and does not need to be defined by the user. If this is temporary, there is no easy way to restore it if you shut down **R**--all rasters and vectors in **GRASS** are lost. However, you may want to use a custom directory if you want to start **R** later. If you do this, you may also want to copy or move the directory to a new place. When you do that, you *must* call this function so that the proper working directory is used in the **GRASS** session.
#'
#' @param workDir The name of the new working directory.
#'
#' @return The previous value of `workDir` (invisibly).
#'
#' @example man/examples/ex_changeWorkDir.r
#'
#' @export

changeWorkDir <- function(workDir) {

	if (!exists('.fasterRaster', where=globalenv())) {
		stop('You must initiate a GRASS session first using startFast(). No action taken.')
	}

	out <- getFastOptions('workDir')
	local({
	
		workDir <- workDir
	
	}, envir=.fasterRaster)

	warning('I need to change working folder with rgrass::initGRASS() too!!!')

	invisible(out)

}
