#' Patch rasters together
#'
#' This function "patches" rasters together. In some GIS programs this is called "mosaicking" but in GRASS it is called "patching." Only \code{NA} values in the first raster are filled in. This is useful for replacing missing data with new data, for example, or mosaicking rasters with different extents together. The function calls the GRASS function \href{https://grass.osgeo.org/grass78/manuals/r.patch.html}{r.patch}.
#'
#' @param ... Either two or more rasters or the names of two or more rasters in an existing GRASS session.
#' @param grassDir Character or \code{NULL} (default). Name of the directory in which GRASS is installed. Example: \code{'C:/Program Files/GRASS GIS 7.8'}. If this is \code{NULL}, R will search for the directory in which GRASS is installed. This usually fails, or if it succeeds, takes several minutes.
#' @param alreadyInGrass Logical, if \code{FALSE} (default) then start a new GRASS session and import the raster named in \code{rast}. If \code{FALSE}, use a raster already in GRASS with the name given by \code{rast}. The latter is useful if you are chaining \pkg{fasterRaster} functions together and the first function initializes the session. The first function should use \code{alreadyInGrass = FALSE} and subsequent functions should use \code{alreadyInGrass = TRUE} then use their \code{rast} (or \code{vect}) arguments to name the raster (or vector) that was made by the previous function.
#' @param grassToR Logical, if \code{TRUE} (default) then the product of the calculations will be returned to R. If \code{FALSE}, then the product is left in the GRASS session and named \code{longitude} and \code{latitude}. The latter case is useful (and faster) when chaining several \pkg{fasterRaster} functions together.
#' @param outGrassName Character. Name of output in GRASS. This is useful if you want to refer to the output object in GRASS later in a session.
#' @return If \code{grassToR} if \code{TRUE}, then a raster with the overall same extent, resolution, and coordinate reference system as \code{rast}. Otherwise, a raster is written into the GRASS session. The name of this raster is as \code{paste0(outGrassName, '_', xxx)}. For example, if \code{outGrassName = 'horizonHeight'}, and \code{directions} is \code{c(0, 90, 180, 270)}, then four rasters will be written: \code{horizonHeight_000}, \code{horizonHeight_090}, \code{horizonHeight_180}, and \code{horizonHeight_270}. Note the padding with zeros before angles <10.
#' @details See the documentation for the GRASS module \code{r.horizon} at \url{https://grass.osgeo.org/grass78/manuals/r.horizon.html}.
#' @seealso \code{\link[raster]{mosaic}}, \code{\link[raster]{mask}}, \code{\link[raster]{extend}}
#' @examples
#' \donttest{
#' # change this to where GRASS 7 is installed on your system
#' grassDir <- 'C:/Program Files/GRASS GIS 7.8' # example for a PC
#' grassDir <- "/Applications/GRASS-7.8.app/Contents/Resources" # for a Mac
#'

#' # obtain DEM for Madagascar and artifically break it into two subsections
elev <- raster::getData('alt', country='MDG')
mad2 <- raster::getData('GADM', country='MDG', level=2)

faritra1 <- mad2[mad2@data$NAME_1 == 'Antsiranana', ]
faritra2 <- mad2[mad2@data$NAME_1 == 'Toamasina', ]

elev1 <- crop(elev, faritra1)
elev2 <- crop(elev, faritra2)

par(mfrow=c(1, 2))
plot(elev1)
plot(elev2)

patched <- fasterPatch(elev1, elev2, grassDir=grassDir)

#' data(madElev)
#' data(madElev2)
#' madElevPatched <- fasterPatch(madElev, madElev2, grassDir=grassDir)
#' par(mfrow=c(1, 3))
#' plot(madElev)
#' plot(madElev2)
#' plot(madElevPatched)
#'
#' }
#' @export

fasterPatch <- function(
	...,
	grassDir = NULL,
	alreadyInGrass = FALSE,
	grassToR = TRUE,
	outGrassName = 'patched'
) {

	flags <- c('quiet', 'overwrite')
	
	# create extent
	rasts <- list(...)
	rasts <- rasts[which(sapply(rasts, class) %in% c('RasterLayer', 'RasterBrick', 'RasterStack'))]
	if (length(rasts) < 2) stop('Function requires >1 raster as input.')
	
	proj <- raster::projection(rasts[[1]])
	ext <- raster::extent(rasts[[1]])
	for (i in 2:length(rasts)) ext <- raster::merge(ext, rasts[[i]])
	
	ext <- as(ext, 'SpatialPolygons')
	raster::projection(ext) <- proj
	
	# initialize GRASS
	input <- initGrass(alreadyInGrass=FALSE, rast=NULL, vect=ext, grassDir=grassDir)
	
	# export rasters to GRASS
	rastNames <- unlist(sapply(rasts, names))
	rastNames[1] <- outGrassName
	for (i in seq_along(rasts)) {
		exportRastToGrass(rasts[[i]], grassName=rastNames[i])
	}
	
	# execute
	rgrass7::execGRASS('r.patch', input=rastNames, output=outGrassName, flags=flags, ...)

	# return
	if (grassToR) {
	
		out <- rgrass7::readRAST(outGrassName)
		out <- raster::raster(out)
		names(out) <- outGrassName
		out
		
	}
	
}
