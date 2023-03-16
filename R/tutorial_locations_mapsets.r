#' @name location
#'
#' @title An explanation of GRASS "locations" and "mapsets"
#'
#' @description Most users of **fasterRaster** will not need to understand or explicitly use **GRASS** locations and mapsets. The default settings allow use of functions in **GRASS** without users needing to explicitly define locations and mapsets. This help page is porvided to assist power users who may wish to use different locations/mapsets or develop their own applications based on **fasterRaster**.
#'
#' A **GRASS** location is a set of vectors and/or rasters with the same coordinate reference system (CRS), that may or may not represent the same location on Earth. Rather, a location can be thought of as a folder in which spatial objects of the same CRS are stored. Mapsets are subfolders inside a location, and typically represent different projects or sub-projects. Locations are themselves inside a working directory. By default, the working directory is the temporary directory on the user's system (assigned using [tempdir()]).  Hence, the "containments" of these components is as:
#'
#' Multiple `mapset`s can be inside a single `location`, which is inside a `working directory`
#'
#' Rasters and vectors must typically be in the same location and mapset to be operated on by a single function, although they can be copied between locations/mapsets.  When the CRS is different between locations, rasters and vectors are automaticlaly reprojected "on the fly". This can be OK for non-essential work, but in practice, it is better to use [project()] to ensure the projecting is done in the manner you prefer.
#'
#' User can exercise full control over working directory, location, and mapset when they initiate a **GRASS** session using [fastStart()]. The ellipse (`...`) argument allows users to assign a mapset or location, and the `workDir` argument allows for a user-defined working directory. The latter is especially helpful if the user wishes to keep the **GRASS** session and restart it with the same spatial objects.
#'
#' Users can switch between locations, mapsets, and working directories that have been created using [fastStart()] using [sessionRestore()]. The location or mapset of the active session can be obtained using [location()] and [mapset()], plus [getFastOptions()].
#'
#' @keywords tutorial
NULL
