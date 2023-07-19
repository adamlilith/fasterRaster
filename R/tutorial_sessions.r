#' @name tutorial_sessions
#'
#' @title An explanation of GRASS "sessions", "locations", and "mapsets"
#'
#' @description Most users of **fasterRaster** will not need to understand or explicitly use **GRASS** locations and mapsets. The default settings allow use of functions in **GRASS** without users needing to explicitly define locations and mapsets. This help page is provided to assist power users who may wish to use different locations/mapsets or develop their own applications based on **fasterRaster**. This is just a summary of the full **GRASS** database system.
#'
#' ### **GRASS** "sessions", "locations", and "mapsets"
#' To start using **fasterRaster**, a user needs to initiate a "session" using [faster()]. This function 1) creates a **GRASS** "location" and "mapset" inside a working directory, and 2) registers **GRASS** so it does operations in this place. Locations and mapsets are stored as folders and files within the working directory, but they also represent data structures in **GRASS**.  Essentially, the working directory contains one or more locations, and each location contains one or more mapsets. A **GRASS** *location* is a set of vectors and/or rasters with the same coordinate reference system (CRS), that may or may not represent the same location on Earth. A **GRASS** location can be thought of as a folder in which spatial objects of the same CRS are stored. *Mapsets* are subfolders inside a location, and typically represent different sub-projects. Each location must always contain a mapset named "PERMANENT" (all-capitals).
#'
#' As used in **fasterRaster** documentation, a *session* is created using [faster()] or restored using [fastRestore()]. Having the requisite location and mapset folders and files on disk is not enough. **GRASS** must be started and linked to the location/mapset using one of these functions.
#'
#' ### Working with sessions
#'
#' By default, the working directory is set to the user's temporary folder, obtained through [tempdir()]. However, if a user wishes to save their work, quit **R**, and later restart the **GRASS** session, they will want to use a custom folder. This is specified in [faster()] using the argument `workDir`. Restarting a session should be done using [fastRestore()].
#'
#' Rasters and vectors must typically be in the same location and mapset to be operated on by a single function, although they can be copied between locations/mapsets. When the CRS is different between locations, rasters and vectors are automatically reprojected "on the fly". This can be OK for non-essential work, but in practice, it is better to use [project()] to ensure the projecting is done in the manner you prefer.
#'
#' The location and mapset in which a `GRaster` or `GVector` occurs can be obtained using [location()] and [mapset()]. The location, mapset, and working directory of the currently active **GRASS** session can be obtained from [getFastOptions()].
#'
#' Users can exercise full control over working directory, location, and mapset when they initiate a **GRASS** session using [faster()]. The ellipse (`...`) argument allows users to assign a `mapset` and/or `location`, and the `workDir` argument allows for a user-defined working directory.
#'
#' Users can switch between locations, mapsets, and working directories that have been created using [fastRestore()]. If **R** has been restarted and a user wishes to return to a specific (not-temporary) working folder, location, and mapset, then they should use [fastRestore()] (not `faster()`).
#'
#' @keywords tutorial
NULL
