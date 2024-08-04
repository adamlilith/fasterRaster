#' @name tutorial_locations_mapsets
#'
#' @title Locations/projects and mapsets
#'
#' @description **GRASS GIS** uses "projects" (previously called "locations") and "mapsets" to store files (rasters, vectors, etc.). **fasterRaster** uses them, to, but invisibly to most users. Thus, this tutorial is mostly of interest to developers and other curious people.
#'
#' ## **GRASS** locations/projects
#'
#' Starting with **GRASS** 
#'
#' Upon starting, **GRASS** creates (or loads) a location, which corresponds to a folder on the user's system. Importantly, all rasters and vectors in a location must have the same coordinate reference system (CRS). Confusingly, rasters and vectors in the same **GRASS** location do not necessarily have to represent the same place on Earth. In general, rasters and vectors can only interact with one another if they are in the same location and mapset.  **GRASS** can only have a connection to a single location at a time.
#'
#' **fasterRaster** handles projects and mapsets automatically, so users typically do not need to manage them. Projects are created on an as-needed basis. Within a given **R** session, if no projects have been made, calling [fast()] to create a `GRaster` or `GVector` will 1) make a connection to **GRASS** and 2) create a location with a CRS the same as the raster or vector. The raster or vector is then stored in this location. [fast()] starts the connection and creates the location using the exported by publicly undocumented function `.locationCreate()`.
#'
#' If [fast()] is called and a location already exists that has the same CRS as the raster or vector, one of two things will happen. First, if **GRASS** is already connected to that location, the raster or vector is simply imported. Second, if **GRASS** is not connected to the location that has the appropriate CRS, it will use `.locationRestore()` to connect to the proper one, then import the raster or vector.
#'
#' Other functions might also cause **GRASS** to connect to a pre-existing location. Generally, if a function is applied to a `GRaster` or `GVector`, it will first check to see that **GRASS** is connected to the location in which the raster or vector is stored. If not, it will use `.locationRestore()` to do so first.
#'
#' Creating a new location or switching connections to pre-existing locations adds a few seconds to processing time of rasters and vectors. To avoid this, users can work as much as possible on sets of rasters and vectors with the same CRS (i.e., the same location).
#'
#' ## **FasterRaster** mapsets
#'
#' **GRAS** "mapsets" are sub-folders within a location. Every location *must* have a mapset (and thus, a sub-folder) named "PERMANENT". Users of Mapsets are intended to store sub-projects that use rasters and vectors with the same CRS. Users of **GRASS** can switch between mapsets. However, for ease-of-use and development, **fasterRaster** always uses the "PERMANENT" mapset within a given location.
#'
#' ## Functions that manage locations and mapsets
#'
#' These functions are exported but not publicly documented. You can, however, read the documentation by opening the development file (i.e., at the **fasterRaster** GitHub [repository](https://github.com/adamlilith/fasterRaster)). All functions take "`x`" as an argument.
#'
#' * `.location(x)`: Name of the location of `x` (a **fasterRaster** object), or the current location (if `x` is missing).
#' * `.locations()`: Names and CRSs of all available locations (takes no arguments).
#' * `.locationCreate(x)`: Create a location with the same CRS as `x`.
#' * `.locationRestore(x)`: Connect to a pre-existing location. `x` can be a **fasterRaster** object, a `SpatRaster`, `SpatVector`, or `sf` object, or the name of a location.
#' * `.locationFind(x, return = <option>)`: Given a spatial object `x`, find the name, index, or CRS of a location that matches it. If `x` is missing, return a list of all locations. `<option>` can be `"name"` (name of the location), `"index"` (index), or "`crs`" (coordinate reference string in WKT format).
#' * `.ls()`: List all file names of rasters and/or vectors in the current location.
#' * `.mapset(x)`: Name of the mapset that contains the object `x`.
#' 
#' ## More rabbit-holing
#'
#' **fasterRaster** maintains a package-specific environment named `.fasterRaster`, where it keeps a list of locations at `.fasterRaster$locations`. This is a named list of locations, where each element is has the name of the location, and the value of the element is the location's CRS. The current location is tracked at `.fasterRaster$activeLocation`.
#'
#' @keywords tutorial
#' @example man/examples/ex_GRaster_GVector.r
NULL
