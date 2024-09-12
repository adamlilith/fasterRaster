#' @name tutorial_hidden_functions
#'
#' @title Hidden functions in "fasterRester"
#'
#' @description **fasterRaster** contains a set of "private" functions that users can access using `fasterRaster:::functionName`. These functions are useful for power users and developers. Not all hidden functions are listed here. Often, a method will have a hidden function of the same name that starts with a period. This "period" function is intended to be supplied the [sources()] name of a `GRaster` or `GVector` from other functions so that the calling function does not need to spend the time creating the `GRaster` or `GVector` pointer before calling the function. "Period" functions will, though, often work on `GRaster`s or `GVector`s, though some error-checking and region definitions are not conducted.
#'
#' `.copyGSpatial()`: Make a copy of the **GRASS** file pointed to by a `GRaster` or `GVector`\cr
#' `.fileExt()`: Get file extension\cr
#' `.exists()`: Does the **GRASS** file of a `GRaster` or `GVector` exist?\cr
#' `.ext()`: Extent from the [sources()] name of a `GRaster` or `GVector`\cr
#' `.geomtype()`: Geometry type ("point", "line", or "area") from the [sources()] name of a `GVector`\cr
#' `.layerIndex()`: Gets the index of `GRaster` layers from a numeric, integer, character, or logical vector\cr
#' `.locationCreate()` Make a connection to **GRASS** (i.e., start **GRASS** from within **R**) and create a location\cr
#' `.locationDelete()` Deletes all files associated with a **GRASS** "location" and mapset\cr
#' `.locationFind()`: Find a specific **GRASS** "location" that already exists\cr
#' `.locationRestore()` Reconnect **GRASS** to a previously-created **GRASS** "location"\cr
#' `.locations()`: List of all available "locations"\cr
#' `.ls()`: Lists the `sources` of all objects in the active **GRASS** "location"\cr
#' `.makeGRaster()` and `.makeGVector()`: Make `GRaster`s or `GVector`s from a vector of `sources`, which are pointers to files in **GRASS**\cr
#' `.makeSourceNames()`: Makes one or more statistically unique strings that can be used as file names to represent rasters or vectors in **GRASS**\cr
#' `.mapset()`: **GRASS** "mapset" of an object or the active session\cr
#' `.message()`: Display a warning or message if the given warning has not been displayed since **fasterRaster** was attached or if a given number or hours has passed since then\cr
#' `.minVal()` and `.maxVal()`: Values in the `@minVal` and `@maxVal` slots in a `GRaster`\cr
#' `.nlevels()`: Number of levels in a `SpatVector`, `data.frame`, `data.table`, empty string, or a list of `data.frame`s, `data.table`s, and/or empty strings.\cr
#' `.plot()`: Plot using the [sources()] name of a `GRaster` or `GVector`\cr
#' `.projection()`: Value of the `@projection` slot in a `GRaster` or `GVector`\cr
#' `.quiet()`: Returns "quiet" if `faster("verbose")` is `TRUE`\cr
#' `.rastInfo()` and `.vectInfo()`: Metadata for a **GRASS** raster or vector\cr
#' `.region()`: Change or report the active region's extent and resolution\cr
#' `.regionDim()]`: Change or report the active region's resolution (also [dim()] and related functions, with no arguments)\cr
#' `.regionExt()`: Change or report the active region's extent (also [ext()] and related functions, with no arguments)\cr
#' `.regionRes()`: Change or report the active region's dimensions (also [res()] and related functions, with no arguments)\cr
#' `.rename()`: Rename a **GRASS** raster or vector\cr
#' `.rm()`: Delete rasters or vectors in **GRASS**\cr
#' `.vAsDataTable()`: Convert the attribute table linked to a vector in **GRASS** to a `data.table`. This table is distinct from the attribute table attached to a `GVector`\cr
#' `.vAttachDatabase()`: Add a database table to the **GRASS** representation of a `GVector`\cr
#' `.vCats()`: Get a `data.table` with a single column named `cat`, which corresponds to the **GRASS** attribute table's `cat` column\cr
#' `.vDetachDatabase(): Detach the **GRASS** database from a **GRASS** vector\cr
#' `.vHasDatabase()`: Tests if **GRASS** vector has a database\cr
#' `.vIncrementCats()`: Increment category values of a `GVector`\cr
#' `.vNames()`: "**GRASS**" vector attribute table column names\cr
#' `.vRecat()`: Change **GRASS** category indices of a **GRASS** vector\cr
#' `.vValidCats()`: Are category values of a vector valid?\cr
#'
#' @keywords tutorial
NULL
