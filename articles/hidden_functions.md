# For developers: Hidden fasterRaster functions

**fasterRaster** contains a set of “private” functions that users can
access using `fasterRaster:::functionName`. These functions are useful
for power users and developers. Not all hidden functions are listed
here. Often, a method will have a hidden function of the same name that
starts with a period (e.g.,
[`.plot()`](https://github.com/adamlilith/fasterRaster/reference/dot-plot.md)).
This “period” function is intended to be supplied the
[`sources()`](https://adamlilith.github.io/fasterRaster/reference/sources.html)
name of a `GRaster` or `GVector` from other functions so that the
calling function does not need to spend the time creating the `GRaster`
or `GVector` pointer before calling the function. “Period” functions
will, though, often work on `GRaster`s or `GVector`s, though some
error-checking and region re-definition is not conducted.

## General

- [`.addons()`](https://github.com/adamlilith/fasterRaster/reference/addons.md):
  Tests if an addon is installed, and if not, attempts to install it.
- [`.backdoor()`](https://github.com/adamlilith/fasterRaster/reference/dot-backdoor.md):
  Calls \[faster()\] and sets **GRASS** folder to
  “`C:/Program Files/GRASS GIS X.Y`”, plus other options useful for
  development.
- `.fileExt()`: Get file extension
- [`.ls()`](https://github.com/adamlilith/fasterRaster/reference/ls.md):
  Lists the `sources` of all objects in the active **GRASS**
  “project/location”
- [`.message()`](https://github.com/adamlilith/fasterRaster/reference/message.md):
  Display a warning or message if the given warning has not been
  displayed since **fasterRaster** was attached or if a given number of
  hours has passed
- [`.quiet()`](https://github.com/adamlilith/fasterRaster/reference/quiet.md):
  Returns “quiet” if `faster("debug")` is `TRUE`
- [`.workDir()`](https://github.com/adamlilith/fasterRaster/reference/workDir.md):
  Working directory of a `GLocation` object

## Rasters and vectors

- [`.copyGSpatial()`](https://github.com/adamlilith/fasterRaster/reference/copyGSpatial.md):
  Make a copy of the **GRASS** file pointed to by a `GRaster` or
  `GVector`
- [`.exists()`](https://github.com/adamlilith/fasterRaster/reference/exists.md):
  Does the **GRASS** file of a `GRaster` or `GVector` exist?
- [`.ext()`](https://github.com/adamlilith/fasterRaster/reference/dot-ext.md):
  Extent from the
  [`sources()`](https://adamlilith.github.io/fasterRaster/reference/sources.html)
  name of a `GRaster` or `GVector`
- `.makeSourceNames()`: Makes one or more statistically unique strings
  that can be used as file names to represent rasters or vectors in
  **GRASS**
- [`.plot()`](https://github.com/adamlilith/fasterRaster/reference/dot-plot.md):
  Plot using the
  [`sources()`](https://adamlilith.github.io/fasterRaster/reference/sources.html)
  name of a `GRaster` or `GVector`
- [`.projection()`](https://github.com/adamlilith/fasterRaster/reference/dot-projection.md):
  Value of the `@projection` slot in a `GRaster` or `GVector`
- [`.rastInfo()`](https://github.com/adamlilith/fasterRaster/reference/dot-rastInfo.md)
  and
  [`.vectInfo()`](https://github.com/adamlilith/fasterRaster/reference/dot-vectInfo.md):
  Metadata for a **GRASS** raster or vector
- [`.rename()`](https://github.com/adamlilith/fasterRaster/reference/dot-rename.md):
  Rename a **GRASS** raster or vector
- [`.rm()`](https://github.com/adamlilith/fasterRaster/reference/rm.md):
  Delete rasters or vectors in **GRASS**

## Vectors

- `.aggDisaggVect()`: Aggregate or disaggregate a vector using its
  [`sources()`](https://adamlilith.github.io/fasterRaster/reference/sources.html)
  name.
- [`.geomtype()`](https://github.com/adamlilith/fasterRaster/reference/dot-geomtype.md):
  Geometry type (“point”, “line”, or “area”) from the
  [`sources()`](https://adamlilith.github.io/fasterRaster/reference/sources.html)
  name of a `GVector`
- `.validVector()`: Test if a `GVector` is valid.
- [`.vAsDataTable()`](https://github.com/adamlilith/fasterRaster/reference/vAsDataTable.md):
  Convert the attribute table linked to a vector in **GRASS** to a
  `data.table`. This table is distinct from the attribute table attached
  to a `GVector`
- [`.vAttachDatabase()`](https://github.com/adamlilith/fasterRaster/reference/vAttachDatabase.md):
  Add a database table to the **GRASS** representation of a `GVector`
- [`.vCats()`](https://github.com/adamlilith/fasterRaster/reference/vCats.md):
  Get a `data.table` with a single column named `cat`, which corresponds
  to the **GRASS** attribute table’s `cat` column
- [`.vDetachDatabase()`](https://github.com/adamlilith/fasterRaster/reference/vDetachDatabase.md):
  Detach the **GRASS** database from a **GRASS** vector
- [`.vHasDatabase()`](https://github.com/adamlilith/fasterRaster/reference/vHasDatabase.md):
  Tests if **GRASS** vector has a database
- [`.vIncrementCats()`](https://github.com/adamlilith/fasterRaster/reference/vIncrementCats.md):
  Increment category values of a `GVector`
- [`.vNames()`](https://github.com/adamlilith/fasterRaster/reference/vNames.md):
  “**GRASS**” vector attribute table column names
- [`.vRecat()`](https://github.com/adamlilith/fasterRaster/reference/vRecat.md):
  Change **GRASS** category indices of a **GRASS** vector
- [`.vValidCats()`](https://github.com/adamlilith/fasterRaster/reference/dot-vValidCats.md):
  Are category values of a vector valid?

## Rasters

- [`.layerIndex()`](https://github.com/adamlilith/fasterRaster/reference/dot-layerIndex.md):
  Gets the index of `GRaster` layers from a numeric, integer, character,
  or logical vector
- [`.makeGRaster()`](https://github.com/adamlilith/fasterRaster/reference/dot-makeGRaster.md)
  and
  [`.makeGVector()`](https://github.com/adamlilith/fasterRaster/reference/dot-makeGVector.md):
  Make `GRaster`s or `GVector`s from a vector of `sources`, which are
  pointers to files in **GRASS**
- [`.minVal()`](https://github.com/adamlilith/fasterRaster/reference/dot-minVal.md)
  and
  [`.maxVal()`](https://github.com/adamlilith/fasterRaster/reference/dot-maxVal.md):
  Values in the `@minVal` and `@maxVal` slots in a `GRaster`
- [`.nlevels()`](https://github.com/adamlilith/fasterRaster/reference/dot-nlevels.md):
  Number of levels in a `SpatVector`, `data.frame`, `data.table`, empty
  string, or a list of `data.frame`s, `data.table`s, and/or empty
  strings.

## **GRASS** “projects/locations” and “mapsets”

- [`.locationCreate()`](https://github.com/adamlilith/fasterRaster/reference/locationCreate.md)
  Make a connection to **GRASS** (i.e., start **GRASS** from within
  **R**) and create a location
- [`.locationDelete()`](https://github.com/adamlilith/fasterRaster/reference/location.md)
  Deletes all files associated with a **GRASS** “location” and mapset
- `.locationFind()`: Find a specific **GRASS** “location” that already
  exists
- [`.locationRestore()`](https://github.com/adamlilith/fasterRaster/reference/locationRestore.md)
  Reconnect **GRASS** to a previously-created **GRASS** “location”
- [`.locations()`](https://github.com/adamlilith/fasterRaster/reference/locations.md):
  List of all available “locations”
- [`.g.proj()`](https://github.com/adamlilith/fasterRaster/reference/dot-g.proj.md):
  Runs **GRASS** `g.proj` tool to display projection of current
  “project”
- [`.g.region()`](https://github.com/adamlilith/fasterRaster/reference/dot-g.region.md):
  Runs **GRASS** `g.region` tool to display region of current “project”
- [`.mapset()`](https://github.com/adamlilith/fasterRaster/reference/mapset.md):
  **GRASS** “mapset” of an object or the active session

## **GRASS** “regions”

- [`.region()`](https://github.com/adamlilith/fasterRaster/reference/region.md):
  Change or report the active region’s extent and resolution
- `.regionDim()]`: Change or report the active region’s resolution (also
  [`dim()`](https://adamlilith.github.io/fasterRaster/reference/dim.html)
  and related functions, with no arguments)
- [`.regionExt()`](https://github.com/adamlilith/fasterRaster/reference/region.md):
  Change or report the active region’s extent (also
  [`ext()`](https://adamlilith.github.io/fasterRaster/reference/ext.html)
  and related functions, with no arguments)
- [`.regionRes()`](https://github.com/adamlilith/fasterRaster/reference/region.md):
  Change or report the active region’s dimensions (also
  [`res()`](https://adamlilith.github.io/fasterRaster/reference/res.html)
  and related functions, with no arguments)

~ FINIS ~
