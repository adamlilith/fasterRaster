# Create a GRaster or GVector

`fast()` creates a `GRaster` or `GVector` from 1) a file; 2) from a
`SpatRaster`, `SpatVector`, or `sf` vector; or 3) from a numeric vector,
`matrix`, `data.frame`, or `data.table`. Behind the scenes, this
function will also create a connection to **GRASS** if none has yet been
made yet.

**GRASS** supports loading from disk a variety of raster formats (see
the **GRASS** manual page for `r.in.gdal` (see `grassHelp("r.in.gdal")`)
and vector formats `v.in.ogr` (see grassHelp("v.in.ogr")\`), though not
all of them will work with this function.

Note that `GVectors` may fail to be created if they contain issues that
do not coincide with the topological data model used by **GRASS**. The
most common of these is overlapping polygons. See *Details* on how to
fix these kinds of issues.

Note also that **GRASS** (and thus, **fasterRaster**) is *not* very fast
when loading vectors. So, if the vector is large and you only want a
portion of it, consider using the `extent` argument to load the spatial
subset you need.

## Usage

``` r
# S4 method for class 'character'
fast(
  x,
  rastOrVect = NULL,
  levels = TRUE,
  extent = NULL,
  correct = TRUE,
  snap = NULL,
  area = NULL,
  steps = 10,
  dropTable = FALSE,
  resolve = NA,
  verbose = TRUE,
  ...
)

# S4 method for class 'SpatRaster'
fast(x, ...)

# S4 method for class 'SpatVector'
fast(
  x,
  extent = NULL,
  correct = TRUE,
  snap = NULL,
  area = NULL,
  steps = 10,
  dropTable = FALSE,
  resolve = NA,
  verbose = TRUE
)

# S4 method for class 'sf'
fast(
  x,
  extent = NULL,
  correct = TRUE,
  snap = NULL,
  area = NULL,
  steps = 10,
  resolve = NA,
  dropTable = FALSE,
  verbose = TRUE
)

# S4 method for class 'missing'
fast(x, rastOrVect, crs = "")

# S4 method for class 'numeric'
fast(x, crs = "", keepgeom = FALSE)

# S4 method for class 'data.frame'
fast(x, geom = 1:2, crs = "", keepgeom = FALSE)

# S4 method for class 'data.table'
fast(x, geom = 1:2, crs = "", keepgeom = FALSE)

# S4 method for class 'matrix'
fast(x, geom = 1:2, crs = "", keepgeom = FALSE)
```

## Arguments

- x:

  Any one of:

  - A `SpatRaster` raster. Rasters can have one or more layers.

  - A `SpatVector` or `sf` spatial vector. See especially arguments
    `correct`, `area`, `snap`, `steps`, and `verbose`.

  - A character string or a vector of strings with the path(s) and
    filename(s) of one or more rasters or one vector to be loaded
    directly into **GRASS**. The function will attempt to ascertain the
    type of object from the file extension (raster or vector), but it
    can help to indicate which it is using the `rastOrVect` argument if
    it is unclear. For rasters, see especially argument `levels`. For
    vectors, see especially arguments `correct`, `resolve`, `area`,
    `snap`, `steps`, and `verbose`.

  - A vector with an even number of numeric values representing
    longitude/latitude pairs. See arguments `geom`, `keepgeom`, and
    `crs`.

  - A `data.frame`, `data.table`, or `matrix`: Create a `points`
    `GVector`. Two of the columns must represent longitude and latitude.
    See arguments `geom`, `keepgeom`, and `crs`.

  - Missing: Creates a generic `GRaster` or `GVector`. You must specify
    `rastOrVect`; for example, `fast(rastOrVect = "raster")`. Also see
    argument `crs`.

- rastOrVect:

  Either `NULL` (default), or `"raster"` or `"vector"`: If `x` is a
  filename, then the function will try to ascertain whether it
  represents a raster or a vector, but sometimes this will fail. In that
  case, it can help to specify if the file holds a raster or vector.
  Partial matching is used.

- levels:

  (`GRaster`s only): Any of:

  - Logical: If `TRUE` (default) and at least one layer of a raster is
    of type `integer`, search for a "levels" file, load it, and attach
    levels. A levels file will have the same name as the raster file,
    but end with any of "rdata", "rdat", "rda", "rds", "csv", or "tab"
    (case will generally not matter). If such a file is not found, no
    levels will be assigned. The levels file must contain either a
    `data.frame`, `data.table`, or `list` of `data.frame`s or
    `data.table`s, or `NULL`.

  - A `data.frame`, `data.table`, or list of `data.frame`s or
    `data.table`s with categories for categorical rasters. The first
    column of a table corresponds to raster values and must be of type
    `integer`. A subsequent column corresponds to category labels. By
    default, the second column is assumed to represent labels, but this
    can be changed with `activeCat<-`. Level tables can also be `NULL`
    (e.g., `data.fame(NULL)`). You can also assign levels after loading
    a raster using `levels<-`.

  - `NULL`: Do not attach a levels table. \#'

- extent:

  (`GVector`s only): Either a `NULL` (default), or a `GVector`, a
  `SpatVector`, a `SpatExtent` object, an `sf` vector, a `bbox` object,
  or a numeric vector of 4 values providing a bounding box. If provided,
  only vector features within this bounding box are imported. If
  `extent` is a numeric vector, the values *must* be in the order west,
  east, south, north. If `NULL`, the entire vector is imported.

- correct:

  Logical (`GVector`s only): Correct topological issues. See *Details*
  for more details! By default, this is `TRUE`.

- snap:

  `GVector`s only: Numeric or `NULL` (default). The value of `snap`
  indicates how close vertices need to be for them to be shifted to to
  the same location. Units of `snap` are map units (usually meters), or
  degrees for unprojected CRSs. For lines and polygons vectors, a value
  of `NULL` will invoke an iterative procedure to find an optimal,
  smallest value of `snap`. To turn snapping off, set `snap = 0`. See
  *Details* for more details!

- area:

  Polygon `GVector`s only: Either a positive numeric value or `NULL`
  (default). Remove polygons with an area smaller than this value. Units
  of `area` are in square meters (regardless of the CRS). If `NULL`,
  then an iterative procedure is used to identify a value of `area` that
  results in a topologically correct polygon vector. For point and lines
  vectors, this argument is ignored. To turn area removal off, set
  `area = 0`. See *Details* for more details!

- steps:

  `GVector`s only: A positive integer \> 1 (default is 10). When using
  automatic vector correction (i.e., either `snap = NULL` and/or
  `area = NULL`), this is the number of values of `snap` and/or `area`
  to try to generate a correct topology, including no snapping or
  polygon removal (i.e, `snap = 0` and `area = 0`).

- dropTable:

  `GVector`s only: Logical. If `TRUE`, then drop the data table
  associated with a vector. By default, this is `FALSE`. See *Details*
  for more details!

- resolve:

  `GVector`s only: Character. If a `GVector` would be topologically
  invalid after a first attempt at creating it, then this method will be
  used to resolve the issue and create a valid `GVector`. Partial
  matching is used.

  - `"disaggregate"`: Coerce each area of overlap between polygons into
    its own geometry. The output will not have a data table associated
    with it.

  - `"aggregate"`: Coerce all geometries into a "multipart" geometry so
    it acts like a single geometry. The output will not have a data
    table associated with it.

  - `NA` (default): Do neither of the above and if either `snap` or
    `area` is `NULL`, keep trying to create the `GVector`. Upon success,
    the `GVector` will retain any data table associated with it unless
    `dropTable` is `FALSE`.

- verbose:

  `GVector`s only: Logical. Displays progress when using automatic
  topology correction.

- ...:

  Other arguments::

  - `table` (`GVector`s–useful mainly to developers, not most users): A
    `data.frame` or `data.table` with one row per geometry in a
    `GVector`. Serves as an attribute table.

  - `xVect` (`GVector`s–useful mainly to developers, not most users):
    The `SpatVector` that corresponds to the file named by `x`.

- crs:

  String: Coordinate reference system (CRS) WKT2 string. This argument
  is used for creating a `GVector` from a `numeric` vector or a
  `data.frame` or similar, or from `fast(rastOrVect = "vector")` or
  `fast(rastOrVect = "raster")`. By default, the function will use the
  value of
  [`crs()`](https://github.com/adamlilith/fasterRaster/reference/crs.md)
  (no arguments), which is the CRS of the current **GRASS**
  "project/location" (see
  [`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md)).

- keepgeom:

  Logical: If `x` is a set of `numeric` coordinates, or a `data.frame`
  or similar, then they can be coerced into a `points` `GVector`. If
  `keepgeom` is `TRUE`, then the coordinates will be included in the
  data table of the `GVector`. The default is `FALSE`.

- geom:

  Character or integer vector: If `x` is a `data.frame`, `data.table`,
  or `matrix`, this specifies which columns of `x` represent longitude
  and latitude. Columns can be given by name (a character vector) or
  index (a numeric or integer vector). The default is to use the first
  two columns of `x`.

## Value

A `GRaster` or `GVector`.

## Details

**GRASS** uses a "topological" model for vectors. Topological issues
generally arise only with polygon vectors, not point or line vectors.
Sometimes, polygons created in other software are topologically
incorrect–the borders of adjacent polygons may cross one another, or
there may be small gaps between them. These errors can be corrected by
slightly shifting vertices and/or removing small polygons that result
from intersections of larger ones that border one another. A topological
system also recognizes that boundaries to adjacent polygons are shared
by the areas, so should not be ascribed attributes that belong to both
areas (e.g., the shared border between two countries "belongs" to both
countries).

By default, `fast()` will try to correct topological errors in vectors.
There are three levels of correction, and they are not necessarily
mutually exclusive:

1.  **Automatic correction**: By default, `fast()` will apply automatic
    topology correction. You can turn this off using the
    `correct = FALSE` argument, though in most cases this is not
    recommended.

2.  ***Manual* snapping and/or area removal**: In addition to correction
    from step 1, you can cause vertices of polygons close to one another
    to be "snapped" to same place and/or polygons that are smaller than
    some threshold to be removed. Problems with mis-aligned vertices
    arise when adjacent polygons are meant to share borders, but slight
    differences in the locations of the vertices cause them to
    mis-align. This mis-alignment can also produce small "slivers" of
    polygons that are the areas where they overlap. You can snap
    vertices within a given distance of one another using the `snap`
    argument followed by a numeric value, like `snap = 0.000001`. Units
    of `snap` are in map units (usually meters) for projected coordinate
    reference systems and degrees for unprojected systems (e.g., WGS84,
    NAD83, NAD27). You can also remove polygons that are smaller than a
    particular area using the `area` argument followed by a numeric
    value (e.g., `area = 1`). The units of `area` are in meters-squared,
    regardless of the coordinate reference system. Note that using
    `snap` and `area` entails some risk, as it is possible for nearby
    vertices to actually be distinct and for small areas to be
    legitimate.

3.  **Automatic snapping and/or area removal**: In addition to the
    correction from step 1, you can use automatic `snap` and/or `area`
    correction on polygons vectors by setting `snap` and/or `area` to
    `NULL` (i.e., their default values). If just `snap` is `NULL`, only
    automatic snapping will be performed, and if just `area` is `NULL`,
    then only automatic area removal will be performed. Regardless, you
    will also need to set an integer value for `steps`, which is the
    number of steps to take between the smallest value of `snap` and/or
    `area` and the maximum value attempted. The function will then
    proceed by first attempting `snap = 0` and/or `area = 0` (i.e., no
    snapping or area removal). If this does not produce a topologically
    correct vector, **GRASS** will (internally) suggest a range for
    `snap`. The `fast()` function then creates `steps` values from the
    lowest to the highest values of this range evenly-spaced along the
    log values of this range, then proceed to repeat the importing
    process until either the vector is imported correctly or the maximum
    value of `snap` is reached and results in a failed topology. Smaller
    values of `step` will result in more fine-grained attempts so are
    less likely to yield overcorrection, but can also take more time.
    The value of `area` in automatic correction is set to `snap^2`.
    **NB**: Automated snapping and area removal are only performed on
    polygons vectors, even if `snap` or `area` is `NULL`. To snap lines
    or points, you must set `snap` equal to a numeric value. The `area`
    correction is ignored for points and lines.

Issues can also arise due to:

- **Data table-vector mismatching**: If your vector has a data table
  ("attribute table") associated with it, errors can occur if there are
  more/fewer geometries (or multi-geometries) per row in the table. If
  you do not really need the data table to do your analysis, you can
  remove it (and thus obviate this error) using `dropTable = TRUE`.

- **Dissolving or aggregating "invalid" geometries**: Using the
  `resolve` argument, you can create a topologically valid vector by
  either coercing all overlapping portions of polygons into their own
  geometries (`resolve = "disaggregate"`), or by coercing them into a
  single, combined geometry (`resolve = "aggregate"`).
  Aggregation/disaggregation will be implemented after loading the
  vector into **GRASS** using the settings given by `snap` and `area`.
  Aggregation/disaggregation will cause any associated data table to be
  dropped (it forces `dropTable` to be `TRUE`). The default action is to
  do neither aggregation nor disaggregation (`resolve = NA`). You can
  also do this outside **fasterRaster** using
  [`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)
  or
  [`terra::disagg()`](https://rspatial.github.io/terra/reference/disaggregate.html).

If none of these fixes work, you can try:

- **Correction outside of *fasterRaster***: Before you convert the
  vector into **fasterRaster**'s `GVector` format, you can also try
  using the
  [`terra::makeValid()`](https://rspatial.github.io/terra/reference/is.valid.html)
  or
  [`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
  tools to fix issues, then use `fast()`. You can also use
  [`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)
  or
  [`terra::disagg()`](https://rspatial.github.io/terra/reference/disaggregate.html)
  to combine/split problematic geometries.

- **Post-load correction**: If you do get a vector loaded into `GVector`
  format, you can also use a set of **fasterRaster** vector-manipulation
  [tools](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md)
  or
  [`fillHoles()`](https://github.com/adamlilith/fasterRaster/reference/fillHoles.md)
  to fix issues.

## See also

[`read_RAST`](https://osgeo.github.io/rgrass/reference/read_RAST.html)
and
[`read_VECT`](https://osgeo.github.io/rgrass/reference/read_VECT.html),
[vector
cleaning](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md),
[`fillHoles()`](https://github.com/adamlilith/fasterRaster/reference/fillHoles.md),
plus **GRASS** modules `v.in.ogr` (see `grassHelp("v.in.ogr")`) and
`r.import` (`grassHelp("r.import")`)

## Examples

``` r
if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev") # integer raster
madCover <- fastData("madCover") # categorical raster
madCoast4 <- fastData("madCoast4") # polygons vector
madRivers <- fastData("madRivers") # lines vector
madDypsis <- fastData("madDypsis") # points vector

### Create GRasters from SpatRasters

# Create an integer raster:
elev <- fast(madElev)
elev

# Create a categorical raster:
cover <- fast(madCover)
madCover
levels(madCover) # category levels

# Create a GRaster from a file on disk:
rastFile <- system.file("extdata", "madForest2000.tif", package = "fasterRaster")
forest2000 <- fast(rastFile)
forest2000

# Create a 1's raster that spans the world:
ones <- fast(rastOrVect = "raster", crs = "epsg:4326")
ones

### Create GVectors

# Create a GVector from an sf vector:
coast4 <- fast(madCoast4)
coast4

# Create a GVector from a SpatVector:
madRivers <- vect(madRivers)
class(madRivers)
rivers <- fast(madRivers)
rivers

# Create a GVector from a vector on disk:
vectFile <- system.file("extdata/shapes", "madCoast.shp",
   package = "fasterRaster")
coast0 <- fast(vectFile)
coast0

# Import only Dypsis occurrences in a restricted area:
ant <- coast4[coast4$NAME_4 == "Antanambe"]
dypsisRestrict <- fast(madDypsis, extent = ant)
dypsis <- fast(madDypsis)

plot(coast4)
plot(ant, col = "gray80", add = TRUE)
plot(dypsis, add = TRUE)
plot(dypsisRestrict, col = "red", add = TRUE)

# Create a generic GVector that spans the world:
wallToWall <- fast(rastOrVect = "vector", crs = "epsg:4326") # WGS84
wallToWall

# Create a GVector from a numeric vector
pts <- c(-90.2, 38.6, -122.3, 37.9)
pts <- fast(pts, crs = "epsg:4326") # WGS84

# Create a GVector from a matrix (can also use data.frame or data.table):
mat <- matrix(c(-90.2, 38.6, -122.3, 37.9), ncol = 2, byrow = TRUE)
mat <- fast(mat, crs = "epsg:4326", keepgeom = TRUE) # WGS84

}
```
