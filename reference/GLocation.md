# Classes for fasterRaster sessions, regions, rasters, and vectors

The `G`-suite of S4 classes contain pointers to **GRASS** objects or
metadata about the current **GRASS** session. Most users will manipulate
objects using these classes, but do not need to know the details.

- The `GLocation` class stores information about the **GRASS**
  "project"/"location"(see
  [`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md)),
  and coordinate reference system. Contained by all the rest.

- The `GSpatial` class contains the `GLocation` class and stores
  information about spatial objects (extent, topology) plus the name of
  the file representing it in **GRASS** (its `source`). Contained by
  `GRegion`, `GRaster`, and `GVector`.

- The `GRegion` class contains the `GSpatial` class and stores
  information about grids (dimensions and resolution). They do have
  `sources`, but these are not used (they're always `NA`). Contained by
  `GRaster`. The `GRegion` corresponds to **GRASS** "regions", though
  `GRegion` objects are not actually pointers to **GRASS** "region"
  files (see
  [`vignette("regions", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/regions.md)).

- The `GRaster` class contains the `GRegion` class and represents
  rasters. It stores information on number of layers, categories,
  min/max values, and user-friendly names. Categorical `GRaster`s are
  associated with a "levels" table for representing categorical data
  (e.g., wetlands, forest, etc.).

- The `GVector` class contains the `GSpatial` class and represents
  spatial vectors. It may or may not have an associated `data.table`
  (i.e., a `data.frame`), which contains metadata about each geometry in
  the vector.

## Value

An object of class `GLocation`, `GSpatial`, `GRegion`, `GRaster`, or
`GVector`.

## Slots

- `location`:

  Character (all classes): The **GRASS** "project"/"location" of the
  object. The default value is `default`. Can be obtained using the
  hidden function
  [`.location()`](https://github.com/adamlilith/fasterRaster/reference/location.md).
  See
  [`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md).

- `mapset`:

  Character (all classes): The **GRASS** "mapset". Default value is
  `PERMANENT`. Typically hidden to users. Can be obtained using the
  hidden function
  [`.mapset()`](https://github.com/adamlilith/fasterRaster/reference/mapset.md).
  See
  [`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md).

- `workDir`:

  Character (all classes): Directory in which **GRASS** stores files.

- `topology`:

  Character (`GSpatial` objects, including `GRegion`s, `GRaster`s, and
  `GVector`s): Valid values are `2D` (2-dimensional–most rasters and
  vectors) or `3D` (3-dimensional–e.g., LIDAR data). Can be obtained
  using
  [`topology()`](https://github.com/adamlilith/fasterRaster/reference/topology-GSpatial-method.md).

- `sources`:

  Character (`GRaster`s and `GVector`s): Name of the object in
  **GRASS**. These are typically made on-the-fly and provide the pointer
  to the object from **R** to **GRASS**. Changing them manually will
  break the connection. Can be obtained using
  [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md).

- `names`:

  Character (`GRaster`s only): Name of a raster or each raster layer in.
  Can be obtained using
  [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md).

- `crs`:

  Character (all classes): Coordinate reference systems string
  (preferably in WKT2 format). Can be obtained using
  [`crs()`](https://github.com/adamlilith/fasterRaster/reference/crs.md)
  or
  [`st_crs()`](https://github.com/adamlilith/fasterRaster/reference/crs.md).

- `projection`:

  Character: The **GRASS** "projection" for a `GRaster` or `GVector`.
  Can be obtained using
  [`.projection()`](https://github.com/adamlilith/fasterRaster/reference/dot-projection.md).

- `dimensions`:

  Dimensions:

  - `GRegion`s and `GRaster`s: Vector of three integers indicating
    number of rows, columns, and depths (for 3D objects). Can be
    obtained using
    [`dim()`](https://github.com/adamlilith/fasterRaster/reference/dim.md),
    plus
    [`nrow()`](https://github.com/adamlilith/fasterRaster/reference/dim.md),
    [`ncol()`](https://github.com/adamlilith/fasterRaster/reference/dim.md),
    and
    [`ndepth()`](https://github.com/adamlilith/fasterRaster/reference/dim.md).

  - `GVectors`s: Vector of two integers indicating number of geometries
    and number of fields. Can be obtained using
    [`dim()`](https://github.com/adamlilith/fasterRaster/reference/dim.md),
    plus
    [`nrow()`](https://github.com/adamlilith/fasterRaster/reference/dim.md)
    and
    [`ncol()`](https://github.com/adamlilith/fasterRaster/reference/dim.md).

- `extent`:

  Numeric vector with four values (`GSpatial` objects, including
  `GRegion`s, `GRaster`s, and `GVector`s): Extent of the object listed
  in order from westernmost longitude, easternmost longitude,
  southernmost latitude, northernmost latitude. Can be obtained using
  [`ext()`](https://github.com/adamlilith/fasterRaster/reference/ext.md).

- `zextent`:

  Numeric (`GSpatial` objects, including `GRegion`s, `GRaster`s, and
  `GVector`s): Bottom- and top-most extents of 3D `GRaster`s and
  `GVector`s. Can be obtained using
  [`zext()`](https://github.com/adamlilith/fasterRaster/reference/ext.md).

- `geometry`:

  Character (`GVectors`s): Either `points`, `lines`, or `polygons`. Can
  be obtained using
  [`geomtype()`](https://github.com/adamlilith/fasterRaster/reference/geomtype.md).

- `nLayers`:

  Integer (`GRaster`s): Number of layers ("stacked" rasters–different
  from number of depths of 3D rasters). Can be obtained using
  [`nlyr()`](https://github.com/adamlilith/fasterRaster/reference/dim.md).

- `nGeometries`:

  Integer (`GVector`s): Number of features (points, lines, or polygons).
  Can be obtained using
  [`nrow()`](https://github.com/adamlilith/fasterRaster/reference/dim.md).

- `datatypeGRASS`:

  Character (`GRaster`s): Type of data stored in a raster, as
  interpreted by `GRASS`. This is either `CELL` (integers), `FCELL`
  (floating-point values), or `DCELL` (double-values). Can be obtained
  using
  [`datatype()`](https://github.com/adamlilith/fasterRaster/reference/datatype.md).

- `resolution`:

  Vector of two numeric values (`GRegion`s, including `GRaster`s): Size
  of a raster cell in the east-west direction and in the north-south
  direction. Can be obtained using
  [`res()`](https://github.com/adamlilith/fasterRaster/reference/res.md)
  and
  [`res3d()`](https://github.com/adamlilith/fasterRaster/reference/res.md).

- `minVal,maxVal`:

  Numeric (`GRaster`s): Minimum and maximum value across all cells. Can
  be obtained using
  [`minmax()`](https://github.com/adamlilith/fasterRaster/reference/minmax.md).

- `activeCat`:

  Integer (`GRaster`s): Column index of the category labels. Must be
  \>0. Note that from the user's standpoint, 1 is subtracted from this
  number. So a value if `@activeCat` is `2`, then the user would see "1"
  when printed. Can be obtained using
  [`activeCat()`](https://github.com/adamlilith/fasterRaster/reference/activeCat.md).

- `levels`:

  List of `data.table`s (`GRaster`s): Tables for categorical rasters. If
  a raster is not categorical, the `data.table` is `NULL`, as in
  `data.table(NULL)`. Can be obtained using
  [`levels()`](https://github.com/adamlilith/fasterRaster/reference/levels.md)
  or
  [`cats()`](https://github.com/adamlilith/fasterRaster/reference/levels.md).

- `table`:

  `data.table` (`GVector`s): Table with metadata, one row per geometry
  (point, line, or plane). If no table is associated with the vector,
  this must be `data.table(NULL)`. The column with the category value is
  given in `@catName`.

- `catName`:

  Character (`GVector`s): Name of the column in the vector's database
  that contains category values (integers).
