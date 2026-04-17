# "fasterRaster": Faster raster and spatial vector processing using "GRASS"

**fasterRaster**: Processing of large-in-memory/-on disk rasters and
spatial vectors in using **GRASS**. Most functions in the **terra** and
**sf** packages are recreated. Processing of medium-sized and smaller
spatial objects will nearly always be faster using **terra** or **sf**.
To use most of the functions you must have the stand-alone version of
**GRASS** version 8.3 or higher (not the **OSGeoW4** installer version).
Note that due to differences in how **GRASS**, **terra**, and **sf**
were implemented, results will not always be strictly comparable between
functions for the same operation.

### Most useful tutorials and functions:

- The quick-start guide to getting started with **fasterRaster**:
  [`vignette("fasterRaster", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/fasterRaster.md):

- Types of `GRaster`s:
  [`vignette("GRasters", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/GRasters.md)

- How to speed up **fasterRaster**:
  [`vignette("faster_fasterRaster", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/faster_fasterRaster.md)

- [`faster()`](https://github.com/adamlilith/fasterRaster/reference/faster.md):
  Set the directory where **GRASS** is installed on your system, and set
  or get other package-wide options. This function must be run once
  before using most **fasterRaster** functions.

- [`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md):
  Convert a `SpatRaster`, `SpatVector`, or `sf` vector to
  **fasterRaster**'s raster format (`GRaster`s) or vector format
  (`GVector`s), or load one from a file

- [`rast()`](https://github.com/adamlilith/fasterRaster/reference/rast.md),
  [`vect()`](https://github.com/adamlilith/fasterRaster/reference/vect.md),
  and
  [`st_as_sf()`](https://github.com/adamlilith/fasterRaster/reference/vect.md):
  Convert `GRaster`s and `GVector`s to `SpatRaster`s, `SpatVector`s, or
  `sf` vectors

- [`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md)
  and
  [`writeVector()`](https://github.com/adamlilith/fasterRaster/reference/writeVector.md):
  Save `GRaster`s or `GVector`s to disk

### Properties of `GRasters`

- [`crs()`](https://github.com/adamlilith/fasterRaster/reference/crs.md):
  Coordinate reference system

- [`coordRef()`](https://github.com/adamlilith/fasterRaster/reference/crs.md):
  Coordinate reference system

- [`datatype()`](https://github.com/adamlilith/fasterRaster/reference/datatype.md):
  Data type

- [`dim()`](https://github.com/adamlilith/fasterRaster/reference/dim.md)
  and
  [`dim3d()`](https://github.com/adamlilith/fasterRaster/reference/dim.md):
  Number of rows, columns, and depths

- [`ext()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  [`N()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  [`S()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  [`E()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  [`W()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  [`top()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  and
  [`bottom()`](https://github.com/adamlilith/fasterRaster/reference/ext.md):
  Spatial extent

- [`freq()`](https://github.com/adamlilith/fasterRaster/reference/freq.md):
  Frequencies of cell values in a raster

- [`is.2d()`](https://github.com/adamlilith/fasterRaster/reference/is.2d.md)
  and
  [`is.3d()`](https://github.com/adamlilith/fasterRaster/reference/is.2d.md):
  Is an object 2- or 3-dimensional?

- [`is.int()`](https://github.com/adamlilith/fasterRaster/reference/is.int.md),
  [`is.cell()`](https://github.com/adamlilith/fasterRaster/reference/is.int.md),
  [`is.float()`](https://github.com/adamlilith/fasterRaster/reference/is.int.md),
  [`is.doub()`](https://github.com/adamlilith/fasterRaster/reference/is.int.md):
  `GRaster` data type (integer/float/double)

- [`is.factor()`](https://github.com/adamlilith/fasterRaster/reference/is.int.md):
  Does a raster represent categorical data?

- [`is.lonlat()`](https://github.com/adamlilith/fasterRaster/reference/is.lonlat.md):
  Is an object projected (e.g., in WGS84)?

- [`levels()`](https://github.com/adamlilith/fasterRaster/reference/levels.md):
  Names of levels in a categorical `GRaster`

- [`minmax()`](https://github.com/adamlilith/fasterRaster/reference/minmax.md):
  Minimum and maximum values across all non-`NA` cells

- [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md):
  `GRaster` names

- [`ncol()`](https://github.com/adamlilith/fasterRaster/reference/dim.md):
  Number of columns

- [`nacell()`](https://github.com/adamlilith/fasterRaster/reference/nacell.md):
  Number of `NA` cells

- [`ncell()`](https://github.com/adamlilith/fasterRaster/reference/dim.md):
  Number of cells

- [`ncell3d()`](https://github.com/adamlilith/fasterRaster/reference/dim.md):
  Number of cells of a 3D `GRaster`

- [`ndepth()`](https://github.com/adamlilith/fasterRaster/reference/dim.md):
  Number of depths of a 3D `GRaster`

- [`nlyr()`](https://github.com/adamlilith/fasterRaster/reference/dim.md):
  Number of layers

- [`nonnacell()`](https://github.com/adamlilith/fasterRaster/reference/nacell.md):
  Number of non-`NA` cells

- [`nrow()`](https://github.com/adamlilith/fasterRaster/reference/dim.md):
  Number of rows

- [`nlevels()`](https://github.com/adamlilith/fasterRaster/reference/nlevels.md):
  Number of categories

- [`res()`](https://github.com/adamlilith/fasterRaster/reference/res.md),
  [`res3d()`](https://github.com/adamlilith/fasterRaster/reference/res.md),
  [`xres()`](https://github.com/adamlilith/fasterRaster/reference/res.md),
  [`yres()`](https://github.com/adamlilith/fasterRaster/reference/res.md),
  and
  [`zres()`](https://github.com/adamlilith/fasterRaster/reference/res.md):
  Spatial resolution

- [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md):
  Name of the raster file in the **GRASS** cache

- [`topology()`](https://github.com/adamlilith/fasterRaster/reference/topology-GSpatial-method.md):
  Dimensionally (2D or 3D)

- [`zext()`](https://github.com/adamlilith/fasterRaster/reference/ext.md):
  Vertical extent

- [`zres()`](https://github.com/adamlilith/fasterRaster/reference/res.md):
  Vertical resolution

### Functions that operate on or create `GRasters`

- [Arithmetic](https://rdrr.io/r/base/Arithmetic.html): Mathematical
  operations on `GRaster`s: `+`, `-`, `*`, `/`, `^`, `%%` (modulus),
  `%/%` (integer division)

- [Logical
  comparisons](https://github.com/adamlilith/fasterRaster/reference/Compare-methods.md):
  `<`, `<=`, `==`, `!=`, `>=`, and `>`, plus
  [`%in%`](https://github.com/adamlilith/fasterRaster/reference/match.md)
  and
  [`%notin%`](https://github.com/adamlilith/fasterRaster/reference/match.md)
  (for categorical rasters only)

- [Logical
  operators](https://github.com/adamlilith/fasterRaster/reference/Logic-methods.md):
  `|`and `&`

Mathematical functions that are applied to each layer of a `GRaster`:

- Working with `NA`s:
  [`is.na()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`not.na()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  and
  [`maskNA()`](https://github.com/adamlilith/fasterRaster/reference/maskNA.md)

- Trigonometry:
  [`sin()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`cos()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`tan()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`asin()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`acos()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`atan()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`atan2()`](https://github.com/adamlilith/fasterRaster/reference/math.md)

- Logarithms and powers:
  [`exp()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`log()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`ln()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`log1p()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`log2()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`log10()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`sqrt()`](https://github.com/adamlilith/fasterRaster/reference/math.md)

- Rounding:
  [`round()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`floor()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`ceiling()`](https://github.com/adamlilith/fasterRaster/reference/math.md),
  [`trunc()`](https://github.com/adamlilith/fasterRaster/reference/math.md)

- Signs:
  [`abs()`](https://github.com/adamlilith/fasterRaster/reference/math.md)

Mathematical functions that are applied across layers of multi-layered
`GRaster`s:

- Numeration:
  [`sum()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`count()`](https://github.com/adamlilith/fasterRaster/reference/functions.md)

- Central tendency:
  [`mean()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`mmode()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`median()`](https://github.com/adamlilith/fasterRaster/reference/functions.md)

- Dispersion:
  [`stdev()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`var()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`varpop()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`nunique()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`range()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`quantile()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`skewness()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`kurtosis()`](https://github.com/adamlilith/fasterRaster/reference/functions.md)

- Extremes:
  [`min()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`max()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`which.min()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`which.max()`](https://github.com/adamlilith/fasterRaster/reference/functions.md)

- `NA`s:
  [`allNA()`](https://github.com/adamlilith/fasterRaster/reference/functions.md),
  [`anyNA()`](https://github.com/adamlilith/fasterRaster/reference/functions.md)

Subsetting, assigning, and replacing `GRaster` layers

- [\$](https://github.com/adamlilith/fasterRaster/reference/subset_dollar.md),
  [`[[`](https://github.com/adamlilith/fasterRaster/reference/subset_double_square_brackets.md),
  or
  [`subset()`](https://github.com/adamlilith/fasterRaster/reference/subset.md):
  Subset or remove specific layers of a `GRaster`

- `[<-`: Replace values of cells of a `GRaster`

- `[[<-`: Replace specific layers of a `GRaster`

- `add<-`: Replace specific layers of a `GRaster`

Operations on `GRaster`s

- [`as.int()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md),
  [`as.float()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md),
  [`as.doub()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md):
  Change data type (integer/float/double)

- [`as.lines()`](https://github.com/adamlilith/fasterRaster/reference/as.lines.md):
  Convert a `GRaster` to a "lines" vector

- [`as.points()`](https://github.com/adamlilith/fasterRaster/reference/as.points.md):
  Convert a `GRaster` to a "points" vector

- [`as.polygons()`](https://github.com/adamlilith/fasterRaster/reference/as.polygons.md):
  Convert a `GRaster` to a "polygons" vector

- [`aggregate()`](https://github.com/adamlilith/fasterRaster/reference/aggregate.md):
  Aggregate values of `GRaster` cells into larger cells

- [`bioclims()`](https://github.com/adamlilith/fasterRaster/reference/bioclims.md):
  BIOCLIM rasters (classic set and extended set)

- [`buffer()`](https://github.com/adamlilith/fasterRaster/reference/buffer.md):
  Create a buffer around non-`NA` cells

- [`app()`](https://github.com/adamlilith/fasterRaster/reference/app.md):
  Apply a user-defined function to multiple layers of a `GRaster` (with
  helper functions
  [`appFuns()`](https://github.com/adamlilith/fasterRaster/reference/app.md)
  and
  [`appCheck()`](https://github.com/adamlilith/fasterRaster/reference/app.md))

- [`c()`](https://github.com/adamlilith/fasterRaster/reference/c.md):
  "Stack" two or more rasters

- [`cellSize()`](https://github.com/adamlilith/fasterRaster/reference/cellSize.md):
  Cell area

- [`classify()`](https://github.com/adamlilith/fasterRaster/reference/classify.md):
  Partition cell values into strata

- [`clump()`](https://github.com/adamlilith/fasterRaster/reference/clump.md):
  Group adjacent cells with similar values

- [`combineLevels()`](https://github.com/adamlilith/fasterRaster/reference/combineLevels.md):
  Combine the "levels" tables of two or more categorical `GRaster`s

- [`concats()`](https://github.com/adamlilith/fasterRaster/reference/concats.md):
  Combine values from two or more categorical and/or integer rasters by
  concatenating them

- [`crop()`](https://github.com/adamlilith/fasterRaster/reference/crop.md):
  Remove parts of a `GRaster`

- [`denoise()`](https://github.com/adamlilith/fasterRaster/reference/denoise.md):
  Remove "noise" from a `GRaster` using a principal components analysis
  (PCA)

- [`distance()`](https://github.com/adamlilith/fasterRaster/reference/distance.md):
  Distance to non-`NA` cells, or vice versa

- [`extend()`](https://github.com/adamlilith/fasterRaster/reference/extend.md):
  Add rows and columns to a `GRaster`

- [`extract()`](https://github.com/adamlilith/fasterRaster/reference/extract.md):
  Extract values from a `GRaster` at locations of a `GVector`

- [`fillNAs()`](https://github.com/adamlilith/fasterRaster/reference/fillNAs.md):
  Fill `NA` cells

- [`focal()`](https://github.com/adamlilith/fasterRaster/reference/focal.md):
  Calculate cell values based on values of nearby cells

- [`fragmentation()`](https://github.com/adamlilith/fasterRaster/reference/fragmentation.md):
  Landscape fragmentation class from Riitters et al. (2020)

- [`global()`](https://github.com/adamlilith/fasterRaster/reference/global.md):
  Summary statistics across cells of each `GRaster` layer

- [`hist()`](https://github.com/adamlilith/fasterRaster/reference/hist.md):
  Histogram of `GRaster` values

- [`interpIDW()`](https://github.com/adamlilith/fasterRaster/reference/interpIDW.md):
  Interpolate values at points to a `GRaster`

- [`kernel()`](https://github.com/adamlilith/fasterRaster/reference/kernel.md):
  Kernel density estimator of points

- [`layerCor()`](https://github.com/adamlilith/fasterRaster/reference/layerCor.md):
  Correlation or covariance between two or more `GRaster` layers

- [`mask()`](https://github.com/adamlilith/fasterRaster/reference/mask.md):
  Remove values in a `GRaster` based on values in another `GRaster` or
  vector

- [`maskNA()`](https://github.com/adamlilith/fasterRaster/reference/maskNA.md):
  Mask all non-NA cells or all NA cells

- [`match()`](https://github.com/adamlilith/fasterRaster/reference/match.md),
  [`%in%`](https://github.com/adamlilith/fasterRaster/reference/match.md),
  and
  [`%notin%`](https://github.com/adamlilith/fasterRaster/reference/match.md):
  Find which cells of a `GRaster` match or do not match certain values

- [`merge()`](https://github.com/adamlilith/fasterRaster/reference/merge.md):
  Combine two or more rasters with different extents and fill in `NA`s

- [`multivarEnvSim()`](https://github.com/adamlilith/fasterRaster/reference/multivarEnvSim.md):
  Multivariate environmental similarity surface (MESS)

- `names<-`: Assign names to a `GRaster`

- [`noise()`](https://github.com/adamlilith/fasterRaster/reference/denoise.md):
  Remove coarse-scale trends from a `GRaster`, leaving just fine-scale
  "noise"

- [`pairs()`](https://github.com/adamlilith/fasterRaster/reference/pairs.md):
  Plot correlations between `GRaster` layers

- [`pcs()`](https://github.com/adamlilith/fasterRaster/reference/pcs.md):
  Retrieve a principal components model from a PCA `GRaster` generated
  using
  [`princomp()`](https://github.com/adamlilith/fasterRaster/reference/princomp.md)

- [`plot()`](https://github.com/adamlilith/fasterRaster/reference/plot.md):
  Display a `GRaster`

- [`project()`](https://github.com/adamlilith/fasterRaster/reference/project.md):
  Change coordinate reference system and cell size

- [`predict()`](https://github.com/adamlilith/fasterRaster/reference/predict.md):
  Make predictions to a `GRaster` from a linear model or generalized
  linear model

- [`princomp()`](https://github.com/adamlilith/fasterRaster/reference/princomp.md):
  Apply a principal components analysis (PCA) to a `GRaster`

- [`regress()`](https://github.com/adamlilith/fasterRaster/reference/regress.md):
  Regression intercept, slope, r2, and t-value across each set of cells

- [`resample()`](https://github.com/adamlilith/fasterRaster/reference/resample.md):
  Change cell size

- [`reorient()`](https://github.com/adamlilith/fasterRaster/reference/reorient.md):
  Convert degrees between 'north-orientation' and 'east orientation'

- [`sampleRast()`](https://github.com/adamlilith/fasterRaster/reference/sampleRast.md):
  Randomly sample cells from a `GRaster`

- [`scale()`](https://github.com/adamlilith/fasterRaster/reference/scale.md),
  [`scalepop()`](https://github.com/adamlilith/fasterRaster/reference/scale.md),
  and
  [`unscale()`](https://github.com/adamlilith/fasterRaster/reference/scale.md):
  Subtract means and divide by standard deviations, or inverse of that

- [`selectRange()`](https://github.com/adamlilith/fasterRaster/reference/selectRange.md):
  Select values from rasters in a stack based on values in another
  `GRaster`

- [`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md):
  Randomly points from a `GRaster`

- [`stretch()`](https://github.com/adamlilith/fasterRaster/reference/stretch.md):
  Rescale values in a GRaster

- [`subst()`](https://github.com/adamlilith/fasterRaster/reference/subst.md):
  Re-assign cell values

- [`thinLines()`](https://github.com/adamlilith/fasterRaster/reference/thinLines.md):
  Reduce linear features on a `GRaster` so linear features are 1 cell
  wide

- [`tiles()`](https://github.com/adamlilith/fasterRaster/reference/tiles.md):
  Divide a `GRaster` into spatially exclusive subsets (though with
  possible overlap)

- [`trim()`](https://github.com/adamlilith/fasterRaster/reference/trim.md):
  Remove rows and columns from a `GRaster` that are all `NA`

- [`zonal()`](https://github.com/adamlilith/fasterRaster/reference/zonal.md):
  Statistics (mean, sum, etc.) on areas of a `GRaster` defined by sets
  of cells with the same values in another `GRaster`, or by geometries
  in a `GVector`

- [`zonalGeog()`](https://github.com/adamlilith/fasterRaster/reference/zonalGeog.md):
  Geographic statistics (area, perimeter, fractal dimension, etc.) for
  sets of cells with the same values

### Creating `GRaster`s *de novo*

- [`fractalRast()`](https://github.com/adamlilith/fasterRaster/reference/fractalRast.md):
  Create a fractal `GRaster`

- [`init()`](https://github.com/adamlilith/fasterRaster/reference/init.md):
  GRaster with values equal to row, column, coordinate, regular, or
  "chess"

- [`longlat()`](https://github.com/adamlilith/fasterRaster/reference/longlat.md):
  Create longitude/latitude rasters

- [`makeGRaster()`](https://github.com/adamlilith/fasterRaster/reference/makeGRaster.md):
  Create a `GRaster` from a **GRASS** raster file

- [`makeGVector()`](https://github.com/adamlilith/fasterRaster/reference/makeGVector.md):
  Create a `GVector` from a **GRASS** vector file

- [`rNormRast()`](https://github.com/adamlilith/fasterRaster/reference/rnormRast.md):
  A random `GRaster` with values drawn from a normal distribution

- [`rSpatialDepRast()`](https://github.com/adamlilith/fasterRaster/reference/rSpatialDepRast.md):
  Create a random `GRaster` with or without spatial dependence

- [`rUnifRast()`](https://github.com/adamlilith/fasterRaster/reference/runifRast.md):
  A random `GRaster` with values drawn from a uniform distribution

- [`rWalkRast()`](https://github.com/adamlilith/fasterRaster/reference/rWalkRast.md):
  Paths of random walkers

- [`sineRast()`](https://github.com/adamlilith/fasterRaster/reference/sineRast.md):
  Sine wave rasters

### Analysis of terrain and hydrology

- [`as.contour()`](https://github.com/adamlilith/fasterRaster/reference/as.contour.md):
  Contour lines from a `GRaster`

- [`flow()`](https://github.com/adamlilith/fasterRaster/reference/flow.md):
  Identify watershed basins and direction and accumulation of flow

- [`flowPath()`](https://github.com/adamlilith/fasterRaster/reference/flowPath.md):
  Path of water flow across a landscape

- [`geomorphons()`](https://github.com/adamlilith/fasterRaster/reference/geomorphons.md):
  Identify terrain feature types

- [`hillshade()`](https://github.com/adamlilith/fasterRaster/reference/hillshade.md):
  Create a hillshade `GRaster`

- [`horizonHeight()`](https://github.com/adamlilith/fasterRaster/reference/horizonHeight.md):
  Horizon height

- [`sun()`](https://github.com/adamlilith/fasterRaster/reference/sun.md):
  Solar radiance and irradiance

- [`ruggedness()`](https://github.com/adamlilith/fasterRaster/reference/ruggedness.md):
  Terrain Ruggedness Index

- [`streams()`](https://github.com/adamlilith/fasterRaster/reference/streams.md):
  Create stream network

- [`terrain()`](https://github.com/adamlilith/fasterRaster/reference/terrain.md):
  Slope, aspect, curvature, and partial slopes

- [`wetness()`](https://github.com/adamlilith/fasterRaster/reference/wetness.md):
  Topographic wetness index

### Operations on categorical (factor) `GRaster`s

- [`%in%`](https://github.com/adamlilith/fasterRaster/reference/match.md),
  and
  [`%notin%`](https://github.com/adamlilith/fasterRaster/reference/match.md):
  Mask cells that match or do not match a given category

- [`activeCat()`](https://github.com/adamlilith/fasterRaster/reference/activeCat.md)
  and
  [`activeCats()`](https://github.com/adamlilith/fasterRaster/reference/activeCat.md):
  Column(s) that defines category labels `activeCat<-`: Set column that
  defines category labels

- [`addCats()`](https://github.com/adamlilith/fasterRaster/reference/addCats.md):
  Add new columns to a "levels" table `addCats<-`: Add new rows (levels)
  to a "levels" table

- [`categories()`](https://github.com/adamlilith/fasterRaster/reference/levels.md):
  Set "levels" table for specific layers of a categorical raster

- [`catNames()`](https://github.com/adamlilith/fasterRaster/reference/catNames.md):
  Column names of each "levels" table

- [`cats()`](https://github.com/adamlilith/fasterRaster/reference/levels.md):
  "Levels" table of a categorical raster

- [`combineLevels()`](https://github.com/adamlilith/fasterRaster/reference/combineLevels.md):
  Combine the "levels" tables of two or more categorical `GRaster`s

- [`complete.cases()`](https://github.com/adamlilith/fasterRaster/reference/complete.cases.md):
  Find rows of a categorical `GRaster`'s "levels" table that have no
  `NA`s in them

- [`concats()`](https://github.com/adamlilith/fasterRaster/reference/concats.md):
  Combine categories from two or more categorical rasters by
  concatenating them

- [`droplevels()`](https://github.com/adamlilith/fasterRaster/reference/droplevels.md):
  Remove one or more levels

- [`freq()`](https://github.com/adamlilith/fasterRaster/reference/freq.md):
  Frequency of each category across cells of a raster

- [`is.factor()`](https://github.com/adamlilith/fasterRaster/reference/is.int.md):
  Is a raster categorical?

- [`levels()`](https://github.com/adamlilith/fasterRaster/reference/levels.md):
  "Levels" table of a categorical raster

- `levels<-`: Set "levels" table of a categorical raster

- [`match()`](https://github.com/adamlilith/fasterRaster/reference/match.md),
  [`%in%`](https://github.com/adamlilith/fasterRaster/reference/match.md),
  and
  [`%notin%`](https://github.com/adamlilith/fasterRaster/reference/match.md):
  Find which cells of a `GRaster` match or do not match certain category
  labels

- [`minmax()`](https://github.com/adamlilith/fasterRaster/reference/minmax.md):
  "Lowest" and "highest" category values of categorical rasters (when
  argument `levels = TRUE`)

- [`missing.cases()`](https://github.com/adamlilith/fasterRaster/reference/complete.cases.md):
  Find rows of a categorical `GRaster`'s "levels" table that have at
  least one `NA` in them

- [`missingCats()`](https://github.com/adamlilith/fasterRaster/reference/missingCats.md):
  Values that have no category assigned to them

- [`nlevels()`](https://github.com/adamlilith/fasterRaster/reference/nlevels.md):
  Number of levels

- [`segregate()`](https://github.com/adamlilith/fasterRaster/reference/segregate.md):
  Create one GRaster layer per unique value in a GRaster

- [`subst()`](https://github.com/adamlilith/fasterRaster/reference/subst.md):
  Re-assign category levels

- [`zonalGeog()`](https://github.com/adamlilith/fasterRaster/reference/zonalGeog.md):
  Geographic statistics (area, perimeter, fractal dimension, etc.) for
  sets of cells with the same values

### Analysis of remote sensing rasters

- [`compositeRGB()`](https://github.com/adamlilith/fasterRaster/reference/compositeRGB.md):
  Combine red, green, and blue color bands to make a composite `GRaster`

- [`plotRGB()`](https://github.com/adamlilith/fasterRaster/reference/plotRGB.md):
  Display a multispectral `GRaster` using red, blue, green, and alpha
  channels

- [`vegIndex()`](https://github.com/adamlilith/fasterRaster/reference/vegIndex.md):
  Vegetation indices from surface reflectance

### Functions that operate on **terra** `SpatRaster`s

- [`bioclims()`](https://github.com/adamlilith/fasterRaster/reference/bioclims.md):
  BIOCLIM rasters (classic set and extended set)

- [`fragmentation()`](https://github.com/adamlilith/fasterRaster/reference/fragmentation.md):
  Landscape fragmentation class from Riitters et al. (2020)

### Properties of `GVector`s

- [`crs()`](https://github.com/adamlilith/fasterRaster/reference/crs.md):
  Coordinate reference system

- [`coordRef()`](https://github.com/adamlilith/fasterRaster/reference/crs.md):
  Coordinate reference system

- [`datatype()`](https://github.com/adamlilith/fasterRaster/reference/datatype.md):
  Data type of fields

- [`dim()`](https://github.com/adamlilith/fasterRaster/reference/dim.md):
  Number of geometries and columns

- [`expanse()`](https://github.com/adamlilith/fasterRaster/reference/expanse.md):
  Area of polygons or length of lines

- [`ext()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  [`N()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  [`S()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  [`E()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  [`W()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  [`top()`](https://github.com/adamlilith/fasterRaster/reference/ext.md),
  and
  [`bottom()`](https://github.com/adamlilith/fasterRaster/reference/ext.md):
  Spatial extent

- [`geomtype()`](https://github.com/adamlilith/fasterRaster/reference/geomtype.md):
  Type of vector (points, lines, polygons)

- [`is.2d()`](https://github.com/adamlilith/fasterRaster/reference/is.2d.md)
  and
  [`is.3d()`](https://github.com/adamlilith/fasterRaster/reference/is.2d.md):
  Is an object 2- or 3-dimensional?

- [`is.lonlat()`](https://github.com/adamlilith/fasterRaster/reference/is.lonlat.md):
  Is an object projected (e.g., in WGS84)?

- [`is.points()`](https://github.com/adamlilith/fasterRaster/reference/geomtype.md),
  [`is.lines()`](https://github.com/adamlilith/fasterRaster/reference/geomtype.md),
  [`is.polygons()`](https://github.com/adamlilith/fasterRaster/reference/geomtype.md):
  Does a `GVector` represent points, lines, or polygons?

- [`names()`](https://github.com/adamlilith/fasterRaster/reference/names.md):
  Names of `GVector` fields

- [`ncol()`](https://github.com/adamlilith/fasterRaster/reference/dim.md):
  Number of fields

- [`ngeom()`](https://github.com/adamlilith/fasterRaster/reference/ngeom.md):
  Number of geometries (points, lines, polygons)

- [`nrow()`](https://github.com/adamlilith/fasterRaster/reference/dim.md):
  Number of rows in a vector data table

- [`nsubgeom()`](https://github.com/adamlilith/fasterRaster/reference/ngeom.md):
  Number of sub-geometries (points, lines, polygons that make up single-
  and multipart geometries)

- [`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md):
  Name of the vector file in the **GRASS** cache

- [`topology()`](https://github.com/adamlilith/fasterRaster/reference/topology-GSpatial-method.md):
  Dimensionally (2D or 3D)

- [`zext()`](https://github.com/adamlilith/fasterRaster/reference/ext.md):
  Vertical extent

### Subsetting and assigning geometries or rows and columns of `GVector`s

- [\$](https://github.com/adamlilith/fasterRaster/reference/subset_dollar.md)
  or
  [`[[`](https://github.com/adamlilith/fasterRaster/reference/subset_double_square_brackets.md):
  Subset columns of a `GVector`'s data table

- [`[`](https://github.com/adamlilith/fasterRaster/reference/subset_single_bracket.md)
  or
  [`subset()`](https://github.com/adamlilith/fasterRaster/reference/subset.md):
  Subset geometries of a `GVector`

- `$<-`: Replace specific columns of a `GVector`'s data table or add
  columns

- `addTable<-`: Add a data table to a `GVector`

- [`dropTable()`](https://github.com/adamlilith/fasterRaster/reference/addTable.md):
  Remove a `GVector`s data table

### Operations on `GVector`s

- [`aggregate()`](https://github.com/adamlilith/fasterRaster/reference/aggregate.md):
  Combine `GVector` geometries

- [`as.data.frame()`](https://github.com/adamlilith/fasterRaster/reference/as.data.frame.md):
  Convert a `GVector`'s attribute table to a `data.frame`

- [`as.data.table()`](https://github.com/adamlilith/fasterRaster/reference/as.data.frame.md):
  Convert a `GVector`'s attribute table to a `data.table`

- [`as.points()`](https://github.com/adamlilith/fasterRaster/reference/as.points.md):
  Extract vertex coordinates from a "lines" or "polygons" `GVector`

- [`buffer()`](https://github.com/adamlilith/fasterRaster/reference/buffer.md):
  Create a polygon around/inside a `GVector`

- [`clusterPoints()`](https://github.com/adamlilith/fasterRaster/reference/clusterPoints.md):
  Identify clusters of points

- [`centroids()`](https://github.com/adamlilith/fasterRaster/reference/centroids.md):
  Centroid(s) of a `GVector`

- [`colbind()`](https://github.com/adamlilith/fasterRaster/reference/colbind.md):
  Add columns to the data table of a `GVector`

- [`complete.cases()`](https://github.com/adamlilith/fasterRaster/reference/complete.cases.md):
  Find rows of a `GVector`'s data table that have no `NA`s in them

- [`connectors()`](https://github.com/adamlilith/fasterRaster/reference/connectors.md):
  Create lines connecting nearest features of two `GVector`s

- [`convHull()`](https://github.com/adamlilith/fasterRaster/reference/convHull.md):
  Minimum convex hull

- [`crds()`](https://github.com/adamlilith/fasterRaster/reference/crds.md):
  Extract coordinates of a `GVector`

- [`crop()`](https://github.com/adamlilith/fasterRaster/reference/crop.md):
  Remove parts of a `GVector`

- [`delaunay()`](https://github.com/adamlilith/fasterRaster/reference/delaunay.md):
  Delaunay triangulation

- [`disagg()`](https://github.com/adamlilith/fasterRaster/reference/disagg.md):
  Separate multipart geometries into singlepart geometries

- [`distance()`](https://github.com/adamlilith/fasterRaster/reference/distance.md):
  Distance between geometries in two `GVector`, or from a `GVector` to
  cells of a `GRaster`

- [`erase()`](https://github.com/adamlilith/fasterRaster/reference/erase.md)
  or `-`: Remove part of a `GVector` that overlaps with another

- [`expanse()`](https://github.com/adamlilith/fasterRaster/reference/expanse.md):
  Area of polygons or length of lines

- [`extract()`](https://github.com/adamlilith/fasterRaster/reference/extract.md):
  Extract values from a `GVector` at specific points

- [`grid()`](https://github.com/adamlilith/fasterRaster/reference/grid.md):
  Create a grid `GVector`

- [`head()`](https://github.com/adamlilith/fasterRaster/reference/head.md):
  First rows of a `GVector`'s data table

- [`hexagons()`](https://github.com/adamlilith/fasterRaster/reference/hexagons.md):
  Create a hexagonal grid

- [`interpIDW()`](https://github.com/adamlilith/fasterRaster/reference/interpIDW.md):
  Interpolate values at points to a `GRaster` using inverse-distance
  weighting

- [`interpSplines()`](https://github.com/adamlilith/fasterRaster/reference/interpSplines.md):
  Interpolate values at points to a `GRaster` using splines

- [`intersect()`](https://github.com/adamlilith/fasterRaster/reference/intersect.md)
  or `*`: Intersection of two `GVectors`

- [`kernel()`](https://github.com/adamlilith/fasterRaster/reference/kernel.md):
  Kernel density estimator of points

- [`missing.cases()`](https://github.com/adamlilith/fasterRaster/reference/complete.cases.md):
  Find rows of a `GVector`'s data table that have at least `NA` in them

- `names<-`: Assign names to columns of a `GVector`s data table

- [`neighborhoodMatrix()`](https://github.com/adamlilith/fasterRaster/reference/neighborhoodMatrix.md)
  and
  [`neighbourhoodMatrix()`](https://github.com/adamlilith/fasterRaster/reference/neighborhoodMatrix.md):
  Neighborhood matrix of a polygons `GVector`

- [`project()`](https://github.com/adamlilith/fasterRaster/reference/project.md):
  Change coordinate reference system

- [`rasterize()`](https://github.com/adamlilith/fasterRaster/reference/rasterize.md):
  Convert a `GVector` to a `GRaster`

- [`rbind()`](https://github.com/adamlilith/fasterRaster/reference/rbind.md):
  Combine `GVectors`

- [`simplifyGeom()`](https://github.com/adamlilith/fasterRaster/reference/simplifyGeom.md):
  Remove vertices

- [`smoothGeom()`](https://github.com/adamlilith/fasterRaster/reference/smoothGeom.md):
  Remove "angular" aspects of features

- [`st_as_sf()`](https://github.com/adamlilith/fasterRaster/reference/vect.md):
  Convert a `GVector` to a `sf` vector

- [`st_buffer()`](https://github.com/adamlilith/fasterRaster/reference/buffer.md):
  Create a polygon around/inside a `GVector`

- [`tail()`](https://github.com/adamlilith/fasterRaster/reference/head.md):
  Last rows of a `GVector`'s data table

- [`thinPoints()`](https://github.com/adamlilith/fasterRaster/reference/thinPoints.md):
  Reduce number of points in same raster cell

- [`union()`](https://github.com/adamlilith/fasterRaster/reference/union.md)
  or `+`: Combine two `GVector`s

- [`voronoi()`](https://github.com/adamlilith/fasterRaster/reference/voronoi.md):
  Voronoi tessellation

- [`xor()`](https://github.com/adamlilith/fasterRaster/reference/xor.md)
  or `/`: Select parts of polygons not shared by two `GVector`s

### Creating `GVector`s *de novo*

- [`rvoronoi()`](https://github.com/adamlilith/fasterRaster/reference/rvoronoi.md):
  Random Voronoi tesselation

### Fixing issues with `GVector`s

(See also *Details*
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md).)

- [`breakPolys()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Break topologically clean areas

- [`fillHoles()`](https://github.com/adamlilith/fasterRaster/reference/fillHoles.md):
  Fill "holes" of a `GVector`

- [`fixBridges()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Change "bridges" to "islands"

- [`fixDangles()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Change "dangles" hanging off boundaries to lines

- [`fixLines()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Break lines at intersections and lines that form closed loops

- [`remove0()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Remove all boundaries and lines with a length of 0

- [`removeAngles()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Collapse lines that diverge at an angle that is computationally
  equivalent to 0

- [`removeBridges()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Remove "bridges" to "islands"

- [`removeDangles()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Remove "dangling" lines

- [`removeDupCentroids()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Remove duplicated area centroids

- [`removeDups()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Remove duplicated features and area centroids

- [`removeSmallPolys()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Remove small polygons

- [`snap()`](https://github.com/adamlilith/fasterRaster/reference/breakPolys.md):
  Snap lines/boundaries to each other

### Converting between data types

- [`as.contour()`](https://github.com/adamlilith/fasterRaster/reference/as.contour.md):
  Convert a `GRaster` to a `GVector` representing contour lines

- [`as.doub()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md):
  Convert a `GRaster` to a double-floating point raster (**GRASS** data
  type `DCELL`)

- [`as.data.frame()`](https://github.com/adamlilith/fasterRaster/reference/as.data.frame.md):
  Convert `GVector` to a `data.frame`

- [`as.data.table()`](https://github.com/adamlilith/fasterRaster/reference/as.data.frame.md):
  Convert `GVector` to a `data.table`

- [`as.float()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md):
  Convert a `GRaster` to a floating-point raster (**GRASS** data type
  `FCELL`)

- [`as.int()`](https://github.com/adamlilith/fasterRaster/reference/as.int.md):
  Convert a `GRaster` to an integer raster (**GRASS** data type `CELL`)

- [`as.points()`](https://github.com/adamlilith/fasterRaster/reference/as.points.md),
  [`as.lines()`](https://github.com/adamlilith/fasterRaster/reference/as.lines.md),
  and
  [`as.polygons()`](https://github.com/adamlilith/fasterRaster/reference/as.polygons.md):
  Convert a `GRaster` to a `GVector`

- [`categories()`](https://github.com/adamlilith/fasterRaster/reference/levels.md)
  and `levels<-`: Convert an integer raster to a categorical ("factor")
  raster.

- [`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md):
  Convert a `SpatRaster` to a `GRaster`; a `SpatVector`, `sf` vector,
  numeric vector, `matrix`, `data.frame`, or `data.table` to a
  `GVector`; or load a vector or raster from a file

- [`rast()`](https://github.com/adamlilith/fasterRaster/reference/rast.md):
  Convert a `GRaster` to a `SpatRaster`

- [`rasterize()`](https://github.com/adamlilith/fasterRaster/reference/rasterize.md):
  Convert a `GVector` to a `GRaster`

- [`st_as_sf()`](https://github.com/adamlilith/fasterRaster/reference/vect.md):
  Convert a `GVector` to a `sf` vector

- [`vect()`](https://github.com/adamlilith/fasterRaster/reference/vect.md):
  Convert a `GVector` to a `SpatVector`

### General purpose functions

- [`addons()`](https://github.com/adamlilith/fasterRaster/reference/addons.md):
  Show installed **GRASS** addons

- [`compareGeom()`](https://github.com/adamlilith/fasterRaster/reference/compareGeom.md):
  Determine if geographic metadata is same between `GRaster`s and/or
  `GVector`s

- [`dropRows()`](https://github.com/adamlilith/fasterRaster/reference/dropRows.md):
  Remove rows from a `data.frame` or `data.table`

- [`grassGUI()`](https://github.com/adamlilith/fasterRaster/reference/grassGUI.md):
  Start the **GRASS** GUI (not recommended for most users!!!)

- [`grassHelp()`](https://github.com/adamlilith/fasterRaster/reference/grassHelp.md):
  Open the help page for a **GRASS** tool.

- [`grassInfo()`](https://github.com/adamlilith/fasterRaster/reference/grassInfo.md):
  **GRASS** version and citation

- [`grassStarted()`](https://github.com/adamlilith/fasterRaster/reference/grassStarted.md):
  Has a connection **GRASS** been made within the current **R** session?

- [`installAddon()`](https://github.com/adamlilith/fasterRaster/reference/addons.md):
  Install a **GRASS** addon

- [`mow()`](https://github.com/adamlilith/fasterRaster/reference/mow.md):
  Remove unused rasters and vectors from the **GRASS** cache

- [`reorient()`](https://github.com/adamlilith/fasterRaster/reference/reorient.md):
  Convert degrees between 'north-orientation' and 'east orientation'

- [`replaceNAs()`](https://github.com/adamlilith/fasterRaster/reference/replaceNAs.md):
  Replace `NA`s in columns of a `data.table` or `data.frame`, or in a
  vector

- [`removeAddon()`](https://github.com/adamlilith/fasterRaster/reference/addons.md):
  Delete **GRASS** addon from your system

- [`seqToSQL()`](https://github.com/adamlilith/fasterRaster/reference/seqToSQL.md):
  Format a numeric series into an SQL value call

- [`update()`](https://github.com/adamlilith/fasterRaster/reference/update.md):
  Refresh metadata in a `GRaster` or `GVector` object

### Data objects

- [`fastData()`](https://github.com/adamlilith/fasterRaster/reference/fastData.md):
  Helper function to quickly obtain example rasters and vectors

- [appFunsTable](https://github.com/adamlilith/fasterRaster/reference/appFunsTable.md)
  (see also
  [`appFuns()`](https://github.com/adamlilith/fasterRaster/reference/app.md)):
  Functions usable by the
  [`app()`](https://github.com/adamlilith/fasterRaster/reference/app.md)
  function

- [madChelsa](https://github.com/adamlilith/fasterRaster/reference/madChelsa.md):
  Climate rasters for of a portion of eastern Madagascar

- [madCoast0](https://github.com/adamlilith/fasterRaster/reference/madCoast0.md),
  [madCoast4](https://github.com/adamlilith/fasterRaster/reference/madCoast4.md),
  and
  [madCoast](https://github.com/adamlilith/fasterRaster/reference/madCoast.md):
  Borders of an eastern portion of Madagascar

- [madCover](https://github.com/adamlilith/fasterRaster/reference/madCover.md):
  Land cover raster

- [madCoverCats](https://github.com/adamlilith/fasterRaster/reference/madCoverCats.md):
  Table of land cover classes

- [madDypsis](https://github.com/adamlilith/fasterRaster/reference/madDypsis.md):
  Specimens records of species in the genus *Dypsis*

- [madElev](https://github.com/adamlilith/fasterRaster/reference/madElev.md):
  Elevation raster

- [madForest2000](https://github.com/adamlilith/fasterRaster/reference/madForest2000.md)
  and
  [madForest2014](https://github.com/adamlilith/fasterRaster/reference/madForest2014.md):
  Forest cover in 2000 and 2014

- [madLANDSAT](https://github.com/adamlilith/fasterRaster/reference/madLANDSAT.md):
  Surface reflectance in 2023

- [madPpt](https://github.com/adamlilith/fasterRaster/reference/madPpt.md),
  [madTmin](https://github.com/adamlilith/fasterRaster/reference/madTmin.md),
  [madTmax](https://github.com/adamlilith/fasterRaster/reference/madTmax.md):
  Rasters of mean monthly precipitation, and minimum and maximum
  temperature

- [madRivers](https://github.com/adamlilith/fasterRaster/reference/madRivers.md):
  Rivers vector

- [vegIndices](https://github.com/adamlilith/fasterRaster/reference/vegIndices.md):
  Vegetation indices that can be calculated using
  [`vegIndex()`](https://github.com/adamlilith/fasterRaster/reference/vegIndex.md)

### Esoteric tutorials and arcane notes

- Comparisons between `GRegion`s can be performed using the `==` and
  `!=` operators.

- Vignette on **GRASS** "projects/locations" and "mapsets":
  [`vignette("projects_mapsets", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/projects_mapsets.md)

- Vignette on **GRASS** "regions":
  [`vignette("regions", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/regions.md)

- Vignette on **GRASS** 3-dimensional `GRaster`s and `GVector`s:
  [`vignette("three_d_objects", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/three_d_objects.md)

- Vignette on **fasterRaster** hidden functions:
  [`vignette("hidden_functions", package = "fasterRaster")`](https://github.com/adamlilith/fasterRaster/articles/hidden_functions.md)

### Classes

- [GLocation](https://github.com/adamlilith/fasterRaster/reference/GLocation.md):
  Fundamental class; points to a "location/project" in **GRASS**

- [GSpatial](https://github.com/adamlilith/fasterRaster/reference/GLocation.md):
  Basic class of any spatial object

- [GRegion](https://github.com/adamlilith/fasterRaster/reference/GLocation.md):
  Points to a "region" of a "location/project" in **GRASS**

- [GRaster](https://github.com/adamlilith/fasterRaster/reference/GLocation.md):
  Raster class

- [GVector](https://github.com/adamlilith/fasterRaster/reference/GLocation.md):
  Spatial vector class

## See also

Useful links:

- <https://github.com/adamlilith/fasterRaster>

- <https://adamlilith.github.io/fasterRaster/>

- Report bugs at <https://github.com/adamlilith/fasterRaster/issues>

## Author

Adam B. Smith
