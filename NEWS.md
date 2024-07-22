# fasterRaster 8.3.0.7021 (2024-XX-XX)
**+**: Denotes potentially code-breaking changes

## New functions and functionality
o `[` (`subset_single_bracket`) can use a `GRaster` inside the `[]` to specify what cells in a `GRaster` to subset.
o `[<-` (`replace_single_square_bracket`) can use a `GRaster` inside the `[]` to specify what cells in a `GRaster` are re-assigned.
o `sineRast()` now accepts arguments for amplitude.
o `tiles()` creates spatially exclusive subsets from `GRaster`s.

## Issues and bug fixes
o `spatSample()` now works when `values = TRUE`.

## Minor
o `project()` now has a `verbose` argument for displaying progress.

# fasterRaster 8.3.0.7020 (2024-07-05)
**+**: Denotes potentially code-breaking changes

## New functions and functionality
o `sineRast()`: Creates sine wave rasters.

## Changes in functionality
o `distance()` now works for calculation of distances between two `GVector`s or a `GVector` and itself.
o **+** `extract()` and **+** `spatSample()`: Changed default value of `cats` argument to `TRUE`.
o `fragmentation()` is *much* faster for `SpatRaster`s and for both `SpatRaster`s and `GRaster`s, can display progress.
o **+** `plot()` is faster for very large rasters. Replaced argument `maxcell` with `simplify`.
o `show()` displays long raster names properly.

# fasterRaster 8.3.0.7019 (2024-06-08)

## Bug fixes
o `not.na()`: Fixed bug causing incorrect answer.

# fasterRaster 8.3.0.7018 (2024-06-07)
**+**: Denotes potentially code-breaking changes

## New functions and functionality
o **+** `spatSample()`: *Much* faster (though not actually fast...) for large samples taken from `GRaster`s. Removed argument `seed` for `GRaster` signature, and added argument `verbose` to give you something to watch.
o `freq()`: Added `function-specific example.

## Bug fixes
o `global()`: Fixed bug arising when called by other functions and main argument was a `sources()` name.

# fasterRaster 8.3.0.7017 (2024-06-02)
**+**: Denotes potentially code-breaking changes

## New functions and functionality
o `rast()`: Attaches the `GRaster`'s levels table to the `SpatRaster` output.
o **+** `rasterize()`: Rewritten to perform (nearly) the same as `terra::rasterize()`.
o `predict()`: Can accommodate models with two-way interactions between categorical rasters and between a categorical predictor and a scalar.
o `scalepop()`: Scales `GRaster`s by population standard deviation.
o Stops with a somewhat informative error when a `GRaster` fails to be created (in hidden function `.makeGRaster()`)

## Issues
o `writeRaster()`: Correctly assign `datatype` to `CELL` rasters.
O **+** `cor()` and `cov()` removed and incorporated into `layerCor()`

## Bug fixes
o `activeCat()` and `activeCats()`: Fixed bug introduced by previous fix.
o `activeCat()`: Correct output when `names = TRUE`.
o `expanse()`: Expanded list of units; correct assignation of units to **GRASS** unit format.
o `extract()`: Extracting from a `GRaster` to a `lines` or `polygons` `GVector` works.
o `fast()`: Fixed bug arising when reading vector saved by `writeRaster()`.
o + `global()`: Removed functions `"countNA"` and `"countNonNA"` from `global()` since **GRASS** module `r.report` can be mistaken.
o `nacell()` and `nonnacell()`: Correct (but slow~~~) reporting of `NA` and non-`NA` cells (workaround of error in **GRASS**'s `r.report` module).

# fasterRaster 8.3.0.7016 (2024-05-27)

## Functionality
o Added `streams()` for calculating location of stream channels from a DEM.
o Added `terrainRuggednessIndex()` for calculating the terrain ruggedness index.
o `unscale()` can skip unscaling of rasters by supplying `NA` in the `center` and/or `scale` vectors.
o `writeRaster()` will now automatically choose the "least-lossy" `datatype` for a stack of rasters.
o More robust checking of whether a vector is topologically valid or not when using `fast()`, and added option to aggregate or disaggregate polygons to overcome the issue.

## Bug fixes
o `crop()` correctly sets westernmost coordinate (was inappropriately too far west, in some cases).
o `extend()` works when the "extension" factor is a integer.
o `GRaster`s can now be multiplied by, divide by, added to, or subtracted from `numeric`s in scientific notation format.
o `hist()` now works with `factor` `GRaster`s.
o `plot()` relies on `writeRaster()` for `datatype` (which is better).
o `writeRaster()` saves all-`NA` rows and columns.

# fasterRaster 8.3.0.7015 (2024-05-21)

## Bug fixes
o `[` now works for large `GVector`s (i.e., >1M geometries).
o Fixed behind-scenes issue arising when a CRS string couldn't be parsed to a shorter version (`.locationCreate()` and related).

# fasterRaster 8.3.0.7014 (2024-05-17)

## Functionality
o Added function `flow()` for calculating flow of water across a landscape.
o Added function `flowPath()` for calculating flow of water from specific points on a landscape.
o `freq()` inserts category labels into results for for categorical `GRaster`s
o Added function `geomorphons()` for identifying geomorphological features.
o Added function `maskNA()` for converting non-`NA` cells or `NA` cells to a user-defined value.
o `plot()` displays of levels of categorical rasters.
o Can save layer-by-layer with `writeRaster()`.
o Added ability to create `points` `GVector`s from numeric, matrices, or data frames using `fast()`
o Improved auto-assessment of raster `datatype` in `writeRaster()`.
o Updated `README` for 8.3.0.7013!

## Bug fixes
o `[` works consistently for `GVector`s!!!!!
o Hidden function `.makeGVector()` now catches cases with zero extent for polygons.
o Fixed installation issue related to `activeCat()<-` and `addCats()<-` (thank you, `@kbondo1`!)
o Fixed bug in `arithmetic` when determining data type of an input raster.
o `crds()` works when the **GRASS** vector has an attribute table.
o `extract()` extracts values from `GVector`s for large numbers of points without crashing
o `plot()` works! (Previous issue arose from changing output of `writeRaster()` to `GRaster`).
o `rast()` correctly returns a `SpatRaster`.
o `vect()` correctly returns a `SpatVector`.

## Issues
o Removed `rasterPrecision` option and now use internal function `.getPrec()` to ascertain the proper precision of rasters.
o Option to fail in creation of `GRaster` or a `polygons` `GVector` if it would have a zero extent.

## Changes
o `complete.cases()` and `missing.cases()` return logical vectors for vectors with no data tables (was integer vectors).

# fasterRaster 8.3.0.7007 (2024-05-01)

## Functionality
o Added function `classify()`.
o Added function `subst()`.
o Added function `combineLevels()`.
o Added hidden function `.plot()`.
o For functions and cases where it is appropriate, the "levels" table of an input `GRaster` is passed to the output.
o `fragmentation()` works for windows sizes > 3 and for `GRaster`s.

## Bug fixes
o `writeRaster()` correctly assigns levels to categorical rasters with >1 layer
o Fixed bug in `[[<-` that passed incorrect dimensions (then failed)

## Issues
o `[` selects geometries from a `GRaster`, overcoming mis-selection by **GRASS**
o Removed `datatype()` method for signature `SpatRaster`

# fasterRaster 8.3.0.7003 (2024-03-15)

## Functionality
`rbind()` and `cbind()` work for `GVector`s.

## Bug fixes
o Fix bug setting extent for new raster in `crop()`

# fasterRaster 8.3.0.7001 (2024-03-15)
Alpha release of new, intuitive **fasterRaster** emulating and interoperable with **terra**!!!

## Breaking changes
Nearly nothing is the same in the new version of **fasterRaster** compared to version 0.7 and lower. All of the functions in previous versions have been removed.

## New features
**fasterRaster** is now compatible with **terra** and **sf** and shares functions with the same names that do (almost always) the same things (esp. with **terra**, less so with **sf**).

# fasterRaster 0.7.1 (2022-08-05a)
* Changed uses of class() t* inherits()... fixes bug in fasterFocal() (and elsewhere?)

# fasterRaster 0.7.0 (2022-06-07)
* fasterRaster can now use objects from terra and sf packages! Thanks for the suggestion, Miika!

# fasterRaster 0.6.6 (2021-11-30)
* Fixed bug in fasterHorizon(). Thanks, Forest!

# fasterRaster 0.6.5 (2021-10-13)
* Fixed bug in fasterTerrain(). Thanks, ankitsagar1!

# fasterRaster 0.6.4 (2021-06-04)
* Added path t* GRASS directory for Mac in examples

# fasterRaster 0.6.3 (2021-03-17)
* Updated documentation of example data sets

# fasterRaster 0.6.2 (2021-01-08)
* Fix bug with workers not stopping when using fasterFocal() on a Mac

# fasterRaster 0.6.0 (2020-09-04)
* Add generic function faster() t* call most GRASS modules easily
* Add fasterContour(): Contours from rasters
* Add fasterConvertDegree(): Convert degrees
* Add fasterMapcalc(): Raster calculation
* Add fasterSun(): Solar irradiation and radiation
* Add fasterSurfFractal(): Fractal raster
* Add fasterTopoidx(): Topographic wetness index
* Revealed initGrass(): Now you can use it, too!
* User can provide names of objects created by GRASS in most functions
* Update PROJ4 strings in data objects
* Update help a lot

# fasterRaster 0.5.1 (2020-09-02)
* Add fasterContour()

# fasterRaster 0.5.0 (2020-09-01)
* Updated for GRASS 7.8.

# fasterRaster 0.4.x (before 2020-09)
* Worked for Open Source Geospatial (OSGeo) GRASS 7.4
