# fasterRaster 8.3.0.7013 (2024-05-16)

## Functionality
o Added function `flow()` for calculating flow of water across a landscape.
o Added function `flowPath()` for calculating flow of water from specific points on a landscape.
o Added function `geomorphons()` for identifying geomorphological features.
o Added function `maskNA()` for converting non-`NA` cells or `NA` cells to a user-defined value.
o `plot()` displays of levels of categorical rasters.
o Can save layer-by-layer with `writeRaster()`.
o Added ability to create `points` `GVector`s from numeric, matrices, or data frames using `fast()`
o Improved auto-assessment of raster `datatype` in `writeRaster()`.

## Bug fixes
o `[` works consistently for `GVector`s!!!!!
o Hidden function `.makeGVector()` now catches cases with zero extent for polygons.
o Fixed installation issue related to `activeCat()<-` and `addCats()<-` (thank you, `@kbondo1`!)
o Fixed bug in `arithmetic` when determining data type of an input raster.
o `crds()` works when the **GRASS** vector has an attribute table.
o `extract()` extracts values from `GVector`s for large numbers of points without crashing
o `plot()` works! (Previous issue arose fromm changing output of `writeRaster()` to `GRaster`).
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
