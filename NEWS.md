# fasterRaster 8.3.0.7008 (2024-05-02)

## Bug fixes
o Fixed installation issue related to `activeCat()<-` (thank you, `kbondo1`!)

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
