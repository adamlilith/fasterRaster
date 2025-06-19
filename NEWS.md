# fasterRaster 8.4.1.0 (2025-06-17)

### Code-breaking changes
o `rnormRast()` is now `rNormRast()`.  
o `runifRast()` is now `rUnifRast()`.  

### New functions and functionality
o `addons()` now reports the names of all installed addons or whether a given addon is installed.  
o `installAddon()` installs an addon.  
o `removeAddon()` deletes an addon.  
o `centroids()` now calculates centroids of clumps in a `GRaster`.  
o `multivarEnvSim()` calculates multivariate environmental similarity (MESS).  
o `neighborhoodMatrix()` generates a neighborhood matrix from a polygons `GVector`.  
o `rWalkRast()` creates a raster with the path of random walkers.  
o `ruggedness()` now allows for calculation of the terrain ruggedness index across user-defined windows with distance-based weighting.  

### Minor fixes
o Rebranding as per **GRASS** re-brand (haromonized logo with **GRASS** April 2025 branding guidelines, "GRASS GIS" --> just "GRASS", "modules" --> "tools).  

# fasterRaster 8.4.0.7 (2025-04-24)
o Removed dependency on **rpanel** because its dependency on **tclk** did not work with **Docker** images. Replaced with version dependency on **omnibus**'s `screenRes()` function.  

# fasterRaster 8.4.0.6 (2025-03-26)
o `faster(debug = TRUE)` displays the **GRASS** command for each **GRASS** tool called in a **fasterRaster** function.  
o `GVector[i]` works for cases with long `i`s.  
o Fixes to help pages.  

# fasterRaster 8.4.0.5 (2025-02-25)
o Added vignette "3-dimensional objects".  
o `[` is faster.  
o `%in%` and `match()` work when `faster(useDataTable = FALSE)` and `table` argument is a character.  
o `extract()` is faster.  
o `fast()` has better error catching for vectors.  
o `spatSample()` is faster when `values` or `cats` is `TRUE`.  
o Fixes issues when linking to `rgrass` and `terra` documentation noted by R Bivand and R Hijmans.  

# fasterRaster 8.4.0.3 (2024-12-15)

### Bug and issue fixes
o Many minor fixes for CRAN submission!  
o Comparison between a string and a categorical `GRaster` using logical operators like `<` or `==` returns a categorical raster.  
o `as.factor()` is now properly exported.  
o `centroids()` has the option to exit gracefully if the `addons` check fails.  
o `crds()` now works for `GVector`s that lacked an internal **GRASS** database. Hidden function `.crds()` accepts a `cats` argument, making it potentially faster.   
o `fast()` correctly defines region on import of raster.  
o `mow()` example works.  
o `spatSample()` works when sampling by `stratum`.  
o `.rbind()` is a hidden function which accepts a `cats` argument that concatenates vectors faster than `rbind()`.   
o Issues with some examples were fixed.  

# fasterRaster 8.4.0.2 (2024-12-09)
o Fixed issues from CRAN R CMD precheck.  

# fasterRaster 8.4.0.0 (2024-11-20)

### Potentially code-breaking changes
o By default, **fasterRaster** now uses `data.frames`, not `data.table`s from the `data.table` package (see `?faster`).  
o Removed option `clean` from `faster()`. Files are now removed from the **GRASS** cache as needed.  
o `mow()` can delete a single `GRaster` or `GVector`, a list of rasters and/or vectors, or all objects in a particular environment.  

### Enhanced functionality and new functions
o `centroids()` locates the center of `GVector`s.  
o `coordRef()` returns information about an object's coordinate reference system.  
o `project()` is now much faster when projecting a `GRaster` using the `terra` or `fallback` values for `res`.  
o `spatSample()` is faster.  
o Support for **GRASS** addons and methods based on them!!!  

### Bug and issue fixes
o `GVector[i]` does not fail when all geometries are selected.  
o Comparison with categorical `GRaster`s (e.g., `<`, `==`, etc.) does not fail when `faster('useDataTable')` is `FALSE`.  
o `droplevels()` does not fail when `faster('useDataTable')` is `FALSE`.  
o `levels()` does not fail when `faster('useDataTable')` is `FALSE`.  
o `segregate()` works when setting `useDataTable` is `FALSE`.  
o `subset()` no longer fails.  
o `subst()` does not fail when `faster('useDataTable')` is `FALSE`.  

# fasterRaster 8.4.0.7028 (2024-10-24)

### Enhanced functionality and new functions
o `grassHelp()` can show the **GRASS** manual "table of contents" (argument `"toc"`).  
o `longlat()` can now return rasters with cell values equal to their coordinates in map units (previously, only coordinates in degrees were returned).  
o For functions that are complicated or have extended references, added a note to the `@seealso` tag to see the respective **GRASS** manual page using `grassHelp()`.  

### Bug and issue fixes
o `project()` correctly restores the user's "location" to that of the newly projected `GRaster`.  

# fasterRaster 8.4.0.7027 (2024-10-15)

### Main task for this version
o Test examples with **GRASS 8.4** and update functions as needed. Upgrade to **fasterRaster** 8.4.X.X.  

### Updates for **GRASS 8.4**
o `addLocationProject()` adds either a `project` or `location` argument to a `list` to be passed to `rgrass::execGRASS()`.  
o `project()` work with **GRASS** 8.4.  
o `.vAttachDatabase()` no longer has the `"o"` flag when calling `v.db.connect` when running **GRASS** >=8.4.  

### Potentially code-breaking changes
o `aggregate()` no longer has the `dissolve` argument for `GVector`s (polygons will always be dissolved).  
o `combineCats()` has been renamed `concats()` to align with **terra**.  
o `intercept()`, `slope()`, `r2()`, and `tvalue()` have been replaced by the single function `regress()` to align with **terra**.  
o `pca()` has been renamed `princomp()`.  

### Enhanced functionality and new functions
o `extract()` now automatically projects a `GVector` to match the CRS of a `GRaster` from which extraction is being made.  
o `grassGUI()` allows users to start the **GRASS** GUI.  
o `grassHelp()` shows the manual page for a **GRASS** tool.  
o `layerIndex()` allows a `negate` argument to get the "opposite" indices of a `GRaster`.  
o `init()` assigns to `GRaster` cells the value of their coordinates, rows, columns, or values in a regular or chessboard-like pattern.  
o `regress()` replaces individual functions `intercept()`, `slope()`, `r2()`, and `tvalue()`.  
o `subset()` subsets layers of a `GRaster` or rows/geometries of a `GVector`.  
o `segregate()` creates one layer per unique value in an input `GRaster`, with values in the output coded 1 or 0 depending on whether cells in the input had the unique value or not.  

### Bug and issue fixes
o `appFuns()` succeeds in opening a **shiny** table with `app()` functions.  
o `categories()` correctly assigns active category column.  
o `crds()` correctly returns coordinates from a "points" `GVector`.  
o `distance()` correctly parses distance matrix.  
o `simplifyGeom()` works for 2-dimensional `GVector`s.  
o `flow()` creates a scratch folder when none is provided.  
o `global()` does not fail when multiple values of `fun` and `probs` are used and `fun` includes `quantile`.  
o `rasterize()` works when `by` is not `NULL`.  
o `.layerIndex()` (called by `categories()` and other functions related to categorical `GRaster`s) does not fail.  
o `.vHasDatabase()` correctly detects if a vector has a database attached to it.  
o Removed all instances of `sQuote()`.  

# fasterRaster 8.3.0.7026 (2024-09-22)

o Recompile `pkgdown`  

# fasterRaster 8.3.0.7025 (2024-09-19)

o Main task: Port tutorials to vignettes

### Bug fixes
o `bioclims()` calculates BIO55-60.  

### Other changes:
o `bioclims()` displays progress more satisfyingly.  

# fasterRaster 8.3.0.7024 (2024-09-17)

o Added `pkgdown` site!!! (Experimental...)

### Bug fixes
o `bioclims()` calculates BIO07 even when BIO05 and BIO06 were not explicitly called.  
o `faster()` accepts a names list as an argument.  

# fasterRaster 8.3.0.7023 (2024-09-15)

### Main task of this pre-release
o Fix all issues arising from `check()`.  

# fasterRaster 8.3.0.7022 (2024-09-07)

### Main task of this pre-release
o Examples in all help files have been checked and, if needed, either they or the calling function(s) have been fixed. See "Bug fixes and speed-ups" below.

### New functions and functionality
o `dim3d()` returns the "region's" dimensions when called with no arguments.  
o `global()` calculates quantiles much faster (minutes vs. weeks) for very large rasters.  
o `layerCor()` by default calculates inter-`GRaster` correlation.  
o `reorient()` converts facing angles between north and east orientations.  
o `terrain()` can return slope and aspect in radians, and allows a custom value to be set for undefined aspects.  
o Default value of `memory` in `faster()` is now 2 GB.

### Potentially co-breaking changes
o `global()` argument `prob` changed to `probs` because it can accommodate more than one value.  
o `horizonHeight()` function now uses argument `step` instead of `directions`.
o Removed `sd()` and `sdpop()` and replaced with `stdev()`.  

### Bug fixes and speed-ups
o `atan2()` works!  
o `extract()` extracts!    
o `fast()` can convert a `SpatRaster` with one or more layers that are a subset of a larger `SpatRaster` into a `GRaster` without error.  
o `fractalRast()` is faster.  
o `freq()` work when the input is a categorical `GRaster`.  
p `interpSplines()` bug causing lambda values to not be returned fixed.  
o `horizonHeight()` returns `GRaster`s that can be used directly in `sun()`.  
o `plotRGB()` is no longer stuck in an infinite loop an infinite loop an infinite loop an infinite loop an infinite loop an infinite loop an infinite loop.  
o `rSpatialDepRast()` is faster.  
o `replace_double_square_brackets` works!  
o `simplifyGeom()` works when using the "dp" or "dpr" methods.  
o `spatSample()` works when `byStratum = TRUE`.  
o `subset_dollar` bug fixed related to rationalization of `dim()` and `res()`.  
o `subset_double_square_brackets` works for `i = missing` and `j = ` not missing.  
o `subset_single_bracket` works for `x[i, j]` when neither `i` nor `j` are missing.  
o `sun()` works with `GRaster`s from `horizonHeight()`.  
o `terrain()` works when all methods (`v = '*'`) are called.  
o `update()` retains a `GVector`'s data table.  
o `vegIndex()` fixed bug parsing `index`.  
o `zonal()` works when zones are set by a `GVector`.  

# fasterRaster 8.3.0.7021 (2024-08-03)

### Potentially co-breaking changes
o Renamed `terrainRuggednessIndex()` to `ruggedness()`.  
o Renamed `topoWetnessIndex()` to `wetness()`.

### New functions and functionality
o `[` (`subset_single_bracket`) can use a `GRaster` inside the `[]` to specify what cells in a `GRaster` to subset.  
o `[<-` (`replace_single_square_bracket`) can use a `GRaster` inside the `[]` to specify what cells in a `GRaster` are re-assigned.  
o `bioclims()` is a new function that calculates the "classic" and "extended" set of BIOCLIM rasters. It works on `GRaster`s and `SpatRaster`s!  
o `faster()` now has option `clean`, which enables automatic deletion of temporary files created by functions.  
o `mow()` is a new function that removes unused raster and vector files from the **GRASS** cache.  
o `project()` now has a `verbose` argument for displaying progress.  
o `sineRast()` now accepts arguments for amplitude.  
o `tiles()` is a new function that creates spatially exclusive subsets from `GRaster`s.  

### Issues and bug fixes
o `spatSample()` now works when `values = TRUE`.

# fasterRaster 8.3.0.7020 (2024-07-05)
**+**: Denotes potentially code-breaking changes

### New functions and functionality
o `sineRast()`: Creates sine wave rasters.

### Changes in functionality
o `distance()` now works for calculation of distances between two `GVector`s or a `GVector` and itself.  
o **+** `extract()` and **+** `spatSample()`: Changed default value of `cats` argument to `TRUE`.  
o `fragmentation()` is *much* faster for `SpatRaster`s and for both `SpatRaster`s and `GRaster`s, can display progress.  
o **+** `plot()` is faster for very large rasters. Replaced argument `maxcell` with `simplify`.  
o `show()` displays long raster names properly.

# fasterRaster 8.3.0.7019 (2024-06-08)

### Bug fixes
o `not.na()`: Fixed bug causing incorrect answer.

# fasterRaster 8.3.0.7018 (2024-06-07)
**+**: Denotes potentially code-breaking changes

### New functions and functionality
o **+** `spatSample()`: *Much* faster (though not actually fast...) for large samples taken from `GRaster`s. Removed argument `seed` for `GRaster` signature, and added argument `verbose` to give you something to watch.  
o `freq()`: Added `function-specific example.

### Bug fixes
o `global()`: Fixed bug arising when called by other functions and main argument was a `sources()` name.

# fasterRaster 8.3.0.7017 (2024-06-02)
**+**: Denotes potentially code-breaking changes

### New functions and functionality
o `rast()`: Attaches the `GRaster`'s levels table to the `SpatRaster` output.  
o **+** `rasterize()`: Rewritten to perform (nearly) the same as `terra::rasterize()`.  
o `predict()`: Can accommodate models with two-way interactions between categorical rasters and between a categorical predictor and a scalar.  
o `scalepop()`: Scales `GRaster`s by population standard deviation.  
o Stops with a somewhat informative error when a `GRaster` fails to be created (in hidden function `.makeGRaster()`)

### Issues
o `writeRaster()`: Correctly assign `datatype` to `CELL` rasters.  
O **+** `cor()` and `cov()` removed and incorporated into `layerCor()`

### Bug fixes
o `activeCat()` and `activeCats()`: Fixed bug introduced by previous fix.  
o `activeCat()`: Correct output when `names = TRUE`.  
o `expanse()`: Expanded list of units; correct assignation of units to **GRASS** unit format.  
o `extract()`: Extracting from a `GRaster` to a `lines` or `polygons` `GVector` works.  
o `fast()`: Fixed bug arising when reading vector saved by `writeRaster()`.  
o + `global()`: Removed functions `"countNA"` and `"countNonNA"` from `global()` since **GRASS** tool `r.report` can be mistaken.  
o `nacell()` and `nonnacell()`: Correct (but slow~~~) reporting of `NA` and non-`NA` cells (workaround of error in **GRASS**'s `r.report` tool).

# fasterRaster 8.3.0.7016 (2024-05-27)

### Functionality
o Added `streams()` for calculating location of stream channels from a DEM.  
o Added `terrainRuggednessIndex()` for calculating the terrain ruggedness index.  
o `unscale()` can skip unscaling of rasters by supplying `NA` in the `center` and/or `scale` vectors.  
o `writeRaster()` will now automatically choose the "least-lossy" `datatype` for a stack of rasters.  
o More robust checking of whether a vector is topologically valid or not when using `fast()`, and added option to aggregate or disaggregate polygons to overcome the issue.

### Bug fixes
o `crop()` correctly sets westernmost coordinate (was inappropriately too far west, in some cases).  
o `extend()` works when the "extension" factor is a integer.  
o `GRaster`s can now be multiplied by, divide by, added to, or subtracted from `numeric`s in scientific notation format.  
o `hist()` now works with `factor` `GRaster`s.  
o `plot()` relies on `writeRaster()` for `datatype` (which is better).  
o `writeRaster()` saves all-`NA` rows and columns.

# fasterRaster 8.3.0.7015 (2024-05-21)

### Bug fixes
o `[` now works for large `GVector`s (i.e., >1M geometries).  
o Fixed behind-scenes issue arising when a CRS string couldn't be parsed to a shorter version (`.locationCreate()` and related).

# fasterRaster 8.3.0.7014 (2024-05-17)

### Functionality
o Added function `flow()` for calculating flow of water across a landscape.  
o Added function `flowPath()` for calculating flow of water from specific points on a landscape.  
o `freq()` inserts category labels into results for for categorical `GRaster`s.  
o Added function `geomorphons()` for identifying geomorphological features.  
o Added function `maskNA()` for converting non-`NA` cells or `NA` cells to a user-defined value.  
o `plot()` displays of levels of categorical rasters.  
o Can save layer-by-layer with `writeRaster()`.  
o Added ability to create `points` `GVector`s from numeric, matrices, or data frames using `fast()`.  
o Improved auto-assessment of raster `datatype` in `writeRaster()`.  
o Updated `README` for 8.3.0.7013!

### Bug fixes
o `[` works consistently for `GVector`s!!!!!  
o Hidden function `.makeGVector()` now catches cases with zero extent for polygons.  
o Fixed installation issue related to `activeCat()<-` and `addCats()<-` (thank you, `@kbondo1`!)  
o Fixed bug in `arithmetic` when determining data type of an input raster.  
o `crds()` works when the **GRASS** vector has an attribute table.  
o `extract()` extracts values from `GVector`s for large numbers of points without crashing.  
o `plot()` works! (Previous issue arose from changing output of `writeRaster()` to `GRaster`).  
o `rast()` correctly returns a `SpatRaster`.  
o `vect()` correctly returns a `SpatVector`.

### Issues
o Removed `rasterPrecision` option and now use internal function `.getPrec()` to ascertain the proper precision of rasters.  
o Option to fail in creation of `GRaster` or a `polygons` `GVector` if it would have a zero extent.

### Changes
o `complete.cases()` and `missing.cases()` return logical vectors for vectors with no data tables (was integer vectors).

# fasterRaster 8.3.0.7007 (2024-05-01)

### Functionality
o Added function `classify()`.  
o Added function `subst()`.  
o Added function `combineLevels()`.  
o Added hidden function `.plot()`.  
o For functions and cases where it is appropriate, the "levels" table of an input `GRaster` is passed to the output.  
o `fragmentation()` works for windows sizes > 3 and for `GRaster`s.

### Bug fixes
o `writeRaster()` correctly assigns levels to categorical rasters with >1 layer.  
o Fixed bug in `[[<-` that passed incorrect dimensions (then failed).

### Issues
o `[` selects geometries from a `GRaster`, overcoming mis-selection by **GRASS**  
o Removed `datatype()` method for signature `SpatRaster`

# fasterRaster 8.3.0.7003 (2024-03-15)

### Functionality
`rbind()` and `cbind()` work for `GVector`s.

### Bug fixes
o Fix bug setting extent for new raster in `crop()`

# fasterRaster 8.3.0.7001 (2024-03-15)
Alpha release of new, intuitive **fasterRaster** emulating and interoperable with **terra**!!!

### Breaking changes
Nearly nothing is the same in the new version of **fasterRaster** compared to version 0.7 and lower. All of the functions in previous versions have been removed.

### New features
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
* Add generic function faster() that call most GRASS modules easily
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
