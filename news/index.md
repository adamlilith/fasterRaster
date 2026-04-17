# Changelog

## fasterRaster 8.4.1.2 (2026-04-17)

#### New functions and functionality

o
[`makeGRaster()`](https://github.com/adamlilith/fasterRaster/reference/makeGRaster.md)
and
[`makeGVector()`](https://github.com/adamlilith/fasterRaster/reference/makeGVector.md)
are now public and allow users to create `G`-objects from files in
**GRASS**. (feature request
<https://github.com/adamlilith/fasterRaster/issues/137> – thank you,
[@JacobusS](https://github.com/JacobusS)!)  
o Updated citation to peer-reviewed journal article

#### Bug fixes

o [`app()`](https://github.com/adamlilith/fasterRaster/reference/app.md)
is more robust. Previously, short raster could have caused improper
function. (issue <https://github.com/adamlilith/fasterRaster/issues/138>
– thank you,
[@laurapoggio-sptools](https://github.com/laurapoggio-sptools)!)  
o
[`bioclims()`](https://github.com/adamlilith/fasterRaster/reference/bioclims.md)
properly calculates progress bar width so you know how long you have to
go get a coffee.  
o
[`resample()`](https://github.com/adamlilith/fasterRaster/reference/resample.md)
correctly calculates number of rows in output raster.

## fasterRaster 8.4.1.1 (2025-11-18)

CRAN release: 2025-11-18

#### New functions and functionality

o `+` can now combine any two `GVector`s with the same geometry type
(points, lines, polygons).  
o
[`installAddon()`](https://github.com/adamlilith/fasterRaster/reference/addons.md)
checks to see if addon is already installed, and if so, uninstalls it
before re-installing.  
o Minor documentation updates.

#### Bug fixes

o
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md)
correctly adds names to `GRaster`s loaded from disk when a `_names.csv`
file exists.  
o
[`geomorphons()`](https://github.com/adamlilith/fasterRaster/reference/geomorphons.md)
now works when `mode = '2'`.  
o
[`rbind()`](https://github.com/adamlilith/fasterRaster/reference/rbind.md)
combines `GVector` data tables when each vecto has the same number of
rows in its table as geometries.  
o
[`scale()`](https://github.com/adamlilith/fasterRaster/reference/scale.md)
and
[`scalepop()`](https://github.com/adamlilith/fasterRaster/reference/scale.md)
work when `center` and/or `scale` are numeric.  
o `sf` example data objects (`madCoast0`, `madCoast4`, and `madRivers`)
have had their CRSs harmonized with the example raster objects. This had
caused some of the examples not to work.

#### Other

o Added error checking in
[`fragmentation()`](https://github.com/adamlilith/fasterRaster/reference/fragmentation.md)
related to limits on size of window `w` for calculation of fragmentation
indices.

## fasterRaster 8.4.1.0 (2025-06-17)

CRAN release: 2025-06-20

#### Code-breaking changes

o `rnormRast()` is now
[`rNormRast()`](https://github.com/adamlilith/fasterRaster/reference/rnormRast.md).  
o `runifRast()` is now
[`rUnifRast()`](https://github.com/adamlilith/fasterRaster/reference/runifRast.md).

#### New functions and functionality

o
[`addons()`](https://github.com/adamlilith/fasterRaster/reference/addons.md)
now reports the names of all installed addons or whether a given addon
is installed.  
o
[`installAddon()`](https://github.com/adamlilith/fasterRaster/reference/addons.md)
installs an addon.  
o
[`removeAddon()`](https://github.com/adamlilith/fasterRaster/reference/addons.md)
deletes an addon.  
o
[`centroids()`](https://github.com/adamlilith/fasterRaster/reference/centroids.md)
now calculates centroids of clumps in a `GRaster`.  
o
[`multivarEnvSim()`](https://github.com/adamlilith/fasterRaster/reference/multivarEnvSim.md)
calculates multivariate environmental similarity (MESS).  
o
[`neighborhoodMatrix()`](https://github.com/adamlilith/fasterRaster/reference/neighborhoodMatrix.md)
generates a neighborhood matrix from a polygons `GVector`.  
o
[`rWalkRast()`](https://github.com/adamlilith/fasterRaster/reference/rWalkRast.md)
creates a raster with the path of random walkers.  
o
[`ruggedness()`](https://github.com/adamlilith/fasterRaster/reference/ruggedness.md)
now allows for calculation of the terrain ruggedness index across
user-defined windows with distance-based weighting.

#### Minor fixes

o Rebranding as per **GRASS** re-brand (haromonized logo with **GRASS**
April 2025 branding guidelines, “GRASS GIS” –\> just “GRASS”, “modules”
–\> “tools).

## fasterRaster 8.4.0.7 (2025-04-24)

CRAN release: 2025-04-25

o Removed dependency on **rpanel** because its dependency on **tclk**
did not work with **Docker** images. Replaced with version dependency on
**omnibus**’s `screenRes()` function.

## fasterRaster 8.4.0.6 (2025-03-26)

o `faster(debug = TRUE)` displays the **GRASS** command for each
**GRASS** tool called in a **fasterRaster** function.  
o `GVector[i]` works for cases with long `i`s.  
o Fixes to help pages.

## fasterRaster 8.4.0.5 (2025-02-25)

CRAN release: 2025-02-25

o Added vignette “3-dimensional objects”.  
o `[` is faster.  
o `%in%` and
[`match()`](https://github.com/adamlilith/fasterRaster/reference/match.md)
work when `faster(useDataTable = FALSE)` and `table` argument is a
character.  
o
[`extract()`](https://github.com/adamlilith/fasterRaster/reference/extract.md)
is faster.  
o
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md)
has better error catching for vectors.  
o
[`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md)
is faster when `values` or `cats` is `TRUE`.  
o Fixes issues when linking to `rgrass` and `terra` documentation noted
by R Bivand and R Hijmans.

## fasterRaster 8.4.0.3 (2024-12-15)

CRAN release: 2024-12-17

#### Bug and issue fixes

o Many minor fixes for CRAN submission!  
o Comparison between a string and a categorical `GRaster` using logical
operators like `<` or `==` returns a categorical raster.  
o
[`as.factor()`](https://rspatial.github.io/terra/reference/is.bool.html)
is now properly exported.  
o
[`centroids()`](https://github.com/adamlilith/fasterRaster/reference/centroids.md)
has the option to exit gracefully if the `addons` check fails.  
o
[`crds()`](https://github.com/adamlilith/fasterRaster/reference/crds.md)
now works for `GVector`s that lacked an internal **GRASS** database.
Hidden function `.crds()` accepts a `cats` argument, making it
potentially faster.  
o
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md)
correctly defines region on import of raster.  
o [`mow()`](https://github.com/adamlilith/fasterRaster/reference/mow.md)
example works.  
o
[`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md)
works when sampling by `stratum`.  
o `.rbind()` is a hidden function which accepts a `cats` argument that
concatenates vectors faster than
[`rbind()`](https://github.com/adamlilith/fasterRaster/reference/rbind.md).  
o Issues with some examples were fixed.

## fasterRaster 8.4.0.2 (2024-12-09)

o Fixed issues from CRAN R CMD precheck.

## fasterRaster 8.4.0.0 (2024-11-20)

#### Potentially code-breaking changes

o By default, **fasterRaster** now uses `data.frames`, not `data.table`s
from the `data.table` package (see
[`?faster`](https://github.com/adamlilith/fasterRaster/reference/faster.md)).  
o Removed option `clean` from
[`faster()`](https://github.com/adamlilith/fasterRaster/reference/faster.md).
Files are now removed from the **GRASS** cache as needed.  
o [`mow()`](https://github.com/adamlilith/fasterRaster/reference/mow.md)
can delete a single `GRaster` or `GVector`, a list of rasters and/or
vectors, or all objects in a particular environment.

#### Enhanced functionality and new functions

o
[`centroids()`](https://github.com/adamlilith/fasterRaster/reference/centroids.md)
locates the center of `GVector`s.  
o
[`coordRef()`](https://github.com/adamlilith/fasterRaster/reference/crs.md)
returns information about an object’s coordinate reference system.  
o
[`project()`](https://github.com/adamlilith/fasterRaster/reference/project.md)
is now much faster when projecting a `GRaster` using the `terra` or
`fallback` values for `res`.  
o
[`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md)
is faster.  
o Support for **GRASS** addons and methods based on them!!!

#### Bug and issue fixes

o `GVector[i]` does not fail when all geometries are selected.  
o Comparison with categorical `GRaster`s (e.g., `<`, `==`, etc.) does
not fail when `faster('useDataTable')` is `FALSE`.  
o
[`droplevels()`](https://github.com/adamlilith/fasterRaster/reference/droplevels.md)
does not fail when `faster('useDataTable')` is `FALSE`.  
o
[`levels()`](https://github.com/adamlilith/fasterRaster/reference/levels.md)
does not fail when `faster('useDataTable')` is `FALSE`.  
o
[`segregate()`](https://github.com/adamlilith/fasterRaster/reference/segregate.md)
works when setting `useDataTable` is `FALSE`.  
o
[`subset()`](https://github.com/adamlilith/fasterRaster/reference/subset.md)
no longer fails.  
o
[`subst()`](https://github.com/adamlilith/fasterRaster/reference/subst.md)
does not fail when `faster('useDataTable')` is `FALSE`.

## fasterRaster 8.4.0.7028 (2024-10-24)

#### Enhanced functionality and new functions

o
[`grassHelp()`](https://github.com/adamlilith/fasterRaster/reference/grassHelp.md)
can show the **GRASS** manual “table of contents” (argument `"toc"`).  
o
[`longlat()`](https://github.com/adamlilith/fasterRaster/reference/longlat.md)
can now return rasters with cell values equal to their coordinates in
map units (previously, only coordinates in degrees were returned).  
o For functions that are complicated or have extended references, added
a note to the `@seealso` tag to see the respective **GRASS** manual page
using
[`grassHelp()`](https://github.com/adamlilith/fasterRaster/reference/grassHelp.md).

#### Bug and issue fixes

o
[`project()`](https://github.com/adamlilith/fasterRaster/reference/project.md)
correctly restores the user’s “location” to that of the newly projected
`GRaster`.

## fasterRaster 8.4.0.7027 (2024-10-15)

#### Main task for this version

o Test examples with **GRASS 8.4** and update functions as needed.
Upgrade to **fasterRaster** 8.4.X.X.

#### Updates for **GRASS 8.4**

o `addLocationProject()` adds either a `project` or `location` argument
to a `list` to be passed to
[`rgrass::execGRASS()`](https://osgeo.github.io/rgrass/reference/execGRASS.html).  
o
[`project()`](https://github.com/adamlilith/fasterRaster/reference/project.md)
work with **GRASS** 8.4.  
o
[`.vAttachDatabase()`](https://github.com/adamlilith/fasterRaster/reference/vAttachDatabase.md)
no longer has the `"o"` flag when calling `v.db.connect` when running
**GRASS** \>=8.4.

#### Potentially code-breaking changes

o
[`aggregate()`](https://github.com/adamlilith/fasterRaster/reference/aggregate.md)
no longer has the `dissolve` argument for `GVector`s (polygons will
always be dissolved).  
o `combineCats()` has been renamed
[`concats()`](https://github.com/adamlilith/fasterRaster/reference/concats.md)
to align with **terra**.  
o `intercept()`, `slope()`, `r2()`, and `tvalue()` have been replaced by
the single function
[`regress()`](https://github.com/adamlilith/fasterRaster/reference/regress.md)
to align with **terra**.  
o `pca()` has been renamed
[`princomp()`](https://github.com/adamlilith/fasterRaster/reference/princomp.md).

#### Enhanced functionality and new functions

o
[`extract()`](https://github.com/adamlilith/fasterRaster/reference/extract.md)
now automatically projects a `GVector` to match the CRS of a `GRaster`
from which extraction is being made.  
o
[`grassGUI()`](https://github.com/adamlilith/fasterRaster/reference/grassGUI.md)
allows users to start the **GRASS** GUI.  
o
[`grassHelp()`](https://github.com/adamlilith/fasterRaster/reference/grassHelp.md)
shows the manual page for a **GRASS** tool.  
o `layerIndex()` allows a `negate` argument to get the “opposite”
indices of a `GRaster`.  
o
[`init()`](https://github.com/adamlilith/fasterRaster/reference/init.md)
assigns to `GRaster` cells the value of their coordinates, rows,
columns, or values in a regular or chessboard-like pattern.  
o
[`regress()`](https://github.com/adamlilith/fasterRaster/reference/regress.md)
replaces individual functions `intercept()`, `slope()`, `r2()`, and
`tvalue()`.  
o
[`subset()`](https://github.com/adamlilith/fasterRaster/reference/subset.md)
subsets layers of a `GRaster` or rows/geometries of a `GVector`.  
o
[`segregate()`](https://github.com/adamlilith/fasterRaster/reference/segregate.md)
creates one layer per unique value in an input `GRaster`, with values in
the output coded 1 or 0 depending on whether cells in the input had the
unique value or not.

#### Bug and issue fixes

o
[`appFuns()`](https://github.com/adamlilith/fasterRaster/reference/app.md)
succeeds in opening a **shiny** table with
[`app()`](https://github.com/adamlilith/fasterRaster/reference/app.md)
functions.  
o
[`categories()`](https://github.com/adamlilith/fasterRaster/reference/levels.md)
correctly assigns active category column.  
o
[`crds()`](https://github.com/adamlilith/fasterRaster/reference/crds.md)
correctly returns coordinates from a “points” `GVector`.  
o
[`distance()`](https://github.com/adamlilith/fasterRaster/reference/distance.md)
correctly parses distance matrix.  
o
[`simplifyGeom()`](https://github.com/adamlilith/fasterRaster/reference/simplifyGeom.md)
works for 2-dimensional `GVector`s.  
o
[`flow()`](https://github.com/adamlilith/fasterRaster/reference/flow.md)
creates a scratch folder when none is provided.  
o
[`global()`](https://github.com/adamlilith/fasterRaster/reference/global.md)
does not fail when multiple values of `fun` and `probs` are used and
`fun` includes `quantile`.  
o
[`rasterize()`](https://github.com/adamlilith/fasterRaster/reference/rasterize.md)
works when `by` is not `NULL`.  
o
[`.layerIndex()`](https://github.com/adamlilith/fasterRaster/reference/dot-layerIndex.md)
(called by
[`categories()`](https://github.com/adamlilith/fasterRaster/reference/levels.md)
and other functions related to categorical `GRaster`s) does not fail.  
o
[`.vHasDatabase()`](https://github.com/adamlilith/fasterRaster/reference/vHasDatabase.md)
correctly detects if a vector has a database attached to it.  
o Removed all instances of
[`sQuote()`](https://rdrr.io/r/base/sQuote.html).

## fasterRaster 8.3.0.7026 (2024-09-22)

o Recompile `pkgdown`

## fasterRaster 8.3.0.7025 (2024-09-19)

o Main task: Port tutorials to vignettes

#### Bug fixes

o
[`bioclims()`](https://github.com/adamlilith/fasterRaster/reference/bioclims.md)
calculates BIO55-60.

#### Other changes:

o
[`bioclims()`](https://github.com/adamlilith/fasterRaster/reference/bioclims.md)
displays progress more satisfyingly.

## fasterRaster 8.3.0.7024 (2024-09-17)

o Added `pkgdown` site!!! (Experimental…)

#### Bug fixes

o
[`bioclims()`](https://github.com/adamlilith/fasterRaster/reference/bioclims.md)
calculates BIO07 even when BIO05 and BIO06 were not explicitly called.  
o
[`faster()`](https://github.com/adamlilith/fasterRaster/reference/faster.md)
accepts a names list as an argument.

## fasterRaster 8.3.0.7023 (2024-09-15)

#### Main task of this pre-release

o Fix all issues arising from `check()`.

## fasterRaster 8.3.0.7022 (2024-09-07)

#### Main task of this pre-release

o Examples in all help files have been checked and, if needed, either
they or the calling function(s) have been fixed. See “Bug fixes and
speed-ups” below.

#### New functions and functionality

o
[`dim3d()`](https://github.com/adamlilith/fasterRaster/reference/dim.md)
returns the “region’s” dimensions when called with no arguments.  
o
[`global()`](https://github.com/adamlilith/fasterRaster/reference/global.md)
calculates quantiles much faster (minutes vs. weeks) for very large
rasters.  
o
[`layerCor()`](https://github.com/adamlilith/fasterRaster/reference/layerCor.md)
by default calculates inter-`GRaster` correlation.  
o
[`reorient()`](https://github.com/adamlilith/fasterRaster/reference/reorient.md)
converts facing angles between north and east orientations.  
o
[`terrain()`](https://github.com/adamlilith/fasterRaster/reference/terrain.md)
can return slope and aspect in radians, and allows a custom value to be
set for undefined aspects.  
o Default value of `memory` in
[`faster()`](https://github.com/adamlilith/fasterRaster/reference/faster.md)
is now 2 GB.

#### Potentially co-breaking changes

o
[`global()`](https://github.com/adamlilith/fasterRaster/reference/global.md)
argument `prob` changed to `probs` because it can accommodate more than
one value.  
o
[`horizonHeight()`](https://github.com/adamlilith/fasterRaster/reference/horizonHeight.md)
function now uses argument `step` instead of `directions`. o Removed
[`sd()`](https://rdrr.io/r/stats/sd.html) and
[`sdpop()`](https://github.com/adamlilith/fasterRaster/reference/functions.md)
and replaced with
[`stdev()`](https://github.com/adamlilith/fasterRaster/reference/functions.md).

#### Bug fixes and speed-ups

o
[`atan2()`](https://github.com/adamlilith/fasterRaster/reference/math.md)
works!  
o
[`extract()`](https://github.com/adamlilith/fasterRaster/reference/extract.md)
extracts!  
o
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md)
can convert a `SpatRaster` with one or more layers that are a subset of
a larger `SpatRaster` into a `GRaster` without error.  
o
[`fractalRast()`](https://github.com/adamlilith/fasterRaster/reference/fractalRast.md)
is faster.  
o
[`freq()`](https://github.com/adamlilith/fasterRaster/reference/freq.md)
work when the input is a categorical `GRaster`.  
p
[`interpSplines()`](https://github.com/adamlilith/fasterRaster/reference/interpSplines.md)
bug causing lambda values to not be returned fixed.  
o
[`horizonHeight()`](https://github.com/adamlilith/fasterRaster/reference/horizonHeight.md)
returns `GRaster`s that can be used directly in
[`sun()`](https://github.com/adamlilith/fasterRaster/reference/sun.md).  
o
[`plotRGB()`](https://github.com/adamlilith/fasterRaster/reference/plotRGB.md)
is no longer stuck in an infinite loop an infinite loop an infinite loop
an infinite loop an infinite loop an infinite loop an infinite loop.  
o
[`rSpatialDepRast()`](https://github.com/adamlilith/fasterRaster/reference/rSpatialDepRast.md)
is faster.  
o `replace_double_square_brackets` works!  
o
[`simplifyGeom()`](https://github.com/adamlilith/fasterRaster/reference/simplifyGeom.md)
works when using the “dp” or “dpr” methods.  
o
[`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md)
works when `byStratum = TRUE`.  
o `subset_dollar` bug fixed related to rationalization of
[`dim()`](https://github.com/adamlilith/fasterRaster/reference/dim.md)
and
[`res()`](https://github.com/adamlilith/fasterRaster/reference/res.md).  
o `subset_double_square_brackets` works for `i = missing` and `j =` not
missing.  
o `subset_single_bracket` works for `x[i, j]` when neither `i` nor `j`
are missing.  
o [`sun()`](https://github.com/adamlilith/fasterRaster/reference/sun.md)
works with `GRaster`s from
[`horizonHeight()`](https://github.com/adamlilith/fasterRaster/reference/horizonHeight.md).  
o
[`terrain()`](https://github.com/adamlilith/fasterRaster/reference/terrain.md)
works when all methods (`v = '*'`) are called.  
o
[`update()`](https://github.com/adamlilith/fasterRaster/reference/update.md)
retains a `GVector`’s data table.  
o
[`vegIndex()`](https://github.com/adamlilith/fasterRaster/reference/vegIndex.md)
fixed bug parsing `index`.  
o
[`zonal()`](https://github.com/adamlilith/fasterRaster/reference/zonal.md)
works when zones are set by a `GVector`.

## fasterRaster 8.3.0.7021 (2024-08-03)

#### Potentially co-breaking changes

o Renamed `terrainRuggednessIndex()` to
[`ruggedness()`](https://github.com/adamlilith/fasterRaster/reference/ruggedness.md).  
o Renamed `topoWetnessIndex()` to
[`wetness()`](https://github.com/adamlilith/fasterRaster/reference/wetness.md).

#### New functions and functionality

o `[` (`subset_single_bracket`) can use a `GRaster` inside the `[]` to
specify what cells in a `GRaster` to subset.  
o `[<-` (`replace_single_square_bracket`) can use a `GRaster` inside the
`[]` to specify what cells in a `GRaster` are re-assigned.  
o
[`bioclims()`](https://github.com/adamlilith/fasterRaster/reference/bioclims.md)
is a new function that calculates the “classic” and “extended” set of
BIOCLIM rasters. It works on `GRaster`s and `SpatRaster`s!  
o
[`faster()`](https://github.com/adamlilith/fasterRaster/reference/faster.md)
now has option `clean`, which enables automatic deletion of temporary
files created by functions.  
o [`mow()`](https://github.com/adamlilith/fasterRaster/reference/mow.md)
is a new function that removes unused raster and vector files from the
**GRASS** cache.  
o
[`project()`](https://github.com/adamlilith/fasterRaster/reference/project.md)
now has a `verbose` argument for displaying progress.  
o
[`sineRast()`](https://github.com/adamlilith/fasterRaster/reference/sineRast.md)
now accepts arguments for amplitude.  
o
[`tiles()`](https://github.com/adamlilith/fasterRaster/reference/tiles.md)
is a new function that creates spatially exclusive subsets from
`GRaster`s.

#### Issues and bug fixes

o
[`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md)
now works when `values = TRUE`.

## fasterRaster 8.3.0.7020 (2024-07-05)

**+**: Denotes potentially code-breaking changes

#### New functions and functionality

o
[`sineRast()`](https://github.com/adamlilith/fasterRaster/reference/sineRast.md):
Creates sine wave rasters.

#### Changes in functionality

o
[`distance()`](https://github.com/adamlilith/fasterRaster/reference/distance.md)
now works for calculation of distances between two `GVector`s or a
`GVector` and itself.  
o **+**
[`extract()`](https://github.com/adamlilith/fasterRaster/reference/extract.md)
and **+**
[`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md):
Changed default value of `cats` argument to `TRUE`.  
o
[`fragmentation()`](https://github.com/adamlilith/fasterRaster/reference/fragmentation.md)
is *much* faster for `SpatRaster`s and for both `SpatRaster`s and
`GRaster`s, can display progress.  
o **+**
[`plot()`](https://github.com/adamlilith/fasterRaster/reference/plot.md)
is faster for very large rasters. Replaced argument `maxcell` with
`simplify`.  
o
[`show()`](https://github.com/adamlilith/fasterRaster/reference/show.md)
displays long raster names properly.

## fasterRaster 8.3.0.7019 (2024-06-08)

#### Bug fixes

o
[`not.na()`](https://github.com/adamlilith/fasterRaster/reference/math.md):
Fixed bug causing incorrect answer.

## fasterRaster 8.3.0.7018 (2024-06-07)

**+**: Denotes potentially code-breaking changes

#### New functions and functionality

o **+**
[`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md):
*Much* faster (though not actually fast…) for large samples taken from
`GRaster`s. Removed argument `seed` for `GRaster` signature, and added
argument `verbose` to give you something to watch.  
o
[`freq()`](https://github.com/adamlilith/fasterRaster/reference/freq.md):
Added \`function-specific example.

#### Bug fixes

o
[`global()`](https://github.com/adamlilith/fasterRaster/reference/global.md):
Fixed bug arising when called by other functions and main argument was a
[`sources()`](https://github.com/adamlilith/fasterRaster/reference/sources.md)
name.

## fasterRaster 8.3.0.7017 (2024-06-02)

**+**: Denotes potentially code-breaking changes

#### New functions and functionality

o
[`rast()`](https://github.com/adamlilith/fasterRaster/reference/rast.md):
Attaches the `GRaster`’s levels table to the `SpatRaster` output.  
o **+**
[`rasterize()`](https://github.com/adamlilith/fasterRaster/reference/rasterize.md):
Rewritten to perform (nearly) the same as
[`terra::rasterize()`](https://rspatial.github.io/terra/reference/rasterize.html).  
o
[`predict()`](https://github.com/adamlilith/fasterRaster/reference/predict.md):
Can accommodate models with two-way interactions between categorical
rasters and between a categorical predictor and a scalar.  
o
[`scalepop()`](https://github.com/adamlilith/fasterRaster/reference/scale.md):
Scales `GRaster`s by population standard deviation.  
o Stops with a somewhat informative error when a `GRaster` fails to be
created (in hidden function
[`makeGRaster()`](https://github.com/adamlilith/fasterRaster/reference/makeGRaster.md))

#### Issues

o
[`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md):
Correctly assign `datatype` to `CELL` rasters.  
O **+** [`cor()`](https://rdrr.io/r/stats/cor.html) and
[`cov()`](https://rdrr.io/r/stats/cor.html) removed and incorporated
into
[`layerCor()`](https://github.com/adamlilith/fasterRaster/reference/layerCor.md)

#### Bug fixes

o
[`activeCat()`](https://github.com/adamlilith/fasterRaster/reference/activeCat.md)
and
[`activeCats()`](https://github.com/adamlilith/fasterRaster/reference/activeCat.md):
Fixed bug introduced by previous fix.  
o
[`activeCat()`](https://github.com/adamlilith/fasterRaster/reference/activeCat.md):
Correct output when `names = TRUE`.  
o
[`expanse()`](https://github.com/adamlilith/fasterRaster/reference/expanse.md):
Expanded list of units; correct assignation of units to **GRASS** unit
format.  
o
[`extract()`](https://github.com/adamlilith/fasterRaster/reference/extract.md):
Extracting from a `GRaster` to a `lines` or `polygons` `GVector`
works.  
o
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md):
Fixed bug arising when reading vector saved by
[`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md).  
o +
[`global()`](https://github.com/adamlilith/fasterRaster/reference/global.md):
Removed functions `"countNA"` and `"countNonNA"` from
[`global()`](https://github.com/adamlilith/fasterRaster/reference/global.md)
since **GRASS** tool `r.report` can be mistaken.  
o
[`nacell()`](https://github.com/adamlilith/fasterRaster/reference/nacell.md)
and
[`nonnacell()`](https://github.com/adamlilith/fasterRaster/reference/nacell.md):
Correct (but slow\~\~~) reporting of `NA` and non-`NA` cells (workaround
of error in **GRASS**’s `r.report` tool).

## fasterRaster 8.3.0.7016 (2024-05-27)

#### Functionality

o Added
[`streams()`](https://github.com/adamlilith/fasterRaster/reference/streams.md)
for calculating location of stream channels from a DEM.  
o Added `terrainRuggednessIndex()` for calculating the terrain
ruggedness index.  
o
[`unscale()`](https://github.com/adamlilith/fasterRaster/reference/scale.md)
can skip unscaling of rasters by supplying `NA` in the `center` and/or
`scale` vectors.  
o
[`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md)
will now automatically choose the “least-lossy” `datatype` for a stack
of rasters.  
o More robust checking of whether a vector is topologically valid or not
when using
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md),
and added option to aggregate or disaggregate polygons to overcome the
issue.

#### Bug fixes

o
[`crop()`](https://github.com/adamlilith/fasterRaster/reference/crop.md)
correctly sets westernmost coordinate (was inappropriately too far west,
in some cases).  
o
[`extend()`](https://github.com/adamlilith/fasterRaster/reference/extend.md)
works when the “extension” factor is a integer.  
o `GRaster`s can now be multiplied by, divide by, added to, or
subtracted from `numeric`s in scientific notation format.  
o
[`hist()`](https://github.com/adamlilith/fasterRaster/reference/hist.md)
now works with `factor` `GRaster`s.  
o
[`plot()`](https://github.com/adamlilith/fasterRaster/reference/plot.md)
relies on
[`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md)
for `datatype` (which is better).  
o
[`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md)
saves all-`NA` rows and columns.

## fasterRaster 8.3.0.7015 (2024-05-21)

#### Bug fixes

o `[` now works for large `GVector`s (i.e., \>1M geometries).  
o Fixed behind-scenes issue arising when a CRS string couldn’t be parsed
to a shorter version
([`.locationCreate()`](https://github.com/adamlilith/fasterRaster/reference/locationCreate.md)
and related).

## fasterRaster 8.3.0.7014 (2024-05-17)

#### Functionality

o Added function
[`flow()`](https://github.com/adamlilith/fasterRaster/reference/flow.md)
for calculating flow of water across a landscape.  
o Added function
[`flowPath()`](https://github.com/adamlilith/fasterRaster/reference/flowPath.md)
for calculating flow of water from specific points on a landscape.  
o
[`freq()`](https://github.com/adamlilith/fasterRaster/reference/freq.md)
inserts category labels into results for for categorical `GRaster`s.  
o Added function
[`geomorphons()`](https://github.com/adamlilith/fasterRaster/reference/geomorphons.md)
for identifying geomorphological features.  
o Added function
[`maskNA()`](https://github.com/adamlilith/fasterRaster/reference/maskNA.md)
for converting non-`NA` cells or `NA` cells to a user-defined value.  
o
[`plot()`](https://github.com/adamlilith/fasterRaster/reference/plot.md)
displays of levels of categorical rasters.  
o Can save layer-by-layer with
[`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md).  
o Added ability to create `points` `GVector`s from numeric, matrices, or
data frames using
[`fast()`](https://github.com/adamlilith/fasterRaster/reference/fast.md).  
o Improved auto-assessment of raster `datatype` in
[`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md).  
o Updated `README` for 8.3.0.7013!

#### Bug fixes

o `[` works consistently for `GVector`s!!!!!  
o Hidden function
[`makeGVector()`](https://github.com/adamlilith/fasterRaster/reference/makeGVector.md)
now catches cases with zero extent for polygons.  
o Fixed installation issue related to `activeCat()<-` and `addCats()<-`
(thank you, `@kbondo1`!)  
o Fixed bug in `arithmetic` when determining data type of an input
raster.  
o
[`crds()`](https://github.com/adamlilith/fasterRaster/reference/crds.md)
works when the **GRASS** vector has an attribute table.  
o
[`extract()`](https://github.com/adamlilith/fasterRaster/reference/extract.md)
extracts values from `GVector`s for large numbers of points without
crashing.  
o
[`plot()`](https://github.com/adamlilith/fasterRaster/reference/plot.md)
works! (Previous issue arose from changing output of
[`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md)
to `GRaster`).  
o
[`rast()`](https://github.com/adamlilith/fasterRaster/reference/rast.md)
correctly returns a `SpatRaster`.  
o
[`vect()`](https://github.com/adamlilith/fasterRaster/reference/vect.md)
correctly returns a `SpatVector`.

#### Issues

o Removed `rasterPrecision` option and now use internal function
`.getPrec()` to ascertain the proper precision of rasters.  
o Option to fail in creation of `GRaster` or a `polygons` `GVector` if
it would have a zero extent.

#### Changes

o
[`complete.cases()`](https://github.com/adamlilith/fasterRaster/reference/complete.cases.md)
and
[`missing.cases()`](https://github.com/adamlilith/fasterRaster/reference/complete.cases.md)
return logical vectors for vectors with no data tables (was integer
vectors).

## fasterRaster 8.3.0.7007 (2024-05-01)

#### Functionality

o Added function
[`classify()`](https://github.com/adamlilith/fasterRaster/reference/classify.md).  
o Added function
[`subst()`](https://github.com/adamlilith/fasterRaster/reference/subst.md).  
o Added function
[`combineLevels()`](https://github.com/adamlilith/fasterRaster/reference/combineLevels.md).  
o Added hidden function
[`.plot()`](https://github.com/adamlilith/fasterRaster/reference/dot-plot.md).  
o For functions and cases where it is appropriate, the “levels” table of
an input `GRaster` is passed to the output.  
o
[`fragmentation()`](https://github.com/adamlilith/fasterRaster/reference/fragmentation.md)
works for windows sizes \> 3 and for `GRaster`s.

#### Bug fixes

o
[`writeRaster()`](https://github.com/adamlilith/fasterRaster/reference/writeRaster.md)
correctly assigns levels to categorical rasters with \>1 layer.  
o Fixed bug in `[[<-` that passed incorrect dimensions (then failed).

#### Issues

o `[` selects geometries from a `GRaster`, overcoming mis-selection by
**GRASS**  
o Removed
[`datatype()`](https://github.com/adamlilith/fasterRaster/reference/datatype.md)
method for signature `SpatRaster`

## fasterRaster 8.3.0.7003 (2024-03-15)

#### Functionality

[`rbind()`](https://github.com/adamlilith/fasterRaster/reference/rbind.md)
and [`cbind()`](https://rdrr.io/r/base/cbind.html) work for `GVector`s.

#### Bug fixes

o Fix bug setting extent for new raster in
[`crop()`](https://github.com/adamlilith/fasterRaster/reference/crop.md)

## fasterRaster 8.3.0.7001 (2024-03-15)

Alpha release of new, intuitive **fasterRaster** emulating and
interoperable with **terra**!!!

#### Breaking changes

Nearly nothing is the same in the new version of **fasterRaster**
compared to version 0.7 and lower. All of the functions in previous
versions have been removed.

#### New features

**fasterRaster** is now compatible with **terra** and **sf** and shares
functions with the same names that do (almost always) the same things
(esp. with **terra**, less so with **sf**).

## fasterRaster 0.7.1 (2022-08-05a)

- Changed uses of class() t\* inherits()… fixes bug in fasterFocal()
  (and elsewhere?)

## fasterRaster 0.7.0 (2022-06-07)

- fasterRaster can now use objects from terra and sf packages! Thanks
  for the suggestion, Miika!

## fasterRaster 0.6.6 (2021-11-30)

- Fixed bug in fasterHorizon(). Thanks, Forest!

## fasterRaster 0.6.5 (2021-10-13)

- Fixed bug in fasterTerrain(). Thanks, ankitsagar1!

## fasterRaster 0.6.4 (2021-06-04)

- Added path t\* GRASS directory for Mac in examples

## fasterRaster 0.6.3 (2021-03-17)

- Updated documentation of example data sets

## fasterRaster 0.6.2 (2021-01-08)

- Fix bug with workers not stopping when using fasterFocal() on a Mac

## fasterRaster 0.6.0 (2020-09-04)

- Add generic function faster() that call most GRASS modules easily
- Add fasterContour(): Contours from rasters
- Add fasterConvertDegree(): Convert degrees
- Add fasterMapcalc(): Raster calculation
- Add fasterSun(): Solar irradiation and radiation
- Add fasterSurfFractal(): Fractal raster
- Add fasterTopoidx(): Topographic wetness index
- Revealed initGrass(): Now you can use it, too!
- User can provide names of objects created by GRASS in most functions
- Update PROJ4 strings in data objects
- Update help a lot

## fasterRaster 0.5.1 (2020-09-02)

- Add fasterContour()

## fasterRaster 0.5.0 (2020-09-01)

- Updated for GRASS 7.8.

## fasterRaster 0.4.x (before 2020-09)

- Worked for Open Source Geospatial (OSGeo) GRASS 7.4
