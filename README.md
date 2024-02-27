# fasterRaster
<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![cran version](https://www.r-pkg.org/badges/version/fasterRaster)](https://cran.r-project.org/package=fasterRaster)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/fasterRaster?color=yellow)](https://r-pkg.org/pkg/fasterRaster)
![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/fasterRaster?color=lightgrey)
[![GPLv3 license](https://img.shields.io/badge/License-GPLv3-blue.svg)](http://perso.crans.org/besson/LICENSE.html)

<!-- badges: end -->

Faster raster processing in **R** using **GRASS GIS**
 
<img align="right" src="fasterRaster.png" height="230"/>  

**fasterRaster** uses the stand-alone installer of Open Source Geospatial's <a href="https://grass.osgeo.org/rgrass/">**GRASS GIS**</a> Version 8 to speed up some commonly used raster and vector operations. Most of these operations can be done using the **terra** package by Robert Hijmans, or the **sf** package by Edzer Pebesma and Roger Bivand.  However, when large-in-memory/large-on-disk rasters and vectors can still be hard for these packages to handle. Processing them can take days, and cause **R** to crash. The **fasterRaster** package attempts to address these problems by calls to **GRASS**, which can be faster. Please note that **terra** and **sf** may be faster and thus the better solution for functions that this package implements. However, in some cases **fasterRaster** is still faster!

**fasterRaster** makes heavy use of the <a href="https://cran.r-project.org/package=rgrass">**rgrass**</a> package by Roger Bivand and others, the <a href="https://cran.r-project.org/package=rgrass">**terra**</a> package by Robert Hijmans, the <a href="https://cran.r-project.org/package=sf">**sf**</a> package by Edzer Pebesma and others, and of course <a href="https://grass.osgeo.org/">**GRASS GIS**</a>, so is greatly indebted to all of these creators.

# Getting started

As of 2024/02/26, a new version of this package, **fasterRaster 8.3**, is in development. The new version works well for rasters, but not dependably for vectors. If you need to use vectors in your analysis, I recommend you do the work with rasters (to the degree possible) in **fasterRaster**, but use **terra** or **sf** to do operations on vectors. When the vectors are ready, import them into **GRASS** using `fast(vector_name)`.

To install the development version, you will need to use:

`remotes::install_github('adamlilith/fasterRaster@intuitive_fasterRaster', dependencies = TRUE)`  

To use **fasterRaster** you will musto install [GRASS version 8+](https://grass.osgeo.org/) on your operating system. You will need to use the stand-alone installer, not the Open Source Geospatial (OS Geo) installer.

## An example

We'll do a simple operation in which we add a buffer to lines representing rivers, then calculate the distance to the buffers and burn the distance values into a raster. To do this, we'll be using maps representing the middle of the eastern coast of Madagascar. We will also use the `terra` and `sf` packages.

```
library(terra)
library(sf)
library(fasterRaster)

# Get example elevation raster and rivers vector:
madElev <- fastData('madElev') # SpatRaster with elevation
madRivers <- fastData('madRivers') # sp vector with rivers

# Plot inputs:
plot(madElev)
plot(st_geometry(madRivers), col = "blue", add = TRUE)
```

To initiate a **GRASS** session in **R**, you must tell **fasterRaster** where **GRASS** is installed on your system. The installation folder will vary by operating system and maybe **GRASS** version, but will look something like this:  

```
grassDir <- "C:/Program Files/GRASS GIS 8.2" # Windows
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac OS
grassDir <- "/usr/local/grass" # Linux
```

Now, use the `faster()` function to tell **fasterRaster** where **GRASS** is installed:
```
faster(grassDir = grassDir)
```

We will now convert the raster from a `SpatRaster` to a `GRaster`, which is **fasterRaster**'s representation of a raster. This is done using the `fast()` function.
```
elev <- fast(madElev)
elev
```

Next, we'll do the same for the rivers vector. In this case, the vector is an `sf` object from the **sf** package, but we could also use a `SpatVector` from the **terra** package.
```
rivers <- fast(madRivers)
rivers
```

Now, let's add a 1000-m buffer to the rivers using `buffer()`. As much as possible, **fasterRaster** functions have the same names and same arguments as their counterparts in the **terra** package to help users who are familiar with that package.

Note, though, that the output from **fasterRaster** is not necessarily guaranteed to be the same as output from the respective functions **terra**. This is because there are different methods to do the same thing, and the developers of **GRASS** may have chosen different methods than the developers of other GIS packages.
```
# width in meters because CRS is projected
buffs <- buffer(madRivers, width = 1000)
```

Finally, let's calculate the distances between the buffered areas and all cells on the raster map using `distance()`.
```
dists <- distance(elev, buffs)
```

Finally, let's plot the output.
```
plot(dists)
plot(buffs, add = TRUE)
plot(rivers, col = "blue", add = TRUE)
```

And that's how it's done!  You can do almost anything in **fasterRaster**  you can do with **terra**. The examples above do not show the advantage of **fasterRaster** because the they do not use in large-in-memory/large-on-disk spatial datasets. For very large datasets, **fasterRaster** can be much faster! For example, for a large raster (many cells), the `distance()` function in **terra** can take many days to run and even crash **R**, whereas in **fasterRaster**, it could take just a few minutes or hours.

## Exporting `GRaster`s and `GVector`s from a **GRASS** session

You can convert a `GRaster` to a `SpatRaster` raster using `rast()`:

`terraElev <- rast(elev)`  

To convert a `GVector` to the **terra** package's `SpatVector` format or to an `sf` vector, use `vect()` or `st_as_sf()`:

```
terraRivers <- vect(rivers)
sfRivers <- st_as_sf(rivers)
```

Finally, you can use `writeRaster()` and `writeVector()` to save **fasterRaster** rasters and vectors directly to disk. This will always be faster than using `rast()`, `vect()`, or `st_as_sf()` and then saving.
```
elevTempFile <- tempfile(fileext = ".tif") # save as GeoTIFF
writeRaster(elev, elevTempFile)

vectTempFile <- tempfile(fileext = ".shp") # save as shapefile
writeRaster(rivers, vectTempFile)
```

# Functions
To see a detailed list of functions available in **fasterRaster**, attach the package and use `?fasterRaster`.

# Tips for masking **fasterRaster** faster

1. Loading rasters and vectors directly from disk using `fast()`, rather than converting **terra** or **sf** objects is faster. Why? Because if the object does not have a file to which the **R** object points, `fast()` has to save it to disk first as a GeoTIFF or GeoPackage file, then load it into **GRASS**.

2. Similarly, saving `GRaster`s and `GVector`s directly to disk will always be faster than converting them to `SpatRaster`s, `SpatVector`s, or `sf` vectors using `rast()`, `vect()`, or `st_as_sf()`, then saving them. Why? Because these functions actually save the file to disk then uses the respective function from the respective package to connect to the file.

3. Every time you switch between using a `GRaster` or `GVector` with a different coordinate reference system (CRS), **GRASS** has to spend a few second changing to that CRS. So, you can save some time by doing as much work as possible with objects in one CRS, then switching to work on objects in another CRS.

4. By default, **GRASS**/**fasterRaster** use 2 cores and 1024 MB (1 GB) of memory for functions that allow users to specify these values. You can set these to higher values using `faster()` and thus potentially speed up some calculations. Functions in newer versions of **GRASS** have more capacity to use these options, so updating **GRASS** to the latest version can help, too.

## Versioning

The latest stable version of **fasterRaster** will mirror the version of **GRASS** for which it was built and tested. For example, **fasterRaster** version 8.3 will work using **GRASS** 8.3 (and any earlier versions starting from 8.0). **fasterRaster** will also have a minor and subminor version. For example, if the **fasterRaster** version is 8.3.2.7, then the "2" refers to changes that potentially break older code developed with a prior version, and the "7" refers to a bug fix or feature update (i.e., usually a new function or added functionality to an existing one). 

## Further reading

* Robert Hijman's [**terra**](https://cran.r-project.org/package=terra) package and Edzer Pebesma's [**sf**](https://cran.r-project.org/package=sf) package are good places to start if you are not familiar with doing GIS in **R**.
* The [GRASS GIS](https://grass.osgeo.org/) website is authoritative and contains the manual on all the **GRASS** functions used in this package and more.
* The Wiki on [how to run **GRASS** in **R** or **R** in **GRASS**](https://grasswiki.osgeo.org/wiki/R_statistics/rgrass) is a good place to start if you want to become a power-user of **GRASS** in **R**.
* Roger Bivand's [**rgrass**](https://cran.r-project.org/package=rgrass) package allows users to call any **GRASS** function with all of its functionality, which in some cases is far beyond what is allowed by **fasterRaster**.



# Citation
A publication is in the works(!), but as of February 2024, there is not as of yet a package-specific citation for **fasterRaster**. However, the package was first used in:

Morelli*, T.L., Smith*, A.B., Mancini, A.N., Balko, E. A., Borgenson, C., Dolch,R., Farris, Z., Federman, S., Golden, C.D., Holmes, S., Irwin, M., Jacobs,R.L., Johnson, S., King, T., Lehman, S., Louis, E.E. Jr., Murphy, A.,Randriahaingo, H.N.T., Lucien,Randriannarimanana, H.L.L.,Ratsimbazafy, J.,Razafindratsima, O.H., and Baden, A.L. 2020. The fate of Madagascar’s rainforest habitat.  *Nature Climate Change* 10:89-96. * Equal contribution DOI: <a href="https://doi.org/10.1038/s41558-019-0647-x">https://doi.org/10.1038/s41558-019-0647-x</a>.

*Abstract*. Madagascar has experienced extensive deforestation and overharvesting, and anthropogenic climate change will compound these pressures. Anticipating these threats to endangered species and their ecosystems requires considering both climate change and habitat loss effects. The genus *Varecia* (ruffed lemurs), which is composed of two Critically Endangered forest-obligate species, can serve as a status indicator of the biodiversity eastern rainforests of Madagascar. Here, we combined decades of research to show that the suitable habitat for ruffed lemurs could be reduced by 29–59% from deforestation, 14–75% from climate change (representative concentration pathway 8.5) or 38–93% from both by 2070. If current protected areas avoid further deforestation, climate change will still reduce the suitable habitat by 62% (range: 38–83%). If ongoing deforestation continues, the suitable habitat will decline by 81% (range: 66–93%). Maintaining and enhancing the integrity of protected areas, where rates of forest loss are lower, will be essential for ensuring persistence of the diversity of the rapidly diminishing Malagasy rainforests.


~ Adam
