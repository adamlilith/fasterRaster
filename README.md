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

**fasterRaster** uses the stand-alone installer of Open Source Geospatial's <a href="https://grass.osgeo.org/rgrass/">**GRASS GIS**</a> Version 8 to speed up some commonly used raster and vector operations. Most of these operations can be done using the **raster** or newer **terra** packages by Robert Hijmans, or the **rgeos** or newer **sf** packages.  However, when the input raster or vector is very large in memory, in some cases functions in those packages can take a long time and fail. The **fasterRaster** package attempts to address these problems by calls to **GRASS** which is faster. Please note that **terra** and **sf** may be faster and thus the better solution for functions that this package implements. However, in some cases **fasterRaster** is still faster!

**fasterRaster** makes heavy use of the <a href="https://cran.r-project.org/package=rgrass">**rgrass**</a> package by Roger Bivand and others, the <a href="https://cran.r-project.org/package=rgrass">**terra**</a> package by Robert Hijmans, the <a href="https://cran.r-project.org/package=sf">**sf**</a> package by Edzer Pebesma and others, and of course <a href="https://grass.osgeo.org/">**GRASS GIS**</a>, so is heavily indebted to all of these creators

# Getting started #

## Installation ###

You can install **fasterRaster** from CRAN.  Alternatively, you can get the latest development version from **GitHub** using:  

`remotes::install_github('adamlilith/fasterRaster', dependencies=TRUE)`  

To use **fasterRaster** you will also need to install [GRASS version 8+](https://grass.osgeo.org/) on your operating system. You will need to use the stand-alone installer, not the Open Source Geospatial (OS Geo) installer.

## The `grassDir` argument

You will need to know the install path for **GRASS** and supply it to functions through an argument named `grassDir`. There are two ways to supply this argument. First, you can pass this to each function that needs it. For example, you can call functions like this:  

`grassDir <- 'C:/Program Files/GRASS GIS 8.2' # PC`  
`grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # Mac`  
`grassDir <- '/usr/local/grass' # Linux... maybe`  

## An example ##

Let's get started! We'll do a simple operation in which we calculate the distance to rivers (represented by a spatial lines vector object) and burn the distance values into a raster. To do this, we'll be using maps representing the middle of the eastern coast of Madagascar. We will also use the `terra` and `sf` packages for raster and spatial vector support, respectively (**fasterRaster** also works with `terra`'s `SpatVector` class, and in fact "prefers" it).

`library(fasterRaster)`  
`library(terra)`  
`library(sf)`  

Now, we need to start a **GRASS** "session". To do this, we run the `faster()` function, which makes the connection to **GRASS** and initiates the "session". We need to supply this function the coordinate reference system (CRS) we'll be using.  Here, we'll get that from a raster that comes with **FasterRaster**.

```
# get raster and vector
madElev <- fastData('madElev') # SpatRaster with elevation
madRivers <- fastData('madRivers') # sp vector with rivers

# make a temporary working directory
workDir <- tempdir()
workDir <- forwardSlash(workDir)
faster(madElev, grassDir = grassDir, location = 'examples', workDir=workDir)
```

This can take a few seconds. Normally, we don't need to use the `location = 'examples'` argument (the default value is `'default'`), but we'll do this here in case you already have a **GRASS** session started and don't want to conflict with it.

We will now convert the raster from a `SpatRaster` to a `GRaster`, which is **FasterRaster**'s representation of a raster. This is done using the `fast()` function.  This function can also convert `stars` rasters from the **stars** package.
```
elev <- fast(madElev)
```

Next, we'll do the same for the rivers vector. In this case, the vector is an `sf` object from the **sf** package, but we could also use a `SpatVector` from the **terra** package.
```
rivers <- fast(madRivers)
```

(This raster and this vector are fairly small, so the operations above went fairly fast. However, if you have big-in-memory/big-on-disk objects, importing them into **R** through the **terra**, **sf**, or **stars**  packages then into **fasterRaster** using `fast()` can be slow. You can instead use `fast()` to load a file with a raster or vector directly into the **GRASS** session. This is usually faster!)

Now, let's add a 1000-m buffer to the rivers using `buffer()`. As much as possible, **fasterRaster** functions have the same names and same arguments as their counterparts in the **terra** and **sf** packages. This is to help users who are familiar with one or both of those packages. Note, though, that the output from **FasterRaster** is not necessarily guaranteed to be the same as output from the respective functions in other packages. This is because there are different methods to do the same thing, and the developers of **GRASS** may have chosen different methods than the developers of other GIS packages.

```
buffs <- buffer(madRivers, width=1000)
```

Finally, let's calculate the distances between the buffered areas and all cells on the raster map using `distance()`.
```
dists <- distance(elev, buffs)
```

A certain weakness of **fasterRaster** is that it lacks in plotting capabilities. For the meantime, we have to convert **fasterRaster** objects back to their **terra**/**stars**/**sf** counterparts and plot them. We do the conversion using the functions from each package that are used to create rasters or vectors.  This includes `rast()` and `stars()` to create rasters, `vect()` and `st_as_sf()` to create vectors (these are actually aliases of those functions from the respective packages).
```
buffsTerra <- vect(buffs)
distsTerra <- vect(dists)

plot(distsTerra)
plot(buffsTerra, add=TRUE)
```

And that's how it's done!  You can do almost anything in **fasterRaster**  you can do with the other packages. The examples above do not show the advantage of **FasterRaster** because the they are not based in large-in-memory/large-on-disk spatial datasets. For very large datasets, **fasterRaster** can be much faster!

# Functions
If you want a detailed list of functions available in **fasterRaster**, attach the package and use `?fasterRaster`.

# Citation #
As of December, 2022, there is not a package-specific citation for **fasterRaster**, but the package was first used in:

Morelli*, T.L., Smith*, A.B., Mancini, A.N., Balko, E. A., Borgenson, C., Dolch,R., Farris, Z., Federman, S., Golden, C.D., Holmes, S., Irwin, M., Jacobs,R.L., Johnson, S., King, T., Lehman, S., Louis, E.E. Jr., Murphy, A.,Randriahaingo, H.N.T., Lucien,Randriannarimanana, H.L.L.,Ratsimbazafy, J.,Razafindratsima, O.H., and Baden, A.L. 2020. The fate of Madagascar’s rainforest habitat.  *Nature Climate Change* 10:89-96. * Equal contribution DOI: <a href="https://doi.org/10.1038/s41558-019-0647-x">https://doi.org/10.1038/s41558-019-0647-x</a>.

*Abstract*. Madagascar has experienced extensive deforestation and overharvesting, and anthropogenic climate change will compound these pressures. Anticipating these threats to endangered species and their ecosystems requires considering both climate change and habitat loss effects. The genus Varecia (ruffed lemurs), which is composed of two Critically Endangered forest-obligate species, can serve as a status indicator of the biodiverse eastern rainforest of Madagascar. Here, we combined decades of research to show that the suitable habitat for ruffed lemurs could be reduced by 29–59% from deforestation, 14–75% from climate change (representative concentration pathway 8.5) or 38–93% from both by 2070. If current protected areas avoid further deforestation, climate change will still reduce the suitable habitat by 62% (range: 38–83%). If ongoing deforestation continues, the suitable habitat will decline by 81% (range: 66–93%). Maintaining and enhancing the integrity of protected areas, where rates of forest loss are lower, will be essential for ensuring persistence of the diversity of the rapidly diminishing Malagasy rainforests.


~ Adam
