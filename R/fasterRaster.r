#' "fasterRaster": Faster raster and spatial vector processing using "GRASS GIS"
#'
#' @description **fasterRaster**: Processing of large-in-memory/-on disk rasters and spatial vectors in using **GRASS GIS**. Most functions in the **terra** and **sf** packages are recreated. Processing of medium-sized and smaller spatial objects will nearly always be faster using **terra** or **sf**. To use most of the functions you must have the stand-alone version of **GRASS GIS** version 8.3 or higher (not the **OSGeoW4** installer version). Note that due to differences in how **GRASS**, **terra**, and **sf** were implemented, results will not always be strictly comparable between functions for the same operation.
#'
#' ## Most useful tutorials and functions:
#' * The quick-start guide to getting started with **fasterRaster**: `vignette("fasterRaster", package = "fasterRaster")`: 
#' * Types of `GRaster`s: `vignette("GRasters", package = "fasterRaster")`
#' * How to speed up **fasterRaster**: `vignette("faster_fasterRaster", package = "fasterRaster")`
#' * Using functions that depend on **GRASS** addons: `vignette("addons", package = "fasterRaster")`
#' * [faster()]: Set the directory where **GRASS** is installed on your system, and set or get other package-wide options. This function must be run once before using most **fasterRaster** functions.
#' * [fast()]: Convert a `SpatRaster`, `SpatVector`, or `sf` vector to **fasterRaster**'s raster format (`GRaster`s) or vector format (`GVector`s), or load one from a file
#' * [rast()], [vect()], and [st_as_sf()]: Convert `GRaster`s and `GVector`s to `SpatRaster`s, `SpatVector`s, or `sf` vectors
#' * [writeRaster()] and [writeVector()]: Save `GRaster`s or `GVector`s to disk
#' * [addons()]: Test if the `addons` directory is correct and if a particular addon **GRASS** module is installed.
#'
#' ## Properties of `GRasters`
#' * [crs()]: Coordinate reference system
#' * [coordRef()]: Coordinate reference system
#' * [datatype()]: Data type
#' * [dim()] and [dim3d()]: Number of rows, columns, and depths
#' * [ext()], [N()], [S()], [E()], [W()], [top()], and [bottom()]: Spatial extent
#' * [freq()]: Frequencies of cell values in a raster
#' * [is.2d()] and [is.3d()]: Is an object 2- or 3-dimensional?
#' * [is.int()], [is.cell()], [is.float()], [is.doub()]: `GRaster` data type (integer/float/double)
#' * [is.factor()]: Does a raster represent categorical data?
#' * [is.lonlat()]: Is an object projected (e.g., in WGS84)?
#' * [levels()]: Names of levels in a categorical `GRaster`
#' * [minmax()]: Minimum and maximum values across all non-`NA` cells
#' * [names()]: `GRaster` names
#' * [ncol()]: Number of columns
#' * [nacell()]: Number of `NA` cells
#' * [ncell()]: Number of cells
#' * [ncell3d()]: Number of cells of a 3D `GRaster`
#' * [ndepth()]: Number of depths of a 3D `GRaster`
#' * [nlyr()]: Number of layers
#' * [nonnacell()]: Number of non-`NA` cells
#' * [nrow()]: Number of rows
#' * [nlevels()]: Number of categories
#' * [res()], [res3d()], [xres()], [yres()], and [zres()]: Spatial resolution
#' * [sources()]: Name of `GRaster` file in the **GRASS** cache
#' * [topology()]: Dimensionality (2D or 3D)
#' * [zext()]: Vertical extent
#' * [zres()]: Vertical resolution
#' 
#' ## Functions that operate on or create `GRasters`
#' * [Arithmetic]: Mathematical operations on `GRaster`s: `+`, `-`, `*`, `/`, `^`, `%%` (modulus), `%/%` (integer division)
#' * [Logical comparisons][Compare-methods]: `<`, `<=`, `==`, `!=`, `>=`, and `>`, plus \code{\link[fasterRaster]{%in%}} and \code{\link[fasterRaster]{%notin%}} (for categorical rasters only)
#' * [Logical operators][Logic-methods]: `|`and `&`
#'
#' Mathematical functions that are applied to each layer of a `GRaster`:
#' * Working with `NA`s: [is.na()], [not.na()], and [maskNA()] 
#' * Trigonometry: [sin()], [cos()], [tan()], [asin()], [acos()], [atan()], [atan2()] 
#' * Logarithms and powers: [exp()], [log()], [ln()], [log1p()], [log2()], [log10()], [sqrt()] 
#' * Rounding: [round()], [floor()], [ceiling()], [trunc()] 
#' * Signs: [abs()] 
#'
#' Mathematical functions that are applied across layers of multi-layered `GRaster`s:
#' * Numeration: [sum()], [count()] 
#' * Central tendency: [mean()], [mmode()], [median()] 
#' * Dispersion: [stdev()], [var()], [varpop()], [nunique()], [range()], [quantile()], [skewness()], [kurtosis()]
#' * Extremes: [min()], [max()], [which.min()], [which.max()] 
#' * `NA`s: [allNA()], [anyNA()] 
#' 
#' Subsetting, assigning, and replacing `GRaster` layers
#' * [$], \code{\link[fasterRaster]{[[}}, or [subset()]: Subset or remove specific layers of a `GRaster`
#' * \code{\link[fasterRaster]{[<-}}: Replace values of cells of a `GRaster`
#' * \code{\link[fasterRaster]{[[<-}}: Replace specific layers of a `GRaster`
#' * \code{\link[fasterRaster]{add<-}}: Replace specific layers of a `GRaster`
#'
#' Operations on `GRaster`s
#' * [as.int()], [as.float()], [as.doub()]: Change data type (integer/float/double)
#' * [as.lines()]: Convert a `GRaster` to a "lines" vector
#' * [as.points()]: Convert a `GRaster` to a "points" vector
#' * [as.polygons()]: Convert a `GRaster` to a "polygons" vector
#' * [aggregate()]: Aggregate values of `GRaster` cells into larger cells
#' * [bioclims()]: BIOCLIM rasters (classic set and extended set)
#' * [buffer()]: Create a buffer around non-`NA` cells
#' * [app()]: Apply a user-defined function to multiple layers of a `GRaster` (with helper functions [appFuns()] and [appCheck()])
#' * [c()]: "Stack" two or more rasters
#' * [cellSize()]: Cell area
#' * [classify()]: Partition cell values into strata
#' * [clump()]: Group adjacent cells with similar values
#' * [combineLevels()]: Combine the "levels" tables of two or more categorical `GRaster`s
#' * [concats()]: Combine values from two or more categorical and/or integer rasters by concatenating them
#' * [crop()]: Remove parts of a `GRaster`
#' * [denoise()]: Remove "noise" from a `GRaster` using a principal components analysis (PCA)
#' * [distance()]: Distance to non-`NA` cells, or vice versa
#' * [extend()]: Add rows and columns to a `GRaster`
#' * [extract()]: Extract values from a `GRaster` at locations of a `GVector`
#' * [fillNAs()]: Fill `NA` cells
#' * [focal()]: Calculate cell values based on values of nearby cells
#' * [fragmentation()]: Landscape fragmentation class from Riitters et al. (2020)
#' * [global()]: Summary statistics across cells of each `GRaster` layer
#' * [hist()]: Histogram of `GRaster` values
#' * [interpIDW()]: Interpolate values at points to a `GRaster`
#' * [kernel()]: Kernel density estimator of points
#' * [layerCor()]: Correlation or covariance between two or more `GRaster` layers
#' * [mask()]: Remove values in a `GRaster` based on values in another `GRaster` or vector
#' * [maskNA()]: Mask all non-NA cells or all NA cells
#' * [match()], \code{\link[fasterRaster]{%in%}}, and \code{\link[fasterRaster]{%notin%}}: Find which cells of a `GRaster` match or do not match certain values
#' * [merge()]: Combine two or more rasters with different extents and fill in `NA`s
#' * \code{\link[fasterRaster]{names<-}}: Assign names to a `GRaster`
#' * [noise()]: Remove coarse-scale trends from a `GRaster`, leaving just fine-scale "noise"
#' * [pairs()]: Plot correlations between `GRaster` layers
#' * [pcs()]: Retrieve a principal components model from a PCA `GRaster` generated using `princomp()`
#' * [plot()]: Display a `GRaster`
#' * [project()]: Change coordinate reference system and cell size
#' * [predict()]: Make predictions to a `GRaster` from a linear model or generalized linear model
#' * [princomp()]: Apply a principal components analysis (PCA) to a `GRaster`
#' * [regress()]: Regression intercept, slope, r2, and t-value across each set of cells
#' * [resample()]: Change cell size
#' * [reorient()]: Convert degrees between 'north-orientation' and 'east orientation'
#' * [sampleRast()]: Randomly sample cells from a `GRaster`
#' * [scale()], [scalepop()], and [unscale()]: Subtract means and divide by standard deviations, or inverse of that
#' * [selectRange()]: Select values from rasters in a stack based on values in another `GRaster`
#' * [spatSample()]: Randomly points from a `GRaster`
#' * [stretch()]: Rescale values in a GRaster
#' * [subst()]: Re-assign cell values
#' * [thinLines()]: Reduce linear features on a `GRaster` so linear features are 1 cell wide
#' * [tiles()]: Divide a `GRaster` into spatially exclusive subsets (though with possible overlap)
#' * [trim()]: Remove rows and columns from a `GRaster` that are all `NA`
#' * [zonal()]: Statistics (mean, sum, etc.) on areas of a `GRaster` defined by sets of cells with the same values in another `GRaster`, or by geometries in a `GVector`
#' * [zonalGeog()]: Geographic statistics (area, perimeter, fractal dimension, etc.) for sets of cells with the same values
#'
#' ## Creating `GRaster`s *de novo*
#' * [fractalRast()]: Create a fractal `GRaster`
#' * [init()]: GRaster with values equal to row, column, coordinate, regular, or "chess"
#' * [longlat()]: Create longitude/latitude rasters
#' * [rnormRast()]: A random `GRaster` with values drawn from a normal distribution
#' * [rSpatialDepRast()]: Create a random `GRaster` with or without spatial dependence
#' * [runifRast()]: A random `GRaster` with values drawn from a uniform distribution
#' * [sineRast()]: Sine wave rasters
#' 
#' ## Analysis of terrain and hydrology
#' * [as.contour()]: Contour lines from a `GRaster`
#' * [flow()]: Identify watershed basins and direction and accumulation of flow
#' * [flowPath()]: Path of water flow across a landscape
#' * [geomorphons()]: Identify terrain feature types
#' * [`hillshade()`][shade]: Create a hillshade `GRaster`
#' * [horizonHeight()]: Horizon height
#' * [sun()]: Solar radiance and irradiance
#' * [ruggedness()]: Terrain Ruggedness Index
#' * [streams()]: Create stream network
#' * [terrain()]: Slope, aspect, curvature, and partial slopes
#' * [wetness()]: Topographic wetness index
#' 
#' ## Operations on categorical (factor) `GRaster`s
#' * \code{\link[fasterRaster]{%in%}}, and \code{\link[fasterRaster]{%notin%}}: Mask cells that match or do not match a given category
#' * [activeCat()] and [activeCats()]: Column(s) that defines category labels
#' \code{\link[fasterRaster]{activeCat<-}}: Set column that defines category labels
#' * [addCats()]: Add new columns to a "levels" table
#' \code{\link[fasterRaster]{addCats<-}}: Add new rows (levels) to a "levels" table
#' * [categories()]: Set "levels" table for specific layers of a categorical raster
#' * [catNames()]: Column names of each "levels" table
#' * [cats()]: "Levels" table of a categorical raster
#' * [combineLevels()]: Combine the "levels" tables of two or more categorical `GRaster`s
#' * [complete.cases()]: Find rows of a categorical `GRaster`'s "levels" table that have no `NA`s in them
#' * [concats()]: Combine categories from two or more categorical rasters by concatenating them
#' * [droplevels()]: Remove one or more levels
#' * [freq()]: Frequency of each category across cells of a raster
#' * [is.factor()]: Is a raster categorical?
#' * [levels()]: "Levels" table of a categorical raster  
#' * \code{\link[fasterRaster]{levels<-}}: Set "levels" table of a categorical raster
#' * [match()], \code{\link[fasterRaster]{%in%}}, and \code{\link[fasterRaster]{%notin%}}: Find which cells of a `GRaster` match or do not match certain category labels
#' * [minmax()]: "Lowest" and "highest" category values of categorical rasters (when argument `levels = TRUE`)
#' * [missing.cases()]: Find rows of a categorical `GRaster`'s "levels" table that have at least one `NA` in them
#' * [missingCats()]: Values that have no category assigned to them
#' * [nlevels()]: Number of levels
#' * [segregate()]: Create one GRaster layer per unique value in a GRaster
#' * [subst()]: Re-assign category levels
#' * [zonalGeog()]: Geographic statistics (area, perimeter, fractal dimension, etc.) for sets of cells with the same values
#' 
#' ## Analysis of remote sensing rasters
#' * [compositeRGB()]: Combine red, green, and blue color bands to make a composite `GRaster`
#' * [plotRGB()]: Display a multispectral `GRaster` using red, blue, green, and alpha channels
#' * [vegIndex()]: Vegetation indices from surface reflectance
#'
#' ## Functions that operate on **terra** `SpatRaster`s
#' * [bioclims()]: BIOCLIM rasters (classic set and extended set)
#' * [fragmentation()]: Landscape fragmentation class from Riitters et al. (2020)
#'
#' ## Properties of `GVector`s
#' * [crs()]: Coordinate reference system
#' * [coordRef()]: Coordinate reference system
#' * [datatype()]: Data type of fields
#' * [dim()]: Number of geometries and columns
#' * [expanse()]: Area of polygons or length of lines
#' * [ext()], [N()], [S()], [E()], [W()], [top()], and [bottom()]: Spatial extent
#' * [geomtype()]: Type of vector (points, lines, polygons)
#' * [is.2d()] and [is.3d()]: Is an object 2- or 3-dimensional?
#' * [is.lonlat()]: Is an object projected (e.g., in WGS84)?
#' * [is.points()], [is.lines()], [is.polygons()]: Does a `GVector` represent points, lines, or polygons?
#' * [names()]: Names of `GVector` fields
#' * [ncol()]: Number of fields
#' * [ngeom()]: Number of geometries (points, lines, polygons)
#' * [nrow()]: Number of rows in a vector data table
#' * [nsubgeom()]: Number of subgeometries (points, lines, polygons that make up single- and multipart geometries)
#' * [sources()]: Name of the vector in **GRASS**
#' * [topology()]: Dimensionality (2D or 3D)
#' * [zext()]: Vertical extent
#'
#' ## Subsetting and assigning geometries or rows and columns of `GVector`s
#' * [$] or \code{\link[fasterRaster]{[[}}: Subset columns of a `GVector`'s data table
#' * \code{\link[fasterRaster]{[}} or [subset()]: Subset geometries of a `GVector`
#' * \code{\link[fasterRaster]{$<-}}: Replace specific columns of a `GVector`'s data table or add columns
#' * \code{\link[fasterRaster]{addTable<-}}: Add a data table to a `GVector`
#' * [dropTable()]: Remove a `GVector`s data table
#'
#' ## Operations on `GVector`s
#' * [aggregate()]: Combine `GVector` geometries
#' * [as.data.frame()]: Convert a `GVector`'s attribute table to a `data.frame`
#' * [as.data.table()]: Convert a `GVector`'s attribute table to a `data.table`
#' * [as.points()]: Extract vertex coordinates from a "lines" or "polygons" `GVector`
#' * [buffer()]: Create a polygon around/inside a `GVector`
#' * [clusterPoints()]: Identify clusters of points
#' * [centroids()]: Centroid(s) of a `GVector`
#' * [colbind()]: Add columns to the data table of a `GVector`
#' * [complete.cases()]: Find rows of a `GVector`'s data table that have no `NA`s in them
#' * [connectors()]: Create lines connecting nearest features of two `GVector`s
#' * [convHull()]: Minimum convex hull
#' * [crds()]: Extract coordinates of a `GVector`
#' * [crop()]: Remove parts of a `GVector`
#' * [delaunay()]: Delaunay triangulation
#' * [disagg()]: Separate multipart geometries into singlepart geometries
#' * [distance()]: Distance between geometries in two `GVector`, or from a `GVector` to cells of a `GRaster`
#' * [erase()] or `-`: Remove part of a `GVector` that overlaps with another
#' * [expanse()]: Area of polygons or length of lines
#' * [extract()]: Extract values from a `GVector` at specific points
#' * [grid()]: Create a grid `GVector`
#' * [head()]: First rows of a `GVector`'s data table
#' * [hexagons()]: Create a hexagonal grid
#' * [interpIDW()]: Interpolate values at points to a `GRaster` using inverse-distance weighting
#' * [interpSplines()]: Interpolate values at points to a `GRaster` using splines
#' * [intersect()] or `*`: Intersection of two `GVectors`
#' * [kernel()]: Kernel density estimator of points
#' * [missing.cases()]: Find rows of a `GVector`'s data table that have at least `NA` in them
#' * \code{\link[fasterRaster]{names<-}}: Assign names to columns of a `GVector`s data table
#' * [project()]: Change coordinate reference system
#' * [rasterize()]: Convert a `GVector` to a `GRaster`
#' * [rbind()]: Combine `GVectors`
#' * [simplifyGeom()]: Remove vertices
#' * [smoothGeom()]: Remove "angular" aspects of features
#' * [st_as_sf()]: Convert a `GVector` to a `sf` vector
#' * [st_buffer()]: Create a polygon around/inside a `GVector`
#' * [tail()]: Last rows of a `GVector`'s data table
#' * [thinPoints()]: Reduce number of points in same raster cell
#' * [union()] or `+`: Combine two `GVector`s
#' * [voronoi()]: Voronoi tessellation
#' * [xor()] or `/`: Select parts of polygons not shared by two `GVector`s
#'
#' ## Creating `GVector`s *de novo*
#' * [rvoronoi()]: Random Voronoi tesselation
#'
#' ## Fixing issues with `GVector`s
#' (See also *Details* [fast()].)
#' * [breakPolys()]: Break topologically clean areas
#' * [fillHoles()]: Fill "holes" of a `GVector`
#' * [fixBridges()]: Change "bridges" to "islands"
#' * [fixDangles()]: Change "dangles" hanging off boundaries to lines
#' * [fixLines()]: Break lines at intersections and lines that form closed loops
#' * [remove0()]: Remove all boundaries and lines with a length of 0
#' * [removeAngles()]: Collapse lines that diverge at an angle that is computationally equivalent to 0
#' * [removeBridges()]: Remove "bridges" to "islands"
#' * [removeDangles()]: Remove "dangling" lines
#' * [removeDupCentroids()]: Remove duplicated area centroids
#' * [removeDups()]: Remove duplicated features and area centroids
#' * [removeSmallPolys()]: Remove small polygons
#' * [snap()]: Snap lines/boundaries to each other
#' 
#' ## Converting between data types
#' * [as.contour()]: Convert a `GRaster` to a `GVector` representing contour lines
#' * [as.doub()]: Convert a `GRaster` to a double-floating point raster (**GRASS** data type `DCELL`)
#' * [as.data.frame()]: Convert `GVector` to a `data.frame`
#' * [as.data.table()]: Convert `GVector` to a `data.table`
#' * [as.float()]: Convert a `GRaster` to a floating-point raster (**GRASS** data type `FCELL`)
#' * [as.int()]: Convert a `GRaster` to an integer raster (**GRASS** data type `CELL`)
#' * [as.points()], [as.lines()], and [as.polygons()]: Convert a `GRaster` to a `GVector`
#' * [categories()] and \code{\link[fasterRaster]{levels<-}}: Convert an integer raster to a categorical ("factor") raster.
#' * [fast()]: Convert a `SpatRaster` to a `GRaster`; a `SpatVector`, `sf` vector, numeric vector, `matrix`, `data.frame`, or `data.table` to a `GVector`; or load a vector or raster from a file
#' * [rast()]: Convert a `GRaster` to a `SpatRaster`
#' * [rasterize()]: Convert a `GVector` to a `GRaster`
#' * [st_as_sf()]: Convert a `GVector` to a `sf` vector
#' * [vect()]: Convert a `GVector` to a `SpatVector`
#'
#' ## General purpose functions
#' * [compareGeom()]: Determine if geographic metadata is same between `GRaster`s and/or `GVector`s
#' * [dropRows()]: Remove rows from a `data.frame` or `data.table`
#' * [grassGUI()]: Start the **GRASS** GUI (not recommended for most users!!!)
#' * [grassHelp()]: Open the help page for a **GRASS** module.
#' * [grassInfo()]: **GRASS** version and citation
#' * [grassStarted()]: Has a connection **GRASS** been made within the current **R** session?
#' * [mow()]: Remove unused rasters and vectors from the **GRASS** cache
#' * [reorient()]: Convert degrees between 'north-orientation' and 'east orientation'
#' * [replaceNAs()]: Replace `NA`s in columns of a `data.table` or `data.frame`, or in a vector
#' * [seqToSQL()]: Format a numeric series into an SQL value call
#' * [update()]: Refresh metadata in a `GRaster` or `GVector` object
#'
#' ## Data objects
#' * [fastData()]: Helper function to quickly obtain example rasters and vectors
#' * [appFunsTable][appFunsTable] (see also [appFuns()]): Functions usable by the [app()] function
#' * [madChelsa][madChelsa]: Climate rasters for of a portion of eastern Madagascar
#' * [madCoast0][madCoast0], [madCoast4][madCoast4], and [madCoast][madCoast]: Borders of an eastern portion of Madagascar
#' * [madCover]: Land cover raster
#' * [madCoverCats][madCoverCats]: Table of land cover classes
#' * [madDypsis][madDypsis]: Specimens records of species in the genus *Dypsis*
#' * [madElev][madElev]: Elevation raster
#' * [madForest2000][madForest2000] and [madForest2014][madForest2014]: Forest cover in 2000 and 2014
#' * [madLANDSAT][madLANDSAT]: Surface reflectance in 2023
#' * [madPpt][madPpt], [madTmin][madTmin], [madTmax][madTmax]: Rasters of mean monthly precipitation, and minimum and maximum temperature
#' * [madRivers][madRivers]: Rivers vector
#' * [vegIndices][vegIndices]: Vegetation indices that can be calculated using [vegIndex()]
#' 
#' ## Esoteric tutorials and arcane notes
#' * Comparisons between `GRegion`s can be performed using the `==` and `!=` operators.
#' * Vignette on **GRASS** "projects/locations" and "mapsets": `vignette("projects_mapsets", package = "fasterRaster")`
#' * Vignette on **GRASS** "regions": `vignette("regions", package = "fasterRaster")`
#' * Vignette on **fasteRaster** hidden functions: `vignette("hidden_functions", package = "fasterRaster")`
#'
#' ## Classes
#' * [`GLocation`]: Fundamental class; points to a "location/project" in **GRASS**
#' * [`GSpatial`]: Basic class of any spatial object
#' * [`GRegion`]: Points to a "region" of a "location/project" in **GRASS**
#' * [`GRaster`]: Raster class
#' * [`GVector`]: Spatial vector class
#'
#' @author Adam B. Smith
#' @name fasterRaster
#' @keywords internal
"_PACKAGE"
