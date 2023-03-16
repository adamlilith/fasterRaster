\dontrun{

library(sf)

# change this to where GRASS is installed on your system
grassDir <- 'C:/Program Files/GRASS GIS 8.2' # example for a PC
grassDir <- "/Applications/GRASS-8.2.app/Contents/Resources" # for a Mac

# spatial points object
data('madCoast0')
data('madDypsis')

### minimum convex hull
mcp <- fasterConvHull(madDypsis, grassDir=grassDir)

plot(st_geometry(madCoast0))
plot(st_geometry(madDypsis), pch=16, add=TRUE)
plot(mcp, border='blue', add=TRUE)

### Delaunay triangulation
del <- fasterDelaunay(madDypsis, grassDir=grassDir)

plot(st_geometry(madCoast0))
plot(st_geometry(madDypsis), pch=16, add=TRUE)
plot(del, border='blue', add=TRUE)

### Voronoi diagram for points
vor <- fasterVoronoi(madDypsis, grassDir=grassDir)

plot(st_geometry(madCoast0))
plot(st_geometry(madDypsis), pch=16, add=TRUE)
plot(vor, border='blue', add=TRUE)

### Voronoi diagram for polygons
data(madCoast4)
vor <- fasterVoronoi(madCoast4, type='polygons', grassDir=grassDir)

plot(st_geometry(madCoast0))
plot(st_geometry(madCoast4), add=TRUE)
plot(vor, border='blue', add=TRUE)

}

