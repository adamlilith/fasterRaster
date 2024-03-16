if (grassStarted()) {

# Setup
library(sf)

# Example data:
madCoast4 <- fastData("madCoast4")
madRivers <- fastData("madRivers")
madDypsis <- fastData("madDypsis")

# Convert sf vectors to GVectors:
coast <- fast(madCoast4)
rivers <- fast(madRivers)
dypsis <- fast(madDypsis)

# Geographic properties:
ext(rivers) # extent
crs(rivers) # coordinate reference system

# Column names and data types:
names(coast)
datatype(coast)

# Session information:
location(rivers) # GRASS location
mapset(rivers) # GRASS mapset

# Points, lines, or polygons?
geomtype(dypsis)
geomtype(rivers)
geomtype(coast)

is.points(dypsis)
is.points(coast)

is.lines(rivers)
is.lines(dypsis)

is.polygons(coast)
is.polygons(dypsis)

# Number of dimensions:
topology(rivers)
is.2d(rivers) # 2-dimensional?
is.3d(rivers) # 3-dimensional?

# Just the data table:
as.data.frame(rivers)
as.data.table(rivers)

# Top/bottom of the data table:
head(rivers)
tail(rivers)

# Vector or table with just selected columns:
names(rivers)
rivers$NAME
rivers[[c("NAM", "NAME_0")]]
rivers[[c(3, 5)]]

# Select geometries/rows of the vector:
nrow(rivers)
selected <- rivers[2:6]
nrow(selected)

# Plot:
plot(coast)
plot(rivers, col = "blue", add = TRUE)
plot(selected, col = "red", lwd = 2, add = TRUE)

# Vector math:
hull <- convHull(dypsis)

un <- union(coast, hull)
sameAsUnion <- coast + hull
plot(un)
plot(sameAsUnion)

inter <- intersect(coast, hull)
sameAsIntersect <- coast * hull
plot(inter)
plot(sameAsIntersect)

er <- erase(coast, hull)
sameAsErase <- coast - hull
plot(er)
plot(sameAsErase)

xr <- xor(coast, hull)
sameAsXor <- coast / hull
plot(xr)
plot(sameAsXor)

# Vector area and length:
expanse(coast, unit = "km") # polygons areas
expanse(rivers, unit = "km") # river lengths

# Fill holes
# First, we will make some holes by creating buffers around points, then
# removing them from a polygons GVector.
buffs <- buffer(dypsis, 500)

holes <- coast - buffs
plot(holes)

filled <- fillHoles(holes, fail = FALSE)

}
