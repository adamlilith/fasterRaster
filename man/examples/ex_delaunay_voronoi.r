if (grassStarted()) {

# Setup
library(sf)

# Example vectors
madDypsis <- fastData("madDypsis") # points
madCoast4 <- fastData("madCoast4") # polygons

# Convert sf vectors to GVectors
dypsis <- fast(madDypsis)
coast4 <- fast(madCoast4)
ant <- coast4[coast4$NAME_4 == "Antanambe"]

# Delaunay triangulation
dypsisDel <- delaunay(dypsis)
plot(dypsisDel)
plot(dypsis, pch = 1, col = "red", add = TRUE)

# Voronoi tessellation
vor <- voronoi(dypsis)
plot(vor)
plot(dypsis, pch = 1, col = "red", add = TRUE)

# Random Voronoi tessellation
rand <- rvoronoi(coast4, size = 100)
plot(rand)

}
