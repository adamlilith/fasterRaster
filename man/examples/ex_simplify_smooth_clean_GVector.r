if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madCoast0 <- fastData("madCoast0")
madRivers <- fastData("madRivers")

### Simplify geometry (remove nodes)
####################################

rivers <- fast(madRivers)

vr <- simplifyGeom(rivers, tolerance = 2000)
dp <- simplifyGeom(rivers, tolerance = 2000, method = "dp")
dpr <- simplifyGeom(rivers, tolerance = 2000, method = "dpr", prop=0.5)
rw <- simplifyGeom(rivers, tolerance = 2000, method = "rw")

plot(st_geometry(madRivers), col="gray", lwd=3)
plot(vr, col="blue", add=TRUE)
plot(dp, col="red", add=TRUE)
plot(dpr, col="chartreuse", add=TRUE)
plot(rw, col="orange", add=TRUE)

legend("topright",
   legend = c(
	  "Original",
      "Vertex reduction",
      "Douglas-Peucker",
      "Douglas-Peucker reduction",
      "Reumann-Witkam"
	),
	col = c("gray", "blue", "red", "chartreuse", "orange"),
	lwd = c(3, 1, 1, 1, 1)
)

### Smooth geometry
###################

hermite <- smoothGeom(rivers, dist = 2000, angle = 3)
chaiken <- smoothGeom(rivers, method = "Chaiken", dist = 2000)

plot(st_geometry(madRivers), col="gray", lwd=2)
plot(hermite, col="blue", add=TRUE)
plot(chaiken, col="red", add=TRUE)

legend("topright",
   legend = c(
	  "Original",
      "Hermite",
      "Chaiken"
	),
	col = c("gray", "blue", "red"),
	lwd = c(2, 1, 1, 1, 1)
)

### Clean geometry
##################

noDangs <- removeDangles(rivers, tolerance = 2000)

plot(st_geometry(madRivers), col="blue")
plot(noDangs, col="red", add=TRUE)

legend("topright",
   legend = c(
	  "Original",
      "No dangles"
	),
	col = c("blue", "red")
)

}
