if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madRivers <- fastData("madRivers")
rivers <- fast(madRivers)
river <- rivers[rivers$TopElev == 652] # select one river for illustration

### Simplify geometry (remove nodes)

vr <- simplifyGeom(river, tolerance = 2000)
dp <- simplifyGeom(river, tolerance = 2000, method = "dp")
dpr <- simplifyGeom(river, tolerance = 2000, method = "dpr", prop = 0.5)
rw <- simplifyGeom(river, tolerance = 2000, method = "rw")

plot(river, col = "black", lwd = 3)
plot(vr, col = "blue", add = TRUE)
plot(dp, col = "red", add = TRUE)
plot(dpr, col = "chartreuse", add = TRUE)
plot(rw, col = "orange", add = TRUE)

legend("top",
   xpd = NA,
   legend = c(
	"Original",
      "Vertex reduction",
      "Douglas-Peucker",
      "Douglas-Peucker reduction",
      "Reumann-Witkam"
	),
	col = c("black", "blue", "red", "chartreuse", "orange"),
	lwd = c(3, 1, 1, 1, 1)
)

### Smooth geometry

hermite <- smoothGeom(river, dist = 2000, angle = 3)
chaiken <- smoothGeom(river, method = "Chaiken", dist = 2000)

plot(river, col = "black", lwd = 2)
plot(hermite, col = "blue", add = TRUE)
plot(chaiken, col = "red", add = TRUE)

legend("bottom",
   xpd = NA,
   legend = c(
	  "Original",
      "Hermite",
      "Chaiken"
	),
	col = c("black", "blue", "red"),
	lwd = c(2, 1, 1, 1, 1)
)

### Clean geometry

# Has no effect on this vector!
noDangs <- removeDangles(soam, tolerance = 10000)

plot(soam, col = "black", lwd = 2)
plot(noDangs, col = "red", add = TRUE)

legend("bottom",
   xpd = NA,
   legend = c(
	  "Original",
      "No dangles"
	),
	lwd = c(2, 1),
	col = c("black", "red")
)

}
