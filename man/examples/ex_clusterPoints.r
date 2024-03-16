if (grassStarted()) {

# Setup
library(sf)

# Points vector of specimens of species in the plant genus Dypsis
madDypsis <- fastData("madDypsis")

# Convert sf to a GVector:
dypsis <- fast(madDypsis)

# Cluster:
dbscan <- clusterPoints(dypsis, method = "DBSCAN", maxDist = 10000)
dbscan2 <- clusterPoints(dypsis, method = "DBSCAN2", maxDist = 10000)
density <- clusterPoints(dypsis, method = "density", maxDist = 10000)
optics <- clusterPoints(dypsis, method = "optics", maxDist = 10000)
optics2 <- clusterPoints(dypsis, method = "optics2", maxDist = 10000)

oldpar <- par(mfrow = c(2, 3))
plot(dypsis, pch = 21, cex = 1.4, bg = dbscan, main = "DBSCAN")
plot(dypsis, pch = 21, cex = 1.4, bg = dbscan2, main = "DBSCAN2")
plot(dypsis, pch = 21, cex = 1.4, bg = density, main = "density")
plot(dypsis, pch = 21, cex = 1.4, bg = optics, main = "OPTICS")
plot(dypsis, pch = 21, cex = 1.4, bg = optics2, main = "OPTICS2")
par(oldpar)

}
