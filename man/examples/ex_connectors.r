if (grassStarted()) {

# Setup
library(sf)

# Rivers vector and locations of Dypsis plants
madRivers <- fastData("madRivers")
madDypsis <- fastData("madDypsis")

# Convert sf's to GVectors:
rivers <- fast(madRivers)
dypsis <- fast(madDypsis)

### Connections from each point to nearest river
consFromDypsis <- connectors(dypsis, rivers)

plot(st_geometry(madDypsis))
plot(st_geometry(madRivers), col = "blue", add = TRUE)
plot(consFromDypsis, add = TRUE)

### Connections from each river to nearest point
consFromRivers <- connectors(rivers, dypsis)

plot(st_geometry(madDypsis))
plot(st_geometry(madRivers), col = "blue", add = TRUE)
plot(consFromRivers, add = TRUE)

}
