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

plot(rivers, col = "blue")
plot(dypsis, add = TRUE)
plot(consFromDypsis, col = "red", add = TRUE)

### Connections from each river to nearest point
consFromRivers <- connectors(rivers, dypsis)

plot(rivers, col = "blue")
plot(dypsis, add = TRUE)
plot(consFromRivers, col = "red", add = TRUE)

}
