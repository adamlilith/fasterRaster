if (grassStarted()) {

# Setup
library(sf)

# Polygons vector:
madCoast4 <- fastData(madCoast4)
mc4 <- fast(madCoast4)

neighs <- neighborhoodMatrix(mc4)
neighs

}
