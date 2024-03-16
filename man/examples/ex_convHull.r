if (grassStarted()) {

# Setup
library(sf)

# Points vector of specimens of species in the plant genus Dypsis
madDypsis <- fastData("madDypsis")

# Convert sf to a GVector:
dypsis <- fast(madDypsis)

### Convex hull for all plant specimens together:
ch <- convHull(dypsis)

### Convex hull for each species:
head(dypsis) # See the "species" column?
chSpecies <- convHull(dypsis, by = "species")

### Plot:
plot(st_geometry(madDypsis))
plot(ch, add = TRUE)
n <- nrow(chSpecies)
plot(chSpecies, col = 1:n, add = TRUE)

}
