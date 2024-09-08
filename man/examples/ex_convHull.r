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
head(dypsis) # See the "rightsHolder" column?
chHolder <- convHull(dypsis, by = "rightsHolder")

### Plot:
plot(dypsis)
plot(ch, add = TRUE)
n <- length(chHolder)
for (i in 1:n) {
   plot(chHolder[[i]], border = i, add = TRUE)
}

}
