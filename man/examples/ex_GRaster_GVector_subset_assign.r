if (grassStarted()) {

# Setup
library(terra)

### GRasters
############

# Example data
madElev <- fastData("madElev") # elevation raster
madForest2000 <- fastData("madForest2000") # forest raster
madForest2014 <- fastData("madForest2000") # forest raster

# Convert SpatRasters to GRasters
elev <- fast(madElev)
forest2000 <- fast(madForest2000)
forest2014 <- fast(madForest2014)

### Re-assigning values of a GRaster

constant <- elev
constant[] <- 14
constant

names(constant) <- "constant"

### Re-assigning specific values of a raster
replace <- elev
replace[replace == 1] <- -20
replace

### Subsetting specific values of a raster based on another raster
elevInForest <- elev[forest2000 == 1]
plot(c(elev, forest2000, elevInForest))

### Adding and re-assigning layers of a GRaster
rasts <- c(elev, constant, forest2000)

# Combine with another layer:
add(rasts) <- forest2014

# Combine two layers:
elevs <- c(2 * elev, elev / 10)
names(elevs) <- c("elev_x2", "elev_div10")
elevs

# Combine with previous layers:
rasts <- c(rasts, elevs)
rasts

### Subsetting GRaster layers

# Subset:
rasts[[2:3]]
rasts[[c("madForest2000", "elev_x2")]]
rasts$madForest2000

# Get every other layer:
rasts[[c(FALSE, TRUE)]]

### Replacing layers of a GRaster

# Replace a layer
logElev <- log(elev)
names(logElev) <- "logElev"
rasts$madForest2000 <- logElev
rasts

# Replace a layer:
rasts[[3]] <- forest2000
rasts

### GVectors
############

# example data
madDypsis <- fastData("madDypsis") # vector of points

# Convert SpatVector to GVector
dypsis <- fast(madDypsis)

### Retrieving GVector columns

dypsis$species # Returns the column

dypsis[[c("year", "species")]] # Returns a GRaster with these columns
dypsis[ , c("year", "species")] # Same as above

### Subsetting GVector geometries

# Subset first three geometries
dypsis[1:3]
dypsis[1:3, "species"]

# Get geometries by data table condition
dypsis[dypsis$species == "Dypsis betsimisarakae"]

### (Re)assigning GVector column values

# New column
dypsis$pi <- pi

# Re-assign values
dypsis$pi <- "pie"

# Re-assign specific values
dypsis$institutionCode[dypsis$institutionCode == "MO"] <-
   "Missouri Botanical Garden"

}
