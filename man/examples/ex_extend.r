if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")

# Send spatial objects to GRASS:
elev <- fast(madElev)
rivers <- fast(madRivers)

# Extend raster by number of rows/columns:
extended1 <- extend(elev, 10, fill = 900)
extended2 <- extend(elev, c(10, 20), fill = 900)
extended3 <- extend(elev, c(10, 80, 0, 100), fill = 900)
dim(elev)
dim(extended1)
dim(extended2)
dim(extended3)

plot(extended3)

# When exporting a raster, NA rows and columns are removed.
extended4 <- extend(elev, 100, fill=1) # default fill is NA
extended4terra <- rast(extended4)

dim(extended4)
dim(extended4terra)

plot(extended4)

# Extend the raster by another object with a wider extent.

# For tis example, first crop the raster, then extend it.
elevCrop <- crop(elev, rivers)
uncrop <- extend(elevCrop, elev, fill = 900)
plot(uncrop)

}
