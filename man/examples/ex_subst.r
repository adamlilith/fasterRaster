if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madCover <- fastData("madCover")

### Substitution within an integer/numeric raster
#################################################

# Convert SpatRaster to GRaster
elev <- fast(madElev)

# Simple substitution of one value, keeping all other values
newElev <- elev
newElev[newElev == 100] <- -100
newElev[newElev > 500] <- 500
hist(newElev)

# Simple substitution of one value, keeping all other values
substituted <- subst(elev, from = 300, to = -300)
substituteds <- c(elev, substituted)
names(substituteds) <- c("original", "substituted")
plot(substituteds)

# Simple substitution of three values, keeping all other values
substituted <- subst(elev, from = c(299, 300, 301), to = c(-699, -600, -601))
substituteds <- c(elev, substituted)
names(substituteds) <- c("original", "substituted")
plot(substituteds)

# Simple substitution of three values to one other value, retaining remainder
substituted <- subst(elev, from = c(299, 300, 301), to = -1000)
substituteds <- c(elev, substituted)
names(substituteds) <- c("original", "substituted")
plot(substituteds)

# Simple substitution of one value, setting all other values to 100
substituted <- subst(elev, from = 300, to = -300, others = 100)
substituteds <- c(elev, substituted)
names(substituteds) <- c("original", "substituted")
plot(substituteds)

### Substitution within a factor/categorical raster
###################################################

# Convert a SpatRaster to a GRaster:
cover <- fast(madCover)

cover <- droplevels(cover) # remove unused levels
levels(cover) # levels of "cover"

# Substitute using level name, replace with EXISTING level label
from <- "Mosaic cropland/vegetation"
to <- "Mosaic crops"
categ <- subst(cover, from = from, to = to)
freq(cover) # original frequencies of each land cover class
freq(categ) # note change in frequency of "from" and "to" categories
plot(c(cover, categ))

# Substitute using level name, replace with NEW level label
from <- c("Mosaic crops", "Mosaic cropland/vegetation")
to <- c("Mixed cropland")
categ <- subst(cover, from = from, to = to)
freq(cover) # original frequencies of each land cover class
freq(categ) # note change in frequency of "from" and "to" categories
plot(c(cover, categ))

# Substitute using level name, replace with NEW level label
from <- c("Mosaic crops", "Mosaic cropland/vegetation")
to <- c("Mixed cropland", "Mixed cropland/vegetation")
categ <- subst(cover, from = from, to = to)
freq(cover) # original frequencies of each land cover class
freq(categ) # note change in frequency of "from" and "to" categories
plot(c(cover, categ))

# Substitute using level name, replace with VALUE of an existing label
from <- c("Mosaic crops", "Mosaic cropland/vegetation")
to <- 120
categ <- subst(cover, from = from, to = to)
freq(cover) # original frequencies of each land cover class
freq(categ) # note change in frequency of "from" and "to" categories
plot(c(cover, categ))

# Substitute using level name, replace with new level name, replace all others
from <- c("Mosaic crops", "Mosaic cropland/vegetation")
to <- "Crops"
categ <- subst(cover, from = from, to = to, others = "Other")
freq(cover) # original frequencies of each land cover class
freq(categ) # note change in frequency of "from" and "to" categories
plot(c(cover, categ))

}
