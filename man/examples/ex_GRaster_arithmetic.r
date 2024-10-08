if (grassStarted()) {

# Setup
library(sf)
library(terra)

# Example data
madElev <- fastData("madElev")

# Convert a SpatRaster to a GRaster
elev <- fast(madElev)
elevs <- c(elev, elev, log10(elev) - 1, sqrt(elev))
names(elevs) <- c("elev1", "elev2", "log_elev", "sqrt_elev")

elev
elevs

# do some math
elev + 100
elev - 100
elev * 100
elev / 100
elev ^ 2
elev %/% 100 # divide then round down
elev %% 100 # modulus

100 + elev
100 %/% elev
100 %% elev

elevs + 100
100 + elevs

# math with logicals
elev + TRUE
elev - TRUE
elev * TRUE
elev / TRUE
elev ^ TRUE
elev %/% TRUE # divide then round down
elev %% TRUE # modulus

elevs + TRUE
TRUE + elevs

# Raster interacting with raster(s):
elev + elev
elev - elev
elev * elev
elev / elev
elev ^ log(elev)
elev %/% sqrt(elev) # divide then round down
elev %% sqrt(elev) # modulus

elevs + elev
elev * elevs

# sign
abs(-1 * elev)
abs(elevs)

# powers
sqrt(elevs)

# trigonometry
sin(elev)
cos(elev)
tan(elev)

asin(elev)
acos(elev)
atan(elev)

atan(elevs)
atan2(elev, elev^1.2)
atan2(elevs, elev^1.2)
atan2(elev, elevs^1.2)
atan2(elevs, elevs^1.2)

# logarithms
exp(elev)
log(elev)
ln(elev)
log2(elev)
log1p(elev)
log10(elev)
log10p(elev)
log(elev, 3)

log(elevs)

# rounding
round(elev + 0.5)
floor(elev + 0.5)
ceiling(elev + 0.5)
trunc(elev + 0.5)

# comparison
elev < 100
elev <= 100
elev == 100
elev != 100
elev > 100
elev >= 100

elev + 100 < 2 * elev

elevs > 10
10 > elevs

# logic
elev < 10 | elev > 200
elev < 10 | cos(elev) > 0.9

elev < 10 | TRUE
TRUE | elev > 200

elev < 10 | FALSE
FALSE | elev > 200

elev < 10 & cos(elev) > 0.9

elev < 10 & TRUE
TRUE & elev > 200

elev < 10 & FALSE
FALSE & elev > 200

# Mathematical functions on GRasters with >= 2 layers:
mean(elevs)
mmode(elevs)
median(elevs)
nunique(elevs)

sum(elevs)
count(elevs)
min(elevs)
max(elevs)
range(elevs)
skewness(elevs)
kurtosis(elevs)

which.min(elevs)
which.max(elevs)

slope(elevs)
intercept(elevs)
r2(elevs)
tvalue(elevs)

stdev(elevs)
stdev(elevs, pop = FALSE)
var(elevs)
varpop(elevs)

# Note: To get quantiles for each layer, use
# global(x, "quantile", probs = 0.2).
quantile(elevs, 0.1)

}
