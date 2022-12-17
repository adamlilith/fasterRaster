\donttest{

library(terra)

madElev <- fasterData('madElev')
sdElev <- fasterFocal(madElev, fun=sd, cores=2)

oldPar <- par(mfrow=c(1, 2))
plot(madElev, main='Elevation')
plot(sdElev, main='SD in focal window')
par(oldPar)

# Using a "high-pass" weights matrix:
w <- matrix(c(-1, -1, -1, -1, 8, -1, -1, 1, -1), ncol=3)
hpFilter <- fasterFocal(madElev, w=w, fun=mean, cores=2)

oldPar <- par(mfrow=c(1, 2))
plot(madElev, main='Elevation')
plot(hpFilter, main='High Pass Filter')
par(oldPar)

# What if your weights matrix has NAs?
# Write a function that returns a value even if there are NAs.
w <- matrix(
  c(1,NA,1,NA,1,NA,1,1,1,NA,1,1,NA,1,1,NA,1,1,1,NA,1,NA,1,NA,1),
  nrow=5, ncol=5
)

fx <- function(x) mean(x, na.rm=TRUE)
naFilter <- fasterFocal(madElev, w=w, fun=fx, cores=2, na.rm=TRUE)

oldPar <- par(mfrow=c(1, 2))
plot(madElev, main='Elevation')
plot(naFilter, main='Filter with NAs')
par(oldPar)

}
