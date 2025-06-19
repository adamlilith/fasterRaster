if (grassStarted()) {

# Setup
library(terra)

# Climatic conditions with 4 variables
madChelsa <- fastData("madChelsa")
chelsa <- fast(madChelsa)

# Simulate new conditions by multiplying values by (1 + small number)
proj <- 1.05 * chelsa
names(proj) <- names(chelsa) # proj must have same names as ref

messes <- multivarEnvSim(ref = chelsa, proj = proj)
plot(messes)

# Where is at least one variable outside the reference range?
extrap <- messes[["MESS"]] < 0
levs <- data.frame(value = 0:1, labels = c('no extrapolation', 'extrapolation'))
levels(extrap) <- levs
plot(extrap)

# Using a data frame as "reference" conditions:
madDypsis <- fastData("madDypsis") # Dypsis occurrences
dypsis <- fast(madDypsis)

dypEnv <- extract(chelsa, dypsis)
dypMess <- multivarEnvSim(ref = dypEnv, proj = proj)
plot(dypMess)

}
