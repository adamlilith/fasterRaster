if (grassStarted()) {

# Setup
library(terra)

# Example data
madElev <- fastData("madElev")
madRivers <- fastData("madRivers")

# Report information on current region:
dim3d()
nrow()
ncol()
ndepth()
ncell()
ncell3d()

ext()
W()
E()
S()
N()

res()
res3d()
xres()
yres()
zres()


}
