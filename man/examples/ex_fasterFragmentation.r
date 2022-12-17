\donttest{

library(terra)
madForest2000 <- fasterData('madForest2000')

# cells are just 1s or NAs... replace NAs with 0
forest <- app(madForest2000, function(x) ifelse(is.na(x), 0, 1))

# single-core... can take a while!
frag <- fragmentation(madForest2000, size=5, pad=TRUE, undet='perforated')

# multi-core... increase number of cores if you have more!
frag <- fasterFragmentation(madForest2000, size=5, pad=TRUE,
undet='perforated', cores=2)

oldPar <- par(mfrow=c(2, 2))
plot(madForest2000, col=c('gray90', 'forestgreen'), main='Forest Cover')
plot(frag[['density']], main='Density')
plot(frag[['connect']], main='Connectivity')
cols <- c('gray90', 'blue', 'lightblue', 'yellow', 'orange', 'forestgreen')
names(cols) <- c('no forest', 'patch', 'transitional',
		'perforated', 'edge', 'interior')
plot(frag[['class']], main='Fragmentation Class', col=cols, legend=FALSE)
legend('topright', fill=cols, legend=names(cols))
par(oldPar)

}
