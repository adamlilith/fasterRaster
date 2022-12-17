\donttest{

library(terra)

# forest cover (1s and NAs)
madForest2000 <- fasterData('madForest2000')

# cells are just 1s or NAs... replace NAs with 0
forest <- calc(madForest2000, function(x) ifelse(is.na(x), 0, 1))

# single-core
frag <- fragmentation(forest, size=5, pad=TRUE, undet='perforated')

# multi-core
frag <- fasterFragmentation(forest, size=5, pad=TRUE, undet='perforated')

par(mfrow=c(2, 2))
plot(madForest2000, col=c('gray90', 'forestgreen'), main='Forest Cover')
plot(frag[['density']], main='Density in 2000')
plot(frag[['connect']], main='Connectivity in 2000')
cols <- c('gray90', 'blue', 'lightblue', 'yellow', 'orange', 'forestgreen')
names(cols) <- c('no forest', 'patch', 'transitional',
		'perforated', 'edge', 'interior')
plot(frag[['class']], main='Fragmentation Class', col=cols, legend=FALSE)
legend('topright', fill=cols, legend=names(cols))

}
