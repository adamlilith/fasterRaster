\dontrun{

library(terra)

# forest cover (1s and NAs)
madForest2000 <- fasterData('madForest2000')

# cells are just 1s or NAs... replace NAs with 0
forest <- app(madForest2000, function(x) ifelse(is.na(x), 0, 1))

# single-core
frag <- fasterFragmentation(forest, size=5, pad=TRUE, undet='perforated')

oldPar <- par(mfrow=c(2, 2))
plot(madForest2000, col='forestgreen', main='Forest')
plot(frag[['density']], main='Density')
plot(frag[['connect']], main='Connectivity')
cols <- c('gray90', 'blue', 'lightblue', 'yellow', 'orange', 'forestgreen')
names(cols) <- c('no forest', 'patch', 'transitional',
		'perforated', 'edge', 'interior')
plot(frag[['class']], main='Fragmentation Class', col=cols, legend=FALSE)
legend('topright', fill=cols, legend=names(cols))
par(oldPar)

}
