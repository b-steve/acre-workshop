library(geoR)
load("ppws-mask.RData")
source("workshop-functions.R")

set.seed(1234)
canopy.height.ppws <- grf(nrow(ppws), grid = ppws, cov.pars = c(1, 100000))$data
elevation.ppws <- grf(nrow(ppws), grid = ppws, cov.pars = c(1, 50000))$data
forest.type.ppws <- grf(nrow(ppws), grid = ppws, cov.pars = c(1, 10000))$data
forest.type.ppws <- ifelse(forest.type.ppws > quantile(forest.type.ppws, 0.3),
                           "evergreen", "deciduous")

## cols = viridis(100)
## par(mfrow = c(1, 3))
## image_xyz(ppws[, 1], ppws[, 2], canopy.height.ppws, col = cols, main  = "canopy height")
## image_xyz(ppws[, 1], ppws[, 2], elevation.ppws, col = cols, main  = "elevation")
## image_xyz(ppws[, 1], ppws[, 2], ifelse(forest.type.ppws == "evergreen", 1, 0),
##           col = cols, main = "forest type")

true.df <- data.frame(canopy.height = canopy.height.ppws,
                      elevation = elevation.ppws,
                      forest.type = forest.type.ppws)

D.calc <- function(df){
    exp(log(0.01) + 0.2*df$canopy.height - 0.6*(df$forest.type != "evergreen"))
}

D.ppws <- D.calc(true.df)
true.df$D <- D.ppws

s.ppws <- sim.pop(true.df, D.calc)
## plot.cov(true.df, "D")
## points(s.ppws)

save(canopy.height.ppws, elevation.ppws, forest.type.ppws, s.ppws, file = "ppws.RData")
