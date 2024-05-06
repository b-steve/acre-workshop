library(acre)

source("workshop-functions.R")

cov.df <- measure.covariates(skip.wait = TRUE)

plot.cov(cov.df, "elevation")
plot.cov(cov.df, "canopy.height")
plot.cov(cov.df, "forest.type")


interpolated.df <- interpolate.covs(cov.df)

plot.cov(cov.df, "elevation")
plot.cov(interpolated.df, "elevation")
plot.cov(cov.df, "canopy.height")
plot.cov(interpolated.df, "canopy.height")
plot.cov(cov.df, "forest.type")
plot.cov(interpolated.df, "forest.type")

compare.to.truth(interpolated.df)

survey.data <- conduct.survey(skip.wait = TRUE)

data <- read.acre(survey.data$captures, survey.data$traps,
                  control.mask = list(buffer = 3000),
                  loc.cov = cov.df, dist.cov = list(villages = villages.ppws))

plot(data, type = "capt")
plot(data, type = "covariates")

fit1 <- fit.acre(data, detfn = "hhn")
fit2 <- fit.acre(data, detfn = "hhn", model = list(D = ~ forest.type + canopy.height))

summary(fit1)
summary(fit2)

plot(fit1, type = "Dsurf", new.data = ppws)
plot(fit2, type = "Dsurf", new.data = ppws)

plot(fit1, type = "detfn")
plot(fit2, type = "detfn", add = TRUE, col = "red")

AIC(fit1)
AIC(fit2)
