#library(acre)
library(devtools)
load_all("~/GitHub/acre")

source("workshop-functions.R")

cov.df <- measure.covariates()

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

survey.data <- conduct.survey()

data <- read.acre(survey.data$captures, survey.data$traps,
                  control_create_mask = list(buffer = 3000),
                  loc_cov = cov.df)

plot(data, type = "capt")
plot(data, type = "covariates")

fit1 <- fit.acre(data, detfn = "hhn")
AIC(fit1)
fit2 <- fit.acre(data, detfn = "hn")
AIC(fit2)
fit3 <- fit.acre(data, par_extend_model = list(D = ~ elevation + canopy.height),
                 detfn = "hhn")
AIC(fit3)

show_Dsurf(fit3, new_data = interpolated.df, plot_contours = FALSE)
