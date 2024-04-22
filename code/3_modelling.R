#install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
#install.packages("inlabru")
#install.packages("Metrics")

library(AER)
library(MASS)
library(psych)
library(rgdal)
library(spdep)
library(dplyr)
library(ggplot2)
library(INLA)
library(stringr)
library(sf)
library(ape)
library(Metrics)
library(inlabru)


# set workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#=====================================================================================================
#read table
load("../data/df.RData")
df$Total <- df$Hate+df$Nohate

#read geometries
geom <- st_read("../data/GIS/us_counties.shp")

#test for NAs
any(is.na(df))
any(is.infinite(df$Hate))

#standardize covariates (mean = 0, stdev = 1)
covars <- names(df)
covars <- covars[! covars %in%  c("GEOID", "Hate", "Nohate", "Total", "Asian_destination")]
df <- df %>% mutate_at(covars, ~(scale(.) %>% as.vector))

#=====================================================================================================
#eliminate zero counties
df2 <- df[df["Total"] > 0, ]

#add geometries
geom2 <- left_join(df2, geom, by = "GEOID")

#multicollinearity
#=====================================================================================================
corr <- subset(df2, select = -c(GEOID, Hate, Nohate, Asian_destination)) 
cor(corr, method = c("pearson", "kendall", "spearman"))
round(corr, 2)

#Variance inflation factors
mod <- glm(Hate/Total ~ Case_rate + Death_rate + Percent_Asian_2020 + Percent_foreign_born + 
             Asian_White_poverty_ratio + Asian_White_population_in_managerial_positions +
             Concentrated_disadvantage + Income_inequality,data = geom2)
summary(mod)
vif(mod)

#=====================================================================================================
## Queens adjacency
queens <- poly2nb(geom2$geometry, queen = TRUE)
nn_mat <-  nb2mat(queens, style='W')
lw <- nb2listw(queens, style="W", zero.policy=TRUE)
nb2INLA("../data/map1.adj", queens)
g1 <- inla.read.graph(filename = "../data/map1.adj")

# spatial effects
geom2$idareau <- as.numeric(as.factor(geom2$GEOID))

#list  models
models <- inla.models()

### Priors: From Moraga - gives better estimate of Phi
u <- .5/.31
alpha <- .01
phi.u <- .5
phi.alpha <- 2/3

prior_bym2 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(u, alpha)),
  phi = list(
    prior = "pc",
    param = c(phi.u, phi.alpha))
)
#=====================================================================================================

#Model 1: binomial
f1 <- Hate ~ Case_rate + Death_rate + Percent_Asian_2020 + Percent_foreign_born + Percent_NH_White +
      Asian_destination + Population_density + Asian_White_poverty_ratio + Asian_White_population_in_managerial_positions +
      Concentrated_disadvantage + Income_inequality
res1 = inla(f1,
            family = "binomial",
            data = geom2,
            Ntrials = Total,
            control.predictor=list(compute=TRUE),
            control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config = TRUE),
            verbose=TRUE
)
summary(res1)

#spatial autocorrelation in residuals
Moran.I(residuals(res1)$deviance.residuals, nn_mat)   

#CPO
mean(log(res1$cpo$cpo))

for (x in 1:17) {   #get coefficients on natural scale
  print(c(inla.emarginal(exp, res1$marginals.fixed[[x]]), inla.qmarginal(c(0.025, 0.5, 0.975),
                                                                         inla.tmarginal(exp, res1$marginals.fixed[[x]]))))
} 

#Model 2: iid
f2 <- Hate ~ 1 + f(idareau, model = "iid")
res2 <- inla(f2,
             family = "binomial",
             data = geom2,
             Ntrials = Total,
             control.predictor=list(compute=TRUE),
             control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config = TRUE),
             verbose=TRUE
)
summary(res2)

#test for spatial autocorrelation in intercept
Moran.I(residuals(res2)$deviance.residuals, nn_mat)   #significant positive spatial autocorrelation

#CPO
mean(log(res2$cpo$cpo))

#Model 3: binomial+BYM2
f3 <- Hate ~ Case_rate + Death_rate + Percent_Asian_2020 + Percent_foreign_born + Percent_NH_White +
  Asian_destination + Population_density + Asian_White_poverty_ratio + Asian_White_population_in_managerial_positions +
  Concentrated_disadvantage + Income_inequality+f(idareau,
                                                  model = "bym2",
                                                  graph = g1,
                                                  hyper = prior_bym2,
                                                  constr = TRUE,
                                                  scale.model = TRUE)
res3 = inla(f3,
            family = "binomial",
            data = geom2,
            Ntrials = Total,
            control.predictor=list(compute=TRUE),
            control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config = TRUE),
            verbose=TRUE
)
summary(res3)
Moran.I(residuals(res3)$deviance.residuals, nn_mat)   #significant positive spatial autocorrelation

#CPO
mean(log(res3$cpo$cpo))

for (x in 1:17) {   #get coefficients on natural scale
  print(c(inla.emarginal(exp, res3$marginals.fixed[[x]]), inla.qmarginal(c(0.025, 0.5, 0.975),
                                                                         inla.tmarginal(exp, res3$marginals.fixed[[x]]))))
} 

#Model 4: zib+bym2
res4 = inla(f3,
            family = "zeroinflatedbinomial0",
            data = geom2,
            Ntrials = Total,
            control.predictor=list(compute=TRUE),
            control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, config = TRUE),
            verbose=TRUE
)
summary(res4)
Moran.I(residuals(res4)$deviance.residuals, nn_mat)   #significant positive spatial autocorrelation

for (x in 1:17) {   #get coefficients on natural scale
  print(c(inla.emarginal(exp, res4$marginals.fixed[[x]]), inla.qmarginal(c(0.025, 0.5, 0.975),
                                                                         inla.tmarginal(exp, res4$marginals.fixed[[x]]))))
} 

#CPO
mean(log(res4$cpo$cpo))
#=====================================================================================================

# #append fitted values to df
df2$fitted <- res3$summary.fitted.values$mean
df2$u <- res3$summary.random$idareau$mean[0:3013]
df2$v <- res3$summary.random$idareau$mean[3014:6026]

#write df to shp
geom <- left_join(geom, df2, by = "GEOID") %>%
  subset(., select = c("GEOID", "fitted", "u", "v", "geometry"))
st_write(geom, "../data/INLA_3108.shp", append = FALSE)

df3 <- geom[!(geom$GEOID %in% df2$GEOID),]
st_write(df3, "../data/INLA_3013.shp", append = FALSE)


