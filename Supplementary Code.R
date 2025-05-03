
## Load the libraries

library(gamlss)
library(janitor)
library(performance)
library(readxl)
library(tidyverse)


# Data Cleaning -----------------------------------------------------------


# Code to filter the raw data to obtain the 'clean' subset used 

fn <- "~/Desktop/Data/YOY_Striped_Bass_2013_2023.xlsx"

bass_raw <- read_excel(fn, 
                       sheet = "2013-2023-ALL") |>
  clean_names() |>
  mutate(year = factor(year),
         station = factor(station),
         tide = as.numeric(tide), 
         vegetation = factor(vegetation))

counts <- bass_raw |> filter(mosa >= 1) |> dplyr::select(station)

at_least_one <- bass_raw |>
  semi_join(counts, by = "station")

no_NA <- at_least_one[complete.cases(at_least_one[,-8]),] ### not 817


# Inadequate Model Example --------------------------------------------------------


# Use the Gaussian distribution (glmer default)
# This is incorrect as the response variable is counts

fit3 <- glmer(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              data = bass)

summary(fit3)

set.seed(2025)

hnp(fit3,
    how.many.out = TRUE,
    paint = T)


# Generalised Linear Model (GLM) ------------------------------------------------


## Fitting the GLM counterpart of the Poisson GLMM in lme4 (no random effect)
fit4 <- glm(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + station,
          family = poisson(link = "log"),
          data = bass)

summary(fit4)

# Likelihood Ratio Test
anova(f2, fit4, test = "Chisq") 
# the model with the random effect is preferred

# Half Normal plot of the Poisson GLMM
hnp(f2,
    how.many.out = T,
    paint = T) 
# pattern indicative of overdispersion - variance >> mean


# Assess the effect of Tide on Mosa -----------------------------------------------


## Fit an adequate PIG model in 'gamlss'

fit5 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(tide) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = PIG,
              data = na.omit(bass)) # already reduced dataset

summary(fit5)


# Variable Significance ---------------------------------------------------


## Covariates that have no statistical support could be removed from the data to further maximise the available information



# Temporal Autocorrelation ------------------------------------------------


check_autocorrelation(f18)



# Standard errors ---------------------------------------------------------


## Compare the inference errors of adequate (PIG) vs inadequate (PO) models

