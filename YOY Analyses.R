## Load the libraries
library(readxl)
library(janitor)
library(dplyr)
library(ggplot2)
library(FSA)
library(psych)
library(hnp)
library(lme4)
library(corrplot)
library(performance)
library(mgcv)
library(glmmTMB)
library(gamlss)
library(gratia)

# Data importation --------------------------------------------------------

fn <- "~/Desktop/Data/YOY_Striped_Bass_2013_2023.xlsx" # file name

bass <- read_excel(fn, 
                   sheet = "2013-2023_CLEAN") |>
  clean_names() |>
  mutate(year = factor(year),
         station = factor(station),
         tide = as.numeric(tide), # Tide is read in as a character
         vegetation = factor(vegetation))

bass

# Reminders ---------------------------------------------------------------

# How to fit quasiPoisson, zero inflated GLMMs in R

# Could use complete cases with all NAs removed
# Show code for filtering the raw data to obtain the subset used
# use a LRT to determine if the random effect is preferred!

# salinity and longitude are expected to be positively associated as you move eastward, but whether this generates estimation problems for the variance is to be determined 

# Descriptive statistics --------------------------------------------------

Summarize(mosa ~ year, data = bass)

# the unbalanced number of stations across the monitored years is not ideal, but maximises the available data within the whole study area
# The “percZero” column tells us that in some years – the first two especially – the percentage of stations with no catch is quite high, often more than half of the time.

describe(bass) # only min and max are informative for categorical variables
# ALl “year-stations”” have 817 values so not including tide in our model due to too many missing values is ok

summary(bass) # only tide has NAs - removed as predictor
# sum(is.na(bass))

# Exploratory visualisations ----------------------------------------------

# Plot the response
bass |>
  ggplot(aes(x = mosa)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Number of fish caught", y = "Count") # observe skewness and outliers

# Compare the outliers with the mean 
# the mean is generally < 4 if not < 3 in some years, especially the first ones
mean(bass$mosa) # global mean

# mean CPUE for each year
bass |>
  filter(year == "2013") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2014") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2015") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2016") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2017") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2018") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2019") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2020") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2021") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2022") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2023") |>
  dplyr::pull(mosa) |>
  mean()

# Plot the response against the main explanatory variable of interest
bass |>
  ggplot(aes(x = as.numeric(year), y = mosa)) +
  geom_point() + 
  theme_bw() +
  labs(x = "Year", y = "Number of fish caught") +
  geom_smooth(se = F) # loess smoothing 
  # + ggplot2:::limits(c(0, 50), "y")
  # + facet_wrap(~ station)

# Effect over time seems to be non-linear - use a cubic spline (ns?)


# Model Fitting: Candidate models --------------------------------------------------------

## Test for correlation between numeric variables
corrplot(cor(bass[,c(3:11, 13)]))

cor_mat <- cor(bass[,c(3:11, 13)], use = "complete.obs")
diag(cor_mat) <- NA # ignore correlations of variables with themselves
high_cor <- which(cor_mat > 0.8 | cor_mat < -0.8, arr.ind = TRUE)
high_cor_df <- data.frame(Variable1 = rownames(cor_mat)[high_cor[, 1]],
                          Variable2 = colnames(cor_mat)[high_cor[, 2]],
                          correlation = cor_mat[high_cor])
high_cor_df <- high_cor_df[high_cor_df$Variable1 < high_cor_df$Variable2, ] # alphabetical order to remove duplicates
high_cor_df

## Example - Global model
# lme4: numerical instability (Downdated VtV is not positive definite) 
f1 <- glmer(mosa ~ year + scale(month) + scale(day) + scale(d1aug) + scale(latitude) + scale(longitude) + scale(tide) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
            family = poisson(link = "log"),
            data = bass)

## Variable specification: 
# station is modelled as a random intercept - grouping structure, account for pseudo-replication
# d1aug incorporates the month and day variables
# latitude is highly correlated with longitude and salinity so this variable is removed
# Numeric variables are scaled
# vegetation is categorical with five levels so it was converted to numeric to reduce parameters

## GLMMs
# Poisson 
var(bass$mosa) 
mean(bass$mosa) # variance >> mean: counts are overdispersed
# Negative Binomial type 2 
# Zero inflated models

### lme4
## Poisson
f3 <- glmer(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
            family = poisson(link = "log"),
            data = bass)

summary(f3) # model does not converge - change optimizer?

f4 <- glm(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + station,
          family = poisson(link = "log"),
          data = bass)

summary(f4)

anova(f3, f4, test = "Chisq") # the model with the random effect is preferred

## Check for (Multi)Collinearity
check_collinearity(f3) # all VIF < 10

# For illustrative purposes only:
drop1(f3) # removing vegetation improves the AIC slightly

hnp(f3,
    how.many.out = T,
    paint = T) # pattern indicative of overdispersion - variance >> mean, check formally using check_overdispersion

## Quasipoisson - "quasi" families cannot be used in glmer

## NB2 - how to choose value for theta?
f4 <- glmer(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
            family = negative.binomial(theta = .05),
            data = bass)

summary(f4)

f5 <- glmer.nb(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
               data = bass)

summary(f5)

## Zero-inflated Poisson - NA?

### glmmTMB
## Poisson
f6 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              family = poisson(link = "log"),
              data = bass)

summary(f6)

## Zero-inflated Poisson
f7 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              zi = ~ year, # include all fixed predictors?
              family = poisson(link = "log"),
              data = bass)

summary(f7)

## Hurdle Poisson
f8 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              zi = ~ year, # include all fixed predictors?
              family = truncated_poisson,
              data = bass)

summary(f8)

## Negative Binomial type 2
f9 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              family = nbinom2,
              data = bass)

summary(f9)

## Zero-inflated Negative Binomial type 2
f10 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              zi = ~ year, # include all fixed predictors?
              family = nbinom2,
              data = bass)

summary(f10)

### gamlss
## Poisson
f11 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = PO,
              data = na.omit(bass))

summary(f11)

## Zero-inflated Poisson
f12 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = ZIP,
              data = na.omit(bass))

summary(f12)

## Negative-Binomial Type 2
f13 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = NBI,
              data = na.omit(bass))

summary(f13)

## Generalised Poisson
f14 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = GPO,
              data = na.omit(bass))


summary(f14)

## Poisson Inverse Gaussian
f15 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = PIG,
              data = na.omit(bass))

summary(f15)

## Double Poisson
f16 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = DPO,
              data = na.omit(bass))

summary(f16)


# Model Adequacy ----------------------------------------------------------

### hnp

## Diagnostic helper functions for glmmTMB

dfun <- function(obj) residuals(obj, type = "pearson")

sfun <- function(n, obj) simulate(obj)[[1]]
  
# Fitting function: Pre-specify model

ffun <- function(model = f6,
                 response,
                 ...) {
  
  f_pred <- deparse(formula(model)[[3]])
  fam <- model$call$family
  mod_data <- eval(model$call$data)
  
  ﬁt <- try(glmmTMB(response ~ f_pred,
                    family = fam,
                    data = mod_data),
            silent = TRUE)
  
  while(class(fit) == "try-error") {
    response2 <- sfun(1, model)
    ﬁt <- try(glmmTMB(response2 ~ f_pred,
                      family = fam,
                      data = mod_data),
              silent = TRUE)
    }
  return(ﬁt)
  }

hnp(f6,
    newclass = TRUE,
    diagfun = dfun,
    simfun = sfun,
    fitfun = ffun,
    paint = T)

## Helper function for gamlss

hnp_gamlss <- function(model, 
                       d_fun, # can use Pearson residuals, otherwise default
                       ...) {
  
  fam <- model$family[1]
  random_generator <- get(paste0("r", fam))
  params <- model$parameters
  
  n <- length(model$y)
  
  mu_hat <- predict(model, type = "response")
  if("sigma" %in% params) sigma_hat <- predict(model, what = "sigma", type = "response")
  if("nu" %in% params) nu_hat <- predict(model, what = "nu", type = "response")
  if("tau" %in% params) tau_hat <- predict(model, what = "tau", type = "response")
  
  if(length(params) == 1) {
    s_fun <- function(n, obj) {
      y_new <- random_generator(n, mu = mu_hat)
      return(y_new)
    }
  } else if(length(params) == 2) {
    s_fun <- function(n, obj) {
      y_new <- random_generator(n, mu = mu_hat, sigma = sigma_hat)
      return(y_new)
    }
  } else if(length(params) == 3) {
    s_fun <- function(n, obj) {
      y_new <- random_generator(n, mu = mu_hat, sigma = sigma_hat, nu = nu_hat)
      return(y_new)
    }
  } else if(length(parms) == 4) {
    s_fun <- function(n, obj) {
      y_new <- random_generator(n, mu = mu_hat, sigma = sigma_hat, nu = nu_hat, tau = tau_hat)
      return(y_new)
    }
  }
  
  if(missing(d_fun)) {
    d_fun <- function(obj) resid(obj)
  }
  
  full_data <- eval(model$call$data)
  
  f_fun <- function(y_new) {
    full_data$y <- y_new
    newfit <- update(model, formula = y ~ ., data = full_data)
    return(newfit)
  }
  
  hnp_results <- hnp(model,
                     newclass = TRUE,
                     diagfun = d_fun,
                     simfun = s_fun,
                     fitfun = f_fun,
                     ...)
  
  return(invisible(hnp_results))
}


### worm plots

wp()


# Counterexamples ---------------------------------------------------------

## Want a model that fits the data well but is inadequate - use simulations or STA QUINZE instead?
## Fit a high degree polynomial to year
fit1 <- glmer(mosa ~ poly(year, 10) + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + vegetation + scale(moam) + (1|station),
              data = bass)

summary(fit1)

## R^2
cor(bass$mosa, fitted(fit1))^2
1 - sum(residuals(fit1)^2) / sum((bass$mosa - mean(bass$mosa))^2)

## Fit a Gaussian GLMM (default) - incorrect: response variable is counts
fit2 <- glmer(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              data = bass)

summary(fit2)

hnp(fit2,
    how.many.out = TRUE,
    paint = T)

## The Poisson model has a higher R^2 than NB but it is inadequate
cor(bass$mosa, fitted(fit2))^2
cor(bass$mosa, fitted(fit3))^2

#######################################################################################



## Try an NB2 model
fit3 <- gam(mosa ~ year + d1aug + longitude + temp + sal + depth + as.numeric(vegetation) + moam + s(station, bs = "re"),
            family = "nb",
            method = "ML",
            data = bass)
summary(fit3)

drop1(fit2, type = "Chisq")

## Model selection 
library(itsadug)
compareML()

library(MuMIn)

## Model adequacy

## hnp helper functions
# hnp helper functions 
sfun <- function(n, fit3) {
  y <- stats::rnbinom(nrow(bass),
                      size = fit3$family$getTheta(TRUE),
                      mu = predict(fit3, type = "response"))
  return(y)
  }


ffun <- function(resp) {
  mgcv::gam(resp ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + s(station, bs = "re"),
            family = "nb",
            method = "ML",
            data = bass)
}

dfun <- function(mod) stats::resid(mod, type = "pearson")

## 10 hnp iterations
set.seed(2024)
hfun <- list()
for(i in 1:10) {
  hfun[[i]] <- hnp::hnp(fit3,
                        newclass = TRUE,
                        diagfun = dfun,
                        simfun = sfun,
                        fitfun = ffun, 
                        how.many.out = TRUE,
                        plot.sim = FALSE)
}

hnp_summary <- sapply(hfun, function(x) x$out / x$total * 100) 
HR <- round(FSA::Summarize(hnp_summary), 2)
cat("Mean % [min - max] of residuals outside the simulated envelope = ",HR[2],"%"," [",HR[[4]]," - ",HR[[8]],"]", "\n", "\n")

## Zero-inflated models

## glmmTMB models

## Overdispersion:
# Negative Binomial type II model in mgcv
# Generalized Poisson, Double Poisson, Poisson-inverse Gaussian, and a Negative Binomial type 1 model in gamlss
# Generalized Poisson, Conway-Maxwell Poisson, and a Negative Binomial type 1 model in glmmTMB

## gamlss models 
fit4 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
               family = GPO,
               data = bass)


library(MuMIn)
model_selection <- model.sel(fit1_gamlss, fit2_gamlss, fit3_gamlss)
candidate_models <- get.models(model_selection, subset = TRUE)
top_ranking_model <- candidate_models[[1]]

fitted <- predict(top_ranking_model, type = "link", se.fit = TRUE)
pred <- exp(fitted$fit)
LL <- exp(fitted$fit - 1.96 * fitted$se.fit)
UL <- exp(fitted$fit + 1.96 * fitted$se.fit)
pred_mean <- pred[[1]]
ci_ll <- LL[[1]]
ci_ul <- UL[[1]]

observed_mean <- mean(top_ranking_model$y)
round(cbind(pred_mean, ci_ll, ci_ul, observed_mean), 2)
