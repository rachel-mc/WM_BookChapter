## Load the libraries

library(corrplot)
library(dplyr)
library(FSA)
library(gamlss)
library(ggplot2)
library(glmmTMB)
library(gratia)
library(hnp)
library(itsadug)
library(janitor)
library(lme4)
library(mgcv)
library(MuMIn)
library(performance)
library(psych)
library(readxl)

# Data importation --------------------------------------------------------

# File name
fn <- "~/Desktop/Data/YOY_Striped_Bass_2013_2023.xlsx"

bass <- read_excel(fn, 
                   sheet = "2013-2023_CLEAN") |>
  clean_names() |>
  mutate(year = factor(year),
         station = factor(station),
         tide = as.numeric(tide), # Tide is read in as a character
         vegetation = factor(vegetation))

bass

# Descriptive statistics --------------------------------------------------

Summarize(mosa ~ year, data = bass)

# inconsistent observations across years but this maximises the available data
# the percentage of stations with no catch is quite high

describe(bass) # only min and max are informative for categorical variables

summary(bass) # only tide has NAs 
# sum(is.na(bass))

# Exploratory visualisations and metrics ----------------------------------------------

# Plot the response
bass |>
  ggplot(aes(x = mosa)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Number of fish caught", y = "Count") # observe skewness and outliers

# Compare the outliers (2018 & 2019) with the mean 
mean(bass$mosa) # global mean

# Mean captures by year
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
  ggplot(aes(x = as.numeric(as.character(year)), y = mosa)) + # for x-axis labels
  geom_point() + 
  theme_bw() +
  labs(x = "Year", y = "Number of fish caught") +
  scale_x_continuous(breaks = 2013:2023) +
  geom_smooth(se = F) # loess smoothing 

# Reduced scale on y-axis
bass |>
  ggplot(aes(x = as.numeric(as.character(year)), y = mosa)) + 
  geom_point() + 
  theme_bw() +
  labs(x = "Year", y = "Number of fish caught") +
  scale_x_continuous(breaks = 2013:2023) +
  geom_smooth(se = F) +
  ggplot2:::limits(c(0, 50), "y")

# Per station
bass |>
  ggplot(aes(x = as.numeric(as.character(year)), y = mosa)) + 
  geom_point() + 
  theme_bw() +
  labs(x = "Year", y = "Number of fish caught") +
  scale_x_continuous(breaks = 2013:2023) +
  geom_smooth(se = F) +
  ggplot2:::limits(c(0, 50), "y") +
  facet_wrap(~ station)



# Model Adequacy ----------------------------------------------------------

### hnp

## lme4
hnp(f2)
hnp(f4)
hnp(f5)

## Diagnostic helper functions for glmmTMB - models 6 to 13

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
    paint = T) # error

## Helper function for gamlss

hnp_gamlss_count <- function(model, 
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

hnp_gamlss_count(f14)
hnp_gamlss_count(f15)
hnp_gamlss_count(f16)
hnp_gamlss_count(f17)
hnp_gamlss_count(f18)
hnp_gamlss_count(f19)

## 10 hnp runs for each model - SLOW

set.seed(2025)
hnp_list <- list()
for (i in 1:10) {
  hnp_list[[i]] <- hnp(f14,
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

set.seed(2025)
hnp_list <- list()
for (i in 1:10) {
  hnp_list[[i]] <- hnp(f15,
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

set.seed(2025)
hnp_list <- list()
for (i in 1:10) {
  hnp_list[[i]] <- hnp(f16,
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

set.seed(2025)
hnp_list <- list()
for (i in 1:10) {
  hnp_list[[i]] <- hnp(f17,
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

set.seed(2025)
hnp_list <- list()
for (i in 1:10) {
  hnp_list[[i]] <- hnp(f18,
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

set.seed(2025)
hnp_list <- list()
for (i in 1:10) {
  hnp_list[[i]] <- hnp(f19,
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

### Worm plots for gamlss

wp(f14)
wp(f15)
wp(f16)
wp(f17)
wp(f18)
wp(f19)

# Model Selection ---------------------------------------------------------

# Use pre-determined adequate models only

# Chi squared goodness of fit test?

# IC
model_selection <- model.sel(f11, f12, f13, f14, f15, f16) # adequate models only
candidate_models <- get.models(model_selection, subset = TRUE)
top_ranking_model <- candidate_models[[1]]

# Predictions -------------------------------------------------------------

fitted <- predict(top_ranking_model, type = "link", se.fit = TRUE)
pred <- exp(fitted$fit)
LL <- exp(fitted$fit - 1.96 * fitted$se.fit)
UL <- exp(fitted$fit + 1.96 * fitted$se.fit)
pred_mean <- pred[[1]]
ci_ll <- LL[[1]]
ci_ul <- UL[[1]]

observed_mean <- mean(top_ranking_model$y)
plot_data <- round(cbind(Year = bass$year, pred, ci_ll, ci_ul), 2)

## Visualisation 

ggplot(data = plot_data, aes(x = Year)) +
  geom_point(aes(y = pred)) +
  geom_errorbar(aes(ymin = ci_ll, ymax = ci_ul)) +
  # geom_hline(yintercept = , col = "red", lwd = 1.1, lty = 2)
  labs(main = "Fitted Model estimates with 95% error bars",
       x = "Year",
       y = "CPUE (n/station)")
