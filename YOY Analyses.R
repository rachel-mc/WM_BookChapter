## Load the libraries
library(readxl)
library(janitor)
library(dplyr)
library(ggplot)
library(hnp)
library(lme4)
library(corrplot)

## Read in the data
fn <- "~/Desktop/Data/YOY_Striped_Bass_2013_2023.xlsx"

bass <- read_excel(fn, 
                   sheet = "2013-2023_CLEAN") |>
  clean_names() |>
  mutate(year = factor(year),
         station = factor(station),
         tide = as.numeric(tide),
         vegetation = factor(vegetation))

bass

## or complete cases - all NAs removed

## Descriptive statistics
library(FSA)
Summarize(mosa ~ year, data = bass)

library(psych)
describe(bass) # only min and max are informative for categorical variables
# ALl “year-stations”” have 817 values so not including tide in our model due to too many missing values doesn’t matter here

summary(bass) # only tide has NAs - removed as predictor?
# sum(is.na(bass))

## Initial visualisations

# Plot the response
bass |>
  ggplot(aes(x = mosa)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Number of fish caught", y = "Count")

# Plot the response against the main explanatory variable
bass |>
  ggplot(aes(x = as.numeric(year), y = mosa)) +
  geom_point() + 
  theme_bw() +
  labs(x = "Year", y = "Number of fish caught") +
  geom_smooth(se = F) # loess smoothing 
  # + ggplot2:::limits(c(0, 50), "y")
  # + facet_wrap(~ station)

# Effect over time seems to be non-linear - cubic spline (ns?)

## Possible models:
# Poisson GLM (stats::glm)
# Negative Binomial type 2 (glm.nb)
# Zero inflated models
# GLMMs

## Global model - numerical instability (Downdated VtV is not positive definite)

a <- glmer(mosa ~ year + scale(month) + scale(day) + scale(d1aug) + scale(latitude) + scale(longitude) + scale(tide) + scale(temp) + scale(sal) + scale(depth) + vegetation + scale(moam) + (1|station),
           family = poisson(link = "log"),
           data = bass)

b <- glmer(mosa ~ year + scale(d1aug) * scale(longitude) * scale(temp) * scale(sal) * scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
           data = bass)
summary(b)

1 - sum(residuals(b)^2) / sum((bass$mosa - mean(bass$mosa))^2)

## Variable specification: 
# station is modelled as a random intercept - grouping structure
# d1aug incorporates the month and day variables
# latitude is highly correlated with longitude and salinity so this variable is removed
# Numeric variables are scaled
# vegetation converted to numeric to reduce parameters

## Counterexample - model that fits the data well but is inadequate - simulations or STA QUINZE??
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

## Test for correlation between numeric variables
corrplot(cor(bass[,c(3:11, 13)]))

##--------------------------------------------------
cor_mat <- cor(bass[,c(3:11, 13)], use = "complete.obs")
diag(cor_mat) <- NA # ignore correlations of variables with themselves
high_cor <- which(cor_mat > 0.8 | cor_mat < -0.8, arr.ind = TRUE)
high_cor_df <- data.frame(Variable1 = rownames(cor_mat)[high_cor[, 1]],
                          Variable2 = colnames(cor_mat)[high_cor[, 2]],
                          correlation = cor_mat[high_cor])
high_cor_df <- high_cor_df[high_cor_df$Variable1 < high_cor_df$Variable2, ] # alphabetical order to remove duplicates
high_cor_df
##--------------------------------------------------

## Does not converge - change optimizer?
fit2 <- glmer(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              family = poisson(link = "log"),
              data = bass)
summary(fit2)

## Check for (Multi)Collinearity
library(performance)
check_collinearity(fit2) # all VIF < 10

# Fit for illustrative purposes only:
drop1(fit2) # removing vegetation improves the AIC slightly
hnp(fit2) # pattern indicative of overdispersion - variance >> mean, check formally using check_overdispersion

## Try an NB2 model
fit3 <- gam(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + s(station, bs = "re"),
            family = "nb",
            data = bass)
summary(fit3)

drop1(fit3)

## 10 hnp iterations
set.seed(2024)
hfun <- list()
for(i in 1:10) {
  hfun[[i]] <- hnp::hnp(fit1,
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

# for gamlss models (random structure using random() or re())
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
