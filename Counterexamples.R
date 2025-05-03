
## Load the libraries

library(gamlss)
library(MuMIn)

# Case 1 ------------------------------------------------------------------

## Demonstrate the mathematical fact that R^2 increases as more predictors are added 

## We fit a(n inadequate) linear model so that R^2 is directly applicable 

m1 <- lm(mosa ~ year + scale(d1aug),
             data = bass)

a <- summary(m1)
a$r.squared

m2 <- lm(mosa ~ year + scale(d1aug) + scale(longitude),
         data = bass)

b <- summary(m2)
b$r.squared

m3 <- lm(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp),
         data = bass)

d <- summary(m3)
d$r.squared

# 0.08288146 > 0.06878097 > 0.04678821


# Case 2 ------------------------------------------------------------------

## An inadequate model is chosen from the candidate set

# The Poisson, Zero-inflated Poisson, NB2, and NB1 models fitted in 
# 'gamlss' are all inadequate according to 'hnp'

model.sel(f12, f13, f14, f16, rank = "AIC")
model.sel(f12, f13, f14, f16)
model.sel(f12, f13, f14, f16, rank = "BIC")

# The NB2 model is chosen as the top-ranking model according to AIC, AICc, and BIC,
# despite being inadequate.


# Case 3 ------------------------------------------------------------------

## Model that fits the data well but is inadequate

## Fit a saturated Poisson model to the data

n <- nrow(na.omit(bass))
mosa <- na.omit(bass) |> dplyr::pull(mosa)
x <- gl(n, k = 1) # factor with the same number of levels as data points 

fit1 <- gamlss(mosa ~ x,
               family = PO)

plot(mosa)
lines(fitted(fit1))

Rsq(fit1) # pseudo R^2 of gamlss (for illustrative purposes)

set.seed(2025) # for reproducable results

hnp_gamlss_count(fit1,
                 how.many.out = T,
                 paint = T)

## The Poisson distribution is inadequate as these counts are overdispersed.

# Case 4 ------------------------------------------------------------------

## An adequate model that has a poor fit

# Try the null PIG model

fit2 <- gamlss(mosa ~ 1,
              family = PIG,
              data = na.omit(bass))

summary(fit)

hnp_gamlss_count(fit2,
                 how.many.out = TRUE,
                 paint = T)

# Case 5 ------------------------------------------------------------------

## Compare the predictive ability of an adequate (PIG) and inadequate (PO) model 

## LOO-CV in 'gamlss'
n <- nrow(na.omit(bass))
pred <- numeric(n) # create space to store predictions

for (i in 1:n) {
  train_data <- na.omit(bass)[-i,] # omit the ith row
  test_data <- na.omit(bass)[i, , drop = FALSE]
  
  # Fit the model using the training data
  mod <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
                family = "PO",
                data = train_data)  
  
  # Predict on left-out observation
  pred[i] <- predict(mod, newdata = test_data, type = "response")
  }

mean((bass$mosa - pred)^2) # mean squared error: 1903.072

n <- nrow(na.omit(bass))
pred <- numeric(n) 

for (i in 1:n) {
  train_data <- na.omit(bass)[-i,] 
  test_data <- na.omit(bass)[i, , drop = FALSE]

  mod <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
                family = "PIG",
                data = train_data)  
  
  pred[i] <- predict(mod, newdata = test_data, type = "response")
}

mean((bass$mosa - pred)^2) # 782023.8

## The inadequate model has a lower predictive error :O
