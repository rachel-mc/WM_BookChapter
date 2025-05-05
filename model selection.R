
## Load the libraries

library(gamlss)
library(MuMIn)


# Information Criteria ----------------------------------------------------


### lme4: Only the Negative Binomial model converged so there is no model comparison in this case


### glmmTMB

model.sel(f4, f5, f6, f7, f8, f9, f10, f11, # use pre-determined ADEQUATE models only
          rank = "AIC") 

model.sel(f4, f5, f6, f7, f8, f9, f10, f11) 

model.sel(f4, f5, f6, f7, f8, f9, f10, f11,
          rank = "BIC") 

## BICc function

BICc <- function (obj) {
  bic <- BIC(obj)
  n <- nrow(obj$data)
  bicc <- bic + (log(n) * (np + 1) * np)/(n - np - 1)
  return(bicc)
}

model.sel(f4, f5, f6, f7, f8, f9, f10, f11,
          rank = "BICc") ## BICc function

## In all cases, the Negative Binomial type 2 model is preferred

### gamlss 

# BICc is not calculable for gamlss models - mathematical error

model.sel(f17, f18, # use pre-determined adequate models only
          rank = "AIC")

model.sel(f17, f18)

model.sel(f17, f18,
          rank = "BIC")

## The Generalised Poisson model is preferred by AIC and AICc but the Poisson Inverse Gaussian model
## is preferred by BIC

model_selection <- model.sel(f17, f18)
model_selection
candidate_models <- get.models(model_selection, subset = TRUE)
candidate_models
top_ranking_model <- candidate_models[[1]]
top_ranking_model


# Likelihood Ratio Tests --------------------------------------------------


## Variable Selection - adequate models only

drop1(f3, test = "Chisq") # dropping depth is slightly more favoured

drop1(f4, test = "Chisq") # dropping vegetation is slightly more favoured
drop1(f5, test = "Chisq") # model is preferred as is
drop1(f6, test = "Chisq") # model is preferred as is
drop1(f7, test = "Chisq") # dropping sal or depth is slightly more favoured
drop1(f8, test = "Chisq") # dropping sal or depth is slightly more favoured
drop1(f9, test = "Chisq") # dropping d1aug is slightly more favoured
# drop1(f10, test = "Chisq") - ignored due to run time
drop1(f11, test = "Chisq") # dropping d1aug is slightly more favoured

drop1(f17) # dropping d1aug, sal, depth, or vegetation is slightly more favoured
drop1(f18) # dropping d1aug, sal, or depth is slightly more favoured


# Cross Validation --------------------------------------------------------


## Compare the predictive performance of the adequate gamlss models

# Generalised Poisson 

n <- nrow(na.omit(bass))
pred <- numeric(n) 

for (i in 1:n) {
  train_data <- na.omit(bass)[-i,] 
  test_data <- na.omit(bass)[i, , drop = FALSE]
  
  mod <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
                family = "GPO",
                data = train_data)  
  
  pred[i] <- predict(mod, newdata = test_data, type = "response")
}

mean((bass$mosa - pred)^2) # Mean-squared error = 1.765303e+27

## Poisson-Inverse Gaussian

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

mean((bass$mosa - pred)^2) # Mean-squared error = 782023.8
