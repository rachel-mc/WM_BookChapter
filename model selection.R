
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


## Comparing the (nested) zero-inflated Poisson model in gamlss with its non-zero-inflated counterpart

LR.test(f12, f13) 
# p-value = 0 so we reject the null hypothesis that the simpler model is better
# We can conclude that the Zero-inflated Poisson model is preferred over the Poisson model 
# However, both are inadequate for this data

## Variable Selection

drop1(f2, test = "Chisq") # dropping vegetation is slightly more favoured
drop1(f3, test = "Chisq") 
drop1(f4, test = "Chisq") 
drop1(f5, test = "Chisq") 
drop1(f6, test = "Chisq") 
drop1(f8, test = "Chisq") 
drop1(f9, test = "Chisq") 


# Cross Validation --------------------------------------------------------

