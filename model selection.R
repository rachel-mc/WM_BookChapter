
## Load the libraries
library(MuMIn)


# Information Criteria ----------------------------------------------------


BICc <- function (obj) {
  bic <- BIC(obj)
  n <- nrow(obj$data)
  bicc <- bic + (log(n) * (np + 1) * np)/(n - np - 1)
  return(bicc)
}

# lme4
AIC()
BIC()
AICc()
BICc()

## glmmTMB

## gamlss

model_selection <- model.sel(f17, f18) # use pre-determined adequate models only
model_selection
candidate_models <- get.models(model_selection, subset = TRUE)
candidate_models
top_ranking_model <- candidate_models[[1]]
top_ranking_model


# Likelihood Ratio Tests --------------------------------------------------


## Compare the nested zero-inflated models with their non-zero-inflated counterparts
LR.test(f12, f13)
LR.test(f14, f16)

## Variable Selection
drop1(f2, test = "Chisq") # dropping vegetation is slightly more favoured
drop1(f3, test = "Chisq") 
drop1(f4, test = "Chisq") 
drop1(f5, test = "Chisq") 
drop1(f6, test = "Chisq") 
drop1(f8, test = "Chisq") 
drop1(f9, test = "Chisq") 


# Cross Validation --------------------------------------------------------

