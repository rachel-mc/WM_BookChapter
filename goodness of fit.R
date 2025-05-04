
## Load the libraries

library(gamlss)
library(performance)


# Pseudo/Conditional/Marginal R^2 ---------------------------------------------------------------------

## lme4

r2(f3) # Negative Binomial type 2 model has a moderate fit to the data

## glmmTMB

r2(f4) # Poisson model has an excellent fit to the data but is inadequate
r2(f5) # Zero-inflated Poisson model has an ok fit to the data
r2(f6) # Hurdle Poisson model has a very high variance explained by both the fixed and random effects
r2(f7) # Negative Binomial type 2 model has a decent fit to the data
r2(f8) # Zero-inflated Negative Binomial type 2 model has an excellent fit to the data
r2(f9) # Generalised Poisson model has a good fit to the data
r2(f10) ## Slow and nonsensical
r2(f11) # Negative Binomial type 1 model has a good fit to the data

## gamlss

# Value is interpreted as the proportion of deviance explained by the model
# How well the mean structure explains the response

Rsq(f12) # Poisson model has an excellent fit to the data but is inadequate
Rsq(f13) # Zero-inflated Poisson model has an even better fit to the data but is inadequate
Rsq(f14) # Negative Binomial type 2 model has a poor fit to the data
Rsq(f16) # Negative Binomial type 1 model has a poor fit to the data
Rsq(f17) # Generalised Poisson model is adequate but has a mediocre fit to the data
Rsq(f18) # Poisson-Inverse Gaussian model is adequate but has a mediocre fit to the data
Rsq(f20) # Model with a variance structure has a poor fit to the data
Rsq(f21) # Model with a cubic natural spline has has a poor fit to the data
