
## Load the libraries

library(gamlss)
library(hnp)


# Assessing model adequacy using Half Normal Plots ------------------------


## lme4

set.seed(2025) # for reproducability

## Poisson model did not converge - for illustrative purposes only
hnp(f2,
    how.many.out = TRUE,
    paint = T)

### Worm plots for converged 'gamlss' models

wp(f12)
wp(f13)
wp(f14)
wp(f16)
wp(f17)
wp(f18)
wp(f20)
wp(f21)
