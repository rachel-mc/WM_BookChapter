
## Load the libraries
library(MuMIn)

### Likelihood Ratio Tests for Nested models
drop1(f4) # removing year improves the AIC??

### Information Criteria
# lme4
AIC(f2, f3)

### Cross Validation