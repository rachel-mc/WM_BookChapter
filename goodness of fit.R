
## Load the libraries

library(ggplot2)
library(gratia)
library(MuMIN)
library(performance)


# R^2 ---------------------------------------------------------------------

r2(f3)
r2(f4) # Poisson model has good fit but is inadequate

Rsq(f17) 
Rsq(f18) # GPO and DPO have moderate goodness-of-fit but are adequate
