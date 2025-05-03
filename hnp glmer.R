
## Load the libraries

library(glmer)
library(hnp)

# Helper functions for glmer.nb()

dfun <- function(obj) residuals(obj, type = "pearson")
  
sfun <- function(n, obj) simulate(obj, nsim = n)[[1]]

ffun <- function(response) {
  data_copy <- bass
  data_copy$new_response <- response
  glmer.nb(new_response ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
           data = data_copy)
}

set.seed(2025)

hnp(f3,
    newclass = TRUE,
    diagfun = dfun,
    simfun = sfun,
    fitfun = ffun,
    how.many.out = T,
    paint = T)
