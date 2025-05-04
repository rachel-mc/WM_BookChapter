
## Load the libraries

library(glmmTMB)
library(hnp)

## Helper function for glmmTMB models

hnp_glmmTMB <- function(model, 
                        ...) {
  
  # Extract components from the model
  fam <- model$call$family
  mod_data <- eval(model$call$data)
  zi <- model$call$ziformula
  
  # Diagnostic function
  dfun <- function(obj) residuals(obj, type = "pearson")
  
  # Simulation function
  sfun <- function(n, obj) simulate(obj, nsim = n)[[1]]
  
  # Fitting function
  ffun <- function(resp) {
    mod_data$new_response <- resp
    fit <- try(glmmTMB(new_response ~ year + scale(d1aug) + scale(longitude)
                       + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) 
                       + scale(moam) + (1 | station), 
                       family = fam,
                       data = mod_data,
                       ziformula = zi),
               silent = TRUE)
    
    while (inherits(fit, "try-error")) {
      response2 <- sfun(1, model)
      mod_data$response2 <- response2
      fit <- try(glmmTMB(response2 ~ year + scale(d1aug) + scale(longitude)
                         + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) 
                         + scale(moam) + (1 | station), 
                         family = fam,
                         data = mod_data,
                         ziformula = zi),
                 silent = TRUE)
    }
    return(fit)
  }
  
  # Run hnp
  hnp_results <- hnp(model,
                     newclass = TRUE,
                     diagfun = dfun,
                     simfun = sfun,
                     fitfun = ffun,
                     ...)
  
  return(invisible(hnp_results))
}

set.seed(2025)

hnp_glmmTMB(f4,
            how.many.out = T,
            paint = T,
            sim = 19, 
            conf = 1)

hnp_glmmTMB(f5,
            how.many.out = T,
            paint = T,
            sim = 19, 
            conf = 1)

hnp_glmmTMB(f6,
            how.many.out = T,
            paint = T,
            sim = 19, 
            conf = 1)

hnp_glmmTMB(f7,
            how.many.out = T,
            paint = T,
            sim = 19, 
            conf = 1)

hnp_glmmTMB(f8,
            how.many.out = T,
            paint = T,
            sim = 19, 
            conf = 1)

hnp_glmmTMB(f9,
            how.many.out = T,
            paint = T,
            sim = 19, 
            conf = 1)

hnp_glmmTMB(f10,
            how.many.out = T,
            paint = T,
            sim = 19, 
            conf = 1)

hnp_glmmTMB(f11,
            how.many.out = T,
            paint = T,
            sim = 19, 
            conf = 1)
