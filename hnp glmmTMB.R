
## Load the libraries

library(glmmTMB)
library(hnp)

## Helper function for glmmTMB

hnp_glmmTMB <- function(model, 
                        ...) {
  
  # Extract components from the model
  fam <- eval(model$call$family)
  mod_data <- eval(model$call$data)
  
  # Diagnostic function
  dfun <- function(obj) residuals(obj, type = "pearson")
  
  # Simulation function
  sfun <- function(n, obj) simulate(obj, nsim = n)[[1]]
  
  # Fitting function
  ffun <- function(response) {
    mod_data$response <- response
    fit <- try(glmmTMB(response ~ ., 
                       family = fam,
                       data = mod_data),
               silent = TRUE)
    
    while (inherits(fit, "try-error")) {
      response2 <- sfun(1, model)
      mod_data$response2 <- response2
      fit <- try(glmmTMB(response2 ~ ., 
                         family = fam,
                         data = mod_data),
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

hnp_glmmTMB(f4)
hnp_glmmTMB(f5)
hnp_glmmTMB(f6)
hnp_glmmTMB(f7)
hnp_glmmTMB(f8)
hnp_glmmTMB(f9)
hnp_glmmTMB(f11)
