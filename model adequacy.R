
## Load the libraries
library(hnp)

### Assessing model adequacy using Half Normal Plots 

## lme4
hnp(f2, paint = T, how.many.out = T)

## Helper functions for NB2

dfun <- function(obj) residuals(obj, type = "pearson")

sfun <- function(n, obj) simulate(obj, nsim = n)[[1]]

ffun <- function(response) {
  fit <- glmer.nb(response ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
                  data = bass)
  return(fit)
}
 
hnp(f3,
    newclass = TRUE,
    diagfun = dfun,
    simfun = sfun,
    fitfun = ffun) 

## Helper function for glmmTMB

hnp_glmmTMB <- function(model,
                        ...) {
  
  # Extract components from the model
  f_pred <- formula(model)[[3]]
  fam <- model$call$family
  mod_data <- eval(model$call$data)
  
  dfun <- function(obj) residuals(obj, type = "pearson")
  
  sfun <- function(n, obj) simulate(obj)[[1]]
  
  # Fitting function
  ffun <- function(response) {
    
    fit <- try(glmmTMB(as.formula(response ~ f_pred),
                       family = fam,
                       data = mod_data),
               silent = TRUE)
    
    while(class(fit) == "try-error") {
      response2 <- sfun(1, model)
      ﬁt <- try(glmmTMB(as.formula(response2 ~ f_pred),
                        family = fam,
                        data = mod_data),
                silent = TRUE)
      }
    return(fit)
  }
  
  hnp_results <- hnp(model,
                     newclass = TRUE,
                     diagfun = dfun,
                     simfun = sfun,
                     fitfun = ffun,
                     ...)
  
  return(invisible(hnp_results))
  }

hnp_glmmTMB(f4)
hnp_glmmTMB(f5)
hnp_glmmTMB(f6)
hnp_glmmTMB(f7)
hnp_glmmTMB(f8)
hnp_glmmTMB(f9)
hnp_glmmTMB(f11)

## Helper function for gamlss

hnp_gamlss_count <- function(model, 
                             d_fun, # can use Pearson residuals, otherwise default 'gamlss' residuals
                             ...) {
  
  fam <- model$family[1]
  random_generator <- get(paste0("r", fam))
  params <- model$parameters
  
  n <- length(model$y)
  
  mu_hat <- predict(model, type = "response")
  if("sigma" %in% params) sigma_hat <- predict(model, what = "sigma", type = "response")
  if("nu" %in% params) nu_hat <- predict(model, what = "nu", type = "response")
  if("tau" %in% params) tau_hat <- predict(model, what = "tau", type = "response")
  
  if(length(params) == 1) {
    s_fun <- function(n, obj) {
      y_new <- random_generator(n, mu = mu_hat)
      return(y_new)
    }
  } else if(length(params) == 2) {
    s_fun <- function(n, obj) {
      y_new <- random_generator(n, mu = mu_hat, sigma = sigma_hat)
      return(y_new)
    }
  } else if(length(params) == 3) {
    s_fun <- function(n, obj) {
      y_new <- random_generator(n, mu = mu_hat, sigma = sigma_hat, nu = nu_hat)
      return(y_new)
    }
  } else if(length(parms) == 4) {
    s_fun <- function(n, obj) {
      y_new <- random_generator(n, mu = mu_hat, sigma = sigma_hat, nu = nu_hat, tau = tau_hat)
      return(y_new)
    }
  }
  
  if (missing(d_fun)) d_fun <- function(obj) resid(obj)

  full_data <- eval(model$call$data)
  
  f_fun <- function(y_new) {
    full_data$y <- y_new
    newfit <- update(model, formula = y ~ ., data = full_data)
    return(newfit)
  }
  
  hnp_results <- hnp(model,
                     newclass = TRUE,
                     diagfun = d_fun,
                     simfun = s_fun,
                     fitfun = f_fun,
                     ...)
  
  return(invisible(hnp_results))
}

hnp_gamlss_count(f12)
hnp_gamlss_count(f13)
hnp_gamlss_count(f14)
hnp_gamlss_count(f15)
hnp_gamlss_count(f16)
hnp_gamlss_count(f17)
hnp_gamlss_count(f19)
hnp_gamlss_count(f20)

## 10 hnp runs for each model - SLOW

set.seed(2025)
hnp_list <- list()
for (i in 1:10) {
  hnp_list[[i]] <- hnp(f_,
                       newclass = TRUE,
                       diagfun = dfun,
                       simfun = sfun,
                       fitfun = ffun,
                       how.many.out = TRUE,
                       plot.sim = FALSE)
}

hnp_summary <- sapply(hnp_list, function(x) x$out / x$total * 100) 
HR <- round(FSA::Summarize(hnp_summary), 2)
cat("Mean % [min - max] of residuals outside the simulated envelope = ",HR[2],"%"," [",HR[[4]]," - ",HR[[8]],"]", "\n", "\n")

### Worm plots for gamlss

wp(f12)
wp(f13)
wp(f14)
wp(f15)
wp(f16)
wp(f17)
wp(f19)
wp(f20)
