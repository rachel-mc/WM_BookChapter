
## Load the libraries

library(crayon)
library(FSA)
library(gamlss)
library(hnp)

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

set.seed(2025)

## These plots can take quite some time to run.

hnp_gamlss_count(f12,
                 how.many.out = T,
                 paint = T)

hnp_gamlss_count(f13, # particularly slow
                 how.many.out = T,
                 paint = T)

hnp_gamlss_count(f14,
                 how.many.out = T,
                 paint = T)

hnp_gamlss_count(f16, # particularly slow
                 how.many.out = T,
                 paint = T)

hnp_gamlss_count(f17,
                 how.many.out = T,
                 paint = T)
## Error in apply(res, 1, quantile, c((1 - conf)/2, 0.5, (1 - conf)/2 + conf)) : 
## dim(X) must have a positive length

hnp_gamlss_count(f18,
                 how.many.out = T,
                 paint = T)

hnp_gamlss_count(f20,
                 how.many.out = T,
                 paint = T)

hnp_gamlss_count(f21,
                 how.many.out = T,
                 paint = T)

## 10 hnp runs for the convergent 'gamlss' models.
## These for loops can take quite some time to run.

set.seed(2025)

hnp_list <- list()

for (i in 1:10) {
  hnp_list[[i]] <- hnp_gamlss_count(f12,
                                    how.many.out = T,
                                    plot.sim = F)
  }

hnp_summary <- sapply(hnp_list, function(x) x$out / x$total * 100) 

HR <- round(Summarize(hnp_summary), 2)

cat(red("Mean % [min - max] of residuals outside the simulated envelope = ",HR[2],"%"," [",HR[[4]]," - ",HR[[8]],"]", "\n", "\n"))

## 99.31 %  [ 99.07  -  99.6 ] 

set.seed(2025)

hnp_list <- list()

# VERY SLOW
for (i in 1:10) {
  hnp_list[[i]] <- hnp_gamlss_count(f13,
                                    how.many.out = T,
                                    plot.sim = F)
}

hnp_summary <- sapply(hnp_list, function(x) x$out / x$total * 100) 

HR <- round(Summarize(hnp_summary), 2)

cat(red("Mean % [min - max] of residuals outside the simulated envelope = ",HR[2],"%"," [",HR[[4]]," - ",HR[[8]],"]", "\n", "\n"))

## 58.07 %  [ 55.98  -  59.71 ] 

set.seed(2025)

hnp_list <- list()

for (i in 1:10) { 
  hnp_list[[i]] <- hnp_gamlss_count(f14,
                                    how.many.out = T,
                                    plot.sim = F)
}

hnp_summary <- sapply(hnp_list, function(x) x$out / x$total * 100) 

HR <- round(Summarize(hnp_summary), 2)

cat(red("Mean % [min - max] of residuals outside the simulated envelope = ",HR[2],"%"," [",HR[[4]]," - ",HR[[8]],"]", "\n", "\n"))

## 47.87 %  [ 36.84  -  58.24 ] 

set.seed(2025)

hnp_list <- list()

# VERY VERY SLOW
for (i in 1:10) {
  hnp_list[[i]] <- hnp_gamlss_count(f16,
                                    how.many.out = T,
                                    plot.sim = F)
}

hnp_summary <- sapply(hnp_list, function(x) x$out / x$total * 100) 

HR <- round(Summarize(hnp_summary), 2)

cat(red("Mean % [min - max] of residuals outside the simulated envelope = ",HR[2],"%"," [",HR[[4]]," - ",HR[[8]],"]", "\n", "\n"))

## 77.31 %  [ 72.07  -  80.32 ]


## Issues with 10 hnp iterations for GPO
set.seed(2025)

hnp_list <- list()

for (i in 1:10) {
  hnp_list[[i]] <- hnp_gamlss_count(f17,
                                    how.many.out = T,
                                    plot.sim = F)
}

hnp_summary <- sapply(hnp_list, function(x) x$out / x$total * 100) 

HR <- round(Summarize(hnp_summary), 2)

cat(red("Mean % [min - max] of residuals outside the simulated envelope = ",HR[2],"%"," [",HR[[4]]," - ",HR[[8]],"]", "\n", "\n"))

set.seed(2025)

hnp_list <- list()

# VERY VERY VERY VERY SLOW
for (i in 1:10) {
  hnp_list[[i]] <- hnp_gamlss_count(f18,
                                    how.many.out = T,
                                    plot.sim = F)
}

hnp_summary <- sapply(hnp_list, function(x) x$out / x$total * 100) 

HR <- round(Summarize(hnp_summary), 2)

cat(red("Mean % [min - max] of residuals outside the simulated envelope = ",HR[2],"%"," [",HR[[4]]," - ",HR[[8]],"]", "\n", "\n"))

## 1.36 %  [ 0  -  3.46 ]

set.seed(2025)

hnp_list <- list()

for (i in 1:10) {
  hnp_list[[i]] <- hnp_gamlss_count(f20,
                                    how.many.out = T,
                                    plot.sim = F)
}

hnp_summary <- sapply(hnp_list, function(x) x$out / x$total * 100) 

HR <- round(Summarize(hnp_summary), 2)

cat(red("Mean % [min - max] of residuals outside the simulated envelope = ",HR[2],"%"," [",HR[[4]]," - ",HR[[8]],"]", "\n", "\n"))

## 67.57 %  [ 60.77  -  73.94 ]

set.seed(2025)

hnp_list <- list()

for (i in 1:10) {
  hnp_list[[i]] <- hnp_gamlss_count(f21,
                                    how.many.out = T,
                                    plot.sim = F)
}

hnp_summary <- sapply(hnp_list, function(x) x$out / x$total * 100) 

HR <- round(Summarize(hnp_summary), 2)

cat(red("Mean % [min - max] of residuals outside the simulated envelope = ",HR[2],"%"," [",HR[[4]]," - ",HR[[8]],"]", "\n", "\n"))

## 63.5 %  [ 59.18  -  66.49 ] 
