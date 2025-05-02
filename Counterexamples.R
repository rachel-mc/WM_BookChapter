

# Case 1 ------------------------------------------------------------------

## Show increase in R^2 when more predictors are added 
# As we are fitting mixed models, the gamlss package is used for model fitting
# and for its pseudo R^2 function

f1 <- gamlss(mosa ~ year + scale(d1aug) + random(station),
             family = NBI,
             data = na.omit(bass))

summary(f1)

Rsq(f1)

f2 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + random(station),
             family = NBI,
             data = na.omit(bass))

summary(f2)

Rsq(f2)

f3 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + random(station),
             family = NBI,
             data = na.omit(bass))

summary(f3)

Rsq(f3)


# Case 2 ------------------------------------------------------------------

## An inadequate model is chosen from candidate set



# Case 3 ------------------------------------------------------------------



## Model that fits the data well but is inadequate



## Fit a high degree polynomial to year
fit1 <- glmer(mosa ~ poly(year, 10) + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + vegetation + scale(moam) + (1|station),
              data = bass)

summary(fit1)

## Inadequate model
# Fit a Gaussian GLMM (default) - incorrect: response variable is counts
fit2 <- glmer(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              data = bass)

summary(fit2)

hnp(fit2,
    how.many.out = TRUE,
    paint = T)

## The Poisson model has a higher R^2 than NB but it is inadequate
cor(bass$mosa, fitted(fit2))^2
cor(bass$mosa, fitted(fit3))^2

## Predictive ability

