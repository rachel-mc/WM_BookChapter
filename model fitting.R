
## Load the libraries

library(corrplot)
library(gamlss)
library(glmmTMB)
library(gratia)
library(lme4)
library(performance)
library(splines)


# Candidate Generalised Linear Mixed Models --------------------------------------------------------

## Global model in lme4 - all predictors except fishtotal
# The Poisson distribution is commonly employed for count data
f1 <- glmer(mosa ~ year + month + day + d1aug + latitude + longitude + tide + temp + sal + depth + vegetation + moam + (1|station),
            family = poisson(link = "log"),
            data = bass)
# numerical instability (Downdated VtV is not positive definite) 

## Test for correlation between the numeric variables
corrplot(cor(bass[,c(3:7, 9:11, 13)]))

cor_mat <- cor(bass[,c(3:7, 9:11, 13)], use = "complete.obs")
diag(cor_mat) <- NA # ignore correlations of variables with themselves
high_cor <- which(cor_mat > 0.8 | cor_mat < -0.8, arr.ind = TRUE)
high_cor_df <- data.frame(Variable1 = rownames(cor_mat)[high_cor[, 1]],
                          Variable2 = colnames(cor_mat)[high_cor[, 2]],
                          correlation = cor_mat[high_cor])
high_cor_df <- high_cor_df[high_cor_df$Variable1 < high_cor_df$Variable2, ] # alphabetical order to remove duplicates
high_cor_df

## Variable specification: 
# station is modelled as a random intercept - account for pseudo-replication, grouping structure in the data
# d1aug incorporates the month and day variables so the latter are removed
# latitude is highly correlated with longitude and salinity (see above) so this variable is removed 
# Numeric variables are scaled
# vegetation is categorical with five levels so it was converted to numeric to reduce parameters used

## Refit the Poisson model in lme4 with scaled and omitted variables 
f2 <- glmer(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
            family = poisson(link = "log"),
            data = bass)

summary(f2) # model does not converge - change optimizer?

## Check for (Multi)Collinearity
check_collinearity(f2) # all VIF < 10

## Overdispersion
var(bass$mosa) 
mean(bass$mosa) # variance >> mean

# Formal test using the check_overdispersion() function of "performance"
check_overdispersion(f2)

# "quasi" families cannot be used in glmer

## Negative Binomial type 2 model in lme4
f3 <- glmer.nb(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
               data = bass)

summary(f3)

### Now using the glmmTMB modelling framework
## Here we can fit additional models to accommodate zero-inflation and overdispersion

## Poisson model
f4 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              family = poisson(link = "log"),
              data = bass)

summary(f4)

## Zero-inflated Poisson model
f5 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              zi = ~ 1, # includes all predictors
              family = poisson(link = "log"),
              data = bass)

summary(f5)

## Hurdle Poisson
f6 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              zi = ~ 1, 
              family = truncated_poisson,
              data = bass)

summary(f6)

## Negative Binomial type 2 model
f7 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
              family = nbinom2,
              data = bass)

summary(f7)

## Zero-inflated Negative Binomial type 2 model
f8 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
               zi = ~ 1,
               family = nbinom2,
               data = bass)

summary(f8)

## Generalized Poisson model
f9 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
               family = genpois,
               data = bass)

summary(f9)

## Conway-Maxwell Poisson model - SLOW
f10 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
               family = compois,
               data = bass) 

summary(f10)
# Does not converge - discard

## Negative Binomial type 1 model
f11 <- glmmTMB(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + (1|station),
               family = nbinom1,
               data = bass)

summary(f11)

### Now using the gamlss modelling framework

## Poisson model
f12 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = PO,
              data = na.omit(bass))

summary(f12)

## Zero-inflated Poisson
f13 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = ZIP,
              n.cyc = 100, # helps algorithm to converge
              data = na.omit(bass))

summary(f13)

## Negative-Binomial Type 2 model
f14 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = NBI,
              data = na.omit(bass))

summary(f14)

## Zero-inflated Negative Binomial (Type 2) model
f15 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = ZINBI,
              data = na.omit(bass))

summary(f15)

## Negative Binomial type 1 model
f16 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = NBII,
              data = na.omit(bass))

summary(f16)

## Generalised Poisson model
f17 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = GPO,
              data = na.omit(bass))

summary(f17)

## Poisson-Inverse Gaussian model
f18 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = PIG,
              data = na.omit(bass))

summary(f18)

## Double Poisson model
f19 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = DPO,
              n.cyc = 300,
              data = na.omit(bass))

summary(f19)
# Does not converge - discard

## Explicitly modelling the variance of the 2 years with outliers  
f20 <- gamlss(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              sigma.formula = ~ I(year %in% c(2018, 2019)), 
              family = NBI,
              data = na.omit(bass))

summary(f20)

wp(f20)

# Try a non-linear effect for time 
f21 <- gamlss(mosa ~ ns(as.numeric(year), df = 3) + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + random(station),
              family = NBI,
              data = na.omit(bass))

summary(f21)
