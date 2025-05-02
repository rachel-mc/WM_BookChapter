
# Code to filter the raw data to obtain the 'clean' subset used 

fn <- "~/Desktop/Data/YOY_Striped_Bass_2013_2023.xlsx"

bass_raw <- read_excel(fn, 
                       sheet = "2013-2023-ALL") |>
  clean_names() |>
  mutate(year = factor(year),
         station = factor(station),
         tide = as.numeric(tide), 
         vegetation = factor(vegetation))

counts <- bass_raw |> filter(mosa >= 1) |> dplyr::select(station)

at_least_one <- bass_raw |>
  semi_join(counts, by = "station")

no_NA <- at_least_one[complete.cases(at_least_one[,-8]),] ### not 817

## Fitting the GLM counterpart of the Poisson GLMM in lme4 (no random effect)
f_ <- glm(mosa ~ year + scale(d1aug) + scale(longitude) + scale(temp) + scale(sal) + scale(depth) + as.numeric(vegetation) + scale(moam) + station,
          family = poisson(link = "log"),
          data = bass)

summary(f_)

# Likelihood Ratio Test
anova(f2, f_, test = "Chisq") 
# the model with the random effect is preferred

# Half Normal plot of the Poisson GLMM
hnp(f2,
    how.many.out = T,
    paint = T) 
# pattern indicative of overdispersion - variance >> mean

## test whether Tide has a potential influence on MOSA,  one could reduce the dataset such that only “year-stations” with a value are left in the then reduced dataset

## Some stations that had a missing value for SAL (salinity) for instance, which were initially removed because we wanted to test for this covariate but ended up finding that it has no influence, could then be “brought back” in the dataset because values for all other covariates are available…

check_autocorrelation()
start_value_rho()

# Could use complete cases with all NAs removed - less rows

# salinity and longitude are expected to be positively associated as you move eastward, but whether this generates estimation problems for the variance is to be determined 