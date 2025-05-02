
## Load the libraries

library(dplyr)
library(FSA)
library(ggplot2)
library(janitor)
library(psych)
library(readxl)


# Data importation --------------------------------------------------------


# File name
fn <- "~/Desktop/Data/YOY_Striped_Bass_2013_2023.xlsx"

bass <- read_excel(fn, 
                   sheet = "2013-2023_CLEAN") |>
  clean_names() |>
  mutate(year = factor(year),
         station = factor(station),
         tide = as.numeric(tide), # tide is read in as a character
         vegetation = factor(vegetation))

bass


# Descriptive statistics --------------------------------------------------


Summarize(mosa ~ year, data = bass)

# inconsistent observations across years but this maximises the available data
# the percentage of stations with no catch is quite high

describe(bass) # only min and max are informative for categorical variables

summary(bass) # only tide has NAs 
# sum(is.na(bass))


# Exploratory visualisations and metrics ----------------------------------------------


# Plot the response
bass |>
  ggplot(aes(x = mosa)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Number of fish caught", y = "Count") # observe skewness and outliers

# Compare the outliers (2018 & 2019) with the mean 
mean(bass$mosa) # global mean

# Mean captures by year
bass |>
  filter(year == "2013") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2014") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2015") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2016") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2017") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2018") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2019") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2020") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2021") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2022") |>
  dplyr::pull(mosa) |>
  mean()

bass |>
  filter(year == "2023") |>
  dplyr::pull(mosa) |>
  mean()

# Plot the response against the main explanatory variable of interest
bass |>
  ggplot(aes(x = as.numeric(as.character(year)), y = mosa)) + # for x-axis labels
  geom_point() + 
  theme_bw() +
  labs(x = "Year", y = "Number of fish caught") +
  scale_x_continuous(breaks = 2013:2023) +
  geom_smooth(se = F) # loess smoothing 

# Reduced scale on y-axis
bass |>
  ggplot(aes(x = as.numeric(as.character(year)), y = mosa)) + 
  geom_point() + 
  theme_bw() +
  labs(x = "Year", y = "Number of fish caught") +
  scale_x_continuous(breaks = 2013:2023) +
  geom_smooth(se = F) +
  ggplot2:::limits(c(0, 50), "y")

# Per station
bass |>
  ggplot(aes(x = as.numeric(as.character(year)), y = mosa)) + 
  geom_point() + 
  theme_bw() +
  labs(x = "Year", y = "Number of fish caught") +
  scale_x_continuous(breaks = 2013:2023) +
  geom_smooth(se = F) +
  ggplot2:::limits(c(0, 50), "y") +
  facet_wrap(~ station)


# Model Selection ---------------------------------------------------------

# Use pre-determined adequate models only

# Chi squared goodness of fit test?

# IC
model_selection <- model.sel(f11, f12, f13, f14, f15, f16) # adequate models only
candidate_models <- get.models(model_selection, subset = TRUE)
top_ranking_model <- candidate_models[[1]]

# Predictions -------------------------------------------------------------

fitted <- predict(top_ranking_model, type = "link", se.fit = TRUE)
pred <- exp(fitted$fit)
LL <- exp(fitted$fit - 1.96 * fitted$se.fit)
UL <- exp(fitted$fit + 1.96 * fitted$se.fit)
pred_mean <- pred[[1]]
ci_ll <- LL[[1]]
ci_ul <- UL[[1]]

observed_mean <- mean(top_ranking_model$y)
plot_data <- round(cbind(Year = bass$year, pred, ci_ll, ci_ul), 2)

## Visualisation 

ggplot(data = plot_data, aes(x = Year)) +
  geom_point(aes(y = pred)) +
  geom_errorbar(aes(ymin = ci_ll, ymax = ci_ul)) +
  # geom_hline(yintercept = , col = "red", lwd = 1.1, lty = 2)
  labs(main = "Fitted Model estimates with 95% error bars",
       x = "Year",
       y = "CPUE (n/station)")
