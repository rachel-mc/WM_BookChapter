
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
  geom_smooth() +
  ggplot2:::limits(c(0, 50), "y")

# Per station
bass |>
  ggplot(aes(x = as.numeric(as.character(year)), y = mosa)) + 
  geom_point() + 
  theme_bw() +
  labs(x = "Year", y = "Number of fish caught") +
  geom_smooth(se = F) +
  ggplot2:::limits(c(0, 50), "y") +
  facet_wrap(~ station) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
