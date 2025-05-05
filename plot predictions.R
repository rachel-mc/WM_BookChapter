
## Load the libraries

library(gamlss)
library(ggplot2)
library(tidyverse)

# Calculate the Predictions -------------------------------------------------------------

# Top ranking gamlss model by AIC(c): GPO

set.seed(2025)

## New data for year, covariates, and the random intercept station 

nd_i <- data.frame(year = seq(from = 2013, to = 2023, by = 0.1), # similar to Julien's syntax 
                   d1aug = mean(bass$d1aug),
                   longitude = mean(bass$longitude),
                   temp = mean(bass$temp),
                   sal = mean(bass$sal),
                   depth = mean(bass$depth),
                   vegetation = mean(as.numeric(bass$vegetation)),
                   moam = mean(bass$moam),
                   station = as.factor(i)) # create a loop for each station?

fitted_i <- predict(top_ranking_model,
                    newdata = nd_i,
                    type = "response")

fitted_ALL <- cbind(fitted_i)

#####

n <- nrow(na.omit(bass))

nd <- data.frame(year = factor(sample(levels(na.omit(bass)$year), size = n, replace = T)),
                 # factor(2013, levels = levels(na.omit(bass)$year))
                 d1aug = sample(0:80, size = n, replace = T),
                 longitude = sample(-50:-80, size = n, replace = T),
                 temp = sample(0:40, size = n, replace = T),
                 sal = sample(0:40, size = n, replace = T),
                 depth = sample(0:4, size = n, replace = T),
                 vegetation = sample(1:5, size = n, replace = T),
                 moam = sample(0:4, size = n, replace = T),
                 station = factor(rep(4, times = n))
                 # factor(4, levels = levels(na.omit(bass)$station))
                 )

View(nd)
str(nd)

nd <- nd |>
  arrange(year)

fitted <- predict(top_ranking_model,
                  newdata = nd,
                  type = "response")


# Visualize the Predictions -----------------------------------------------


## Use training data for predictions instead (not ideal)

fitted <- predict(top_ranking_model,
                  type = "response")

plot_data <- data.frame(year = na.omit(bass)$year,
                        obs_resp = na.omit(bass)$mosa,
                        fv = round(fitted, digits = 2)) 

ggplot(data = plot_data, aes(x = year)) +
  geom_point(aes(y = obs_resp), colour = "black") +  
  geom_line(aes(y = fv), colour = "blue") +
  scale_y_continuous(limits = c(0, 200)) + #### Super inflated 2019 predictions - rescale y-axis
  labs(title = "Top-Ranking Model estimates",
       x = "Year",
       y = "CPUE (n/station)") +
  theme_minimal()

# Obtain a mean predictions per year

plot_data2 <- plot_data |>
  group_by(year) |>
  summarise(avg_fv = mean(fv))

ggplot() +
  geom_point(data = plot_data, aes(x = year, y = obs_resp), color = "black") +
  geom_line(data = plot_data2, aes(x = year, y = avg_fv, group = 1), color = "blue") +
  geom_ribbon() +
  scale_y_continuous(limits = c(0, 200)) +
  labs(title = "Top-Ranking Model estimates",
       x = "Year",
       y = "CPUE (n/station)") +
  theme_minimal()

## Calculate 95% CIs and plot using geom_ribbon()
