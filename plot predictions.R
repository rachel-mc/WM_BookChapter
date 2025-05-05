
## Load the libraries

library(ggplot2)

# Calculate the Predictions -------------------------------------------------------------

# Top ranking gamlss model

set.seed(2025)

n <- nrow(na.omit(bass))

nd <- data.frame(year = sample(levels(na.omit(bass)$year), size = n, replace = T),
                   # factor(2013, levels = levels(na.omit(bass)$year)),
                 d1aug = sample(0:80, size = n, replace = T),
                 longitude = sample(-50:-80, size = n, replace = T),
                 temp = sample(0:40, size = n, replace = T),
                 sal = sample(0:40, size = n, replace = T),
                 depth = sample(0:4, size = n, replace = T),
                 vegetation = sample(1:5, size = n, replace = T),
                 moam = sample(0:4, size = n, replace = T),
                 station = factor(4, levels = levels(na.omit(bass)$station)))

View(nd)

fitted <- predict(top_ranking_model,
                  newdata = nd,
                  type = "response")

plot_data <- data.frame(Year = na.omit(bass)$year,
                        Observed = na.omit(bass)$mosa,
                        Pred = round(fitted, digits = 2))

## Super inflated 2019 predictions

# Visualize the Predictions -----------------------------------------------

ggplot(data = plot_data, aes(x = Year)) +
  geom_point(aes(y = Observed), colour = "black") +  
  geom_line(aes(y = Pred), colour = "blue") +
  scale_y_continuous(limits = c(0, 200)) +
  labs(title = "Top-Ranking Model estimates",
       x = "Year",
       y = "CPUE (n/station)") +
  theme_minimal()

ggplot() +
  geom_point(data = plot_data, aes(x = Year, y = Observed), 
             color = "black") +
  geom_line(data = plot_summary, aes(x = Year, y = Mean_Predicted), 
            color = "blue", size = 1.2) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(title = "Smoothed Fitted Values from GAMLSS",
       x = "Year", y = "CPUE (n/station)") +
  theme_minimal()
