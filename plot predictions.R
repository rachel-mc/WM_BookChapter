
## Load the libraries

library(ggplot2)

# Calculate the Predictions -------------------------------------------------------------

# Top ranking gamlss model

fitted <- predict(top_ranking_model, type = "response")

plot_data <- data.frame(Year = na.omit(bass)$year,
                        Observed = na.omit(bass)$mosa,
                        Pred = round(fitted, digits = 2))

## Super inflated 2019 predictions

# Visualize the Predictions -----------------------------------------------

ggplot(data = plot_data, aes(x = Year)) +
  geom_point(aes(y = Observed), colour = "black") +  
  geom_line(aes(y = Pred), colour = "blue") +
  scale_y_continuous(limits = c(0, 200)) +
  labs(title = "Fitted Model estimates",
       x = "Year",
       y = "CPUE (n/station)") +
  theme_minimal()

ggplot() +
  geom_point(data = plot_data, aes(x = Year, y = Observed), 
             color = "black", alpha = 0.4, shape = 1) +
  geom_line(data = plot_summary, aes(x = Year, y = Mean_Predicted), 
            color = "blue", size = 1.2) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(title = "Smoothed Fitted Values from GAMLSS",
       x = "Year", y = "CPUE (n/station)") +
  theme_minimal()

