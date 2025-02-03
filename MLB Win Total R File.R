# Load required libraries
library(Lahman)   # Baseball dataset package
library(dplyr)    # Data manipulation
library(ggplot2)  # Data visualization
library(caret)    # Machine learning utilities

# Load the Teams dataset from Lahman package
teams <- Teams
head(teams)  # Display the first few rows to check the structure

# Calculate runs per game for each team
teams <- teams %>%
  mutate(runs_game = R / (W + L))  # Runs per game = Total runs / Total games played

head(teams)  # Check the updated dataset with the new column

# Compute the mean runs per game for each year
teams_year <- teams %>%
  group_by(yearID) %>%  # Group data by year
  summarize(mean_runs = mean(runs_game, na.rm = TRUE))  # Compute yearly average

# Plot the trend of mean runs per game over the years
teams_year %>%
  ggplot(aes(x = yearID, y = mean_runs)) +  # Set x-axis as year and y-axis as mean runs
  geom_line() +  # Add a line chart
  geom_point() +  # Add points for each year
  ggtitle('MLB Runs by Year')  # Add a title to the plot

head(teams_year)  # Display the first few rows of the yearly averages

# Prepare dataset for linear regression model
df_clean <- teams %>%
  select(name, yearID, W, L, R, H, X2B, X3B, HR, SO, RA) %>%  # Select relevant columns
  filter(yearID >= 2010)  # Use only data from 2010 onward

head(df_clean)  # Check the first few rows
tail(df_clean)  # Check the last few rows

# Build the initial linear regression model
lm_info <- lm(W ~ R + H + X2B + X3B + HR + SO + RA, data = df_clean)  # Predict Wins (W)
summary(lm_info)  # Display the model summary

# Identify variables that are not significant (e.g., SO, X2B, X3B)

# Create an improved model excluding insignificant variables
lm2 <- lm(W ~ R + H + HR + RA, data = df_clean)  # Use only significant predictors
summary(lm2)  # Display the summary of the refined model

# Predict wins using the refined model
preds <- predict(lm2, df_clean)  # Generate predictions for all records
preds  # Display predicted values

# Add predictions to the dataframe
df_clean$pred <- preds
head(df_clean)  # Check the updated dataframe

# Evaluate model performance using RMSE (Root Mean Squared Error)
RMSE(df_clean$pred, df_clean$W)  # Compute RMSE between predicted and actual wins

# Plot predicted wins vs. actual wins to visualize model performance
df_clean %>%
  ggplot(aes(pred, W)) +  # X-axis: Predicted wins, Y-axis: Actual wins
  geom_point() +  # Scatter plot
  geom_smooth() +  # Add a trend line
  ggtitle('Predicted Wins versus Actual')  # Title of the plot
