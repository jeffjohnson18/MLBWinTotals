# Danny Malter: R with baseball: https://www.youtube.com/watch?v=dgPH_y6GwuY&feature=youtu.be

# linear models to predict how many win a baseball team will have

# In R-Studio go to Tools, Install, type Lahman
# --or-- at the R-Studio command line type install.packages("Lahman)"
# install.packages('Lahman')
library(Lahman)

library(dplyr)

library(ggplot2)  
library(caret)    

teams <- Teams
head(teams)

# find teams runs per game #
teams <- teams %>%
  mutate(runs_game =  R/(W+L))

#display
head(teams)


teams_year <- teams %>%
  group_by(yearID) %>%
  summarize(mean_runs = mean(runs_game, na.rm=TRUE))

#plot mean runs by year
teams_year %>%
  ggplot(aes(x=yearID, y=mean_runs)) +
  geom_line() +
  geom_point() +
  ggtitle('MLB Runs by Year')

head(teams_year)

## Predict Wins, the target variable, by team ##

df_clean <- teams %>%
  select(name, yearID, W, L, R, H, X2B, X3B, HR, SO, RA) %>%
  filter(yearID >= 2010)

head(df_clean)
tail(df_clean)

lm_info <- lm(W ~ R + H + X2B + X3B + HR + SO + RA,
              data = df_clean)

# show summary of model
summary(lm_info)

# shows strikeouts, X2B, X3B were not significant (.9447, .6443, .5076)
# Now show new model without X2B, X3B, SO

lm2 <- lm(W ~ R + H + HR + RA,
          data = df_clean)
summary(lm2)

# now make new predictions
# normally we'd break up into training and testing, but for this tutorial we have just one dataset

# run a new prediction on df_clean to get predicted value for every record in the dataset
preds <- predict(lm2, df_clean)
preds
# first value is 67.5 wins, second is 90.5 wins

df_clean$pred <- preds
head(df_clean)

# can see the Diamondbacks had 65 wins and 67.5 predicted
# RMSE is from the caret package

RMSE(df_clean$pred, df_clean$W)
#  get RMSE between the two values, and the result is 3.886
# shows how each model is performing

# now can plot predicted value vs win value
df_clean %>%
  ggplot(aes(pred, W)) +
  geom_point() +
  geom_smooth() +
  ggtitle('Predicted Wins versus Actual')

