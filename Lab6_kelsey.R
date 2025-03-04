library(tidyverse)
library(lubridate)

data <- read.csv("~/Desktop/Sports Analytics/nhl_pbp20162017.csv")
head(data, 3)

################################ Problem 1 #####################################

## EDA / Pre-Processing 

# View unique periods 
unique(data$Period) # 1, 2, 3, 4, 5

# View range of values for Time_Elapsed 
list(c(min(data$Time_Elapsed),(max(data$Time_Elapsed)))) # isn't accurate 
class(data$Time_Elapsed) # character; can't be converted to numeric

# View range of Seconds_Elapsed 
class(data$Seconds_Elapsed)
list(c(min(data$Seconds_Elapsed),(max(data$Seconds_Elapsed))))
# There is 1200 seconds per each period (or 20 mins). 
# For the sake of the problem, we will ignore periods 4 and 5 (they are overtime). 

# Get only relevant periods (not including overtime)
data2 <- data %>% 
  filter(Period %in% c(1, 2, 3))


## Calculating score differential 
# Ev_Team: who has possession during the shot 
data2$Point_Diff <- ifelse(data2$Ev_Team == data2$Home_Team, 
                     data2$Home_Score - data2$Away_Score, # If Ev_Team is the home team  
                     data2$Away_Score - data2$Home_Score) # If Ev_Team is the away team

# Convert Period to a categorical variable
data2$Period <- as.factor(data2$Period)

# Adding Time_Bin (making time a categorical variable)
data2 <- data2 %>% 
  # Make Seconds_Elapsed categorical 
  mutate(Time_Bin = case_when(
    Seconds_Elapsed < 400  ~ "Early",
    Seconds_Elapsed < 800  ~ "Mid",
    TRUE                ~ "Late"
  ))

## Summarize shot counts by team, period, and point differential
# Each row represents a shot 
shot_count_data <- data2 %>% 
  group_by(Ev_Team, Period, Point_Diff, Time_Bin) %>% 
  summarize(Shot_Count = n(), .groups = "drop")

## Fit the Poisson regression mode 
poisson_model1 <- glm(Shot_Count ~ Period + Point_Diff + Time_Bin, 
              data = shot_count_data, family = poisson())
# summary(model1)

## Testing the Model / Making Predictions 

# Generate test data
set.seed(3402)
test_data <- expand.grid(
  Ev_Team = unique(shot_count_data$Ev_Team),
  Period = as.factor(c(1, 2, 3)),
  Point_Diff = seq(min(shot_count_data$Point_Diff, na.rm = TRUE), 
                   max(shot_count_data$Point_Diff, na.rm = TRUE), by = 1),
  Time_Bin = c("Early", "Mid", "Late")
)

# Make predictions 
test_data$Predicted_Shots <- predict(poisson_model1, newdata = test_data, type = "response")
head(test_data, 5)


# Problem 3 
# Use Poisson, logistic regression, etc. 
# e.g. shot count would be Poisson, shot probabilities would be logistic regression 
