data <- read.csv("~/Desktop/Sports Analytics/nhl_pbp20162017.csv")
head(data, 3)

# Problem 1 
# Event team: who shot the puck 
# away score 
# home score
# away team 
# home team

data$score <- ifelse(data$Ev_Team == data$Home_Team, 
                     data$Home_Score - data$Away_Score, # Home_Team score 
                     data$Away_Score - data$Home_Score) # Away_Team score 


# Problem 3 
# Use Poisson, logistic regression, etc. 
# e.g. shot count would be Poisson, shot probabilities would be logistic regression 
