## Lab5.R contains the same base code as Lab4_helper_down_functions but uses 
## logistic regression and multinomial regression to predict the probability of success for a field goal 
## and to predict the play type on the 4th down 

library(tidyverse)
library(nnet)

data <- readRDS("pbp2014-2024.rds")

## yards_gained()
# Randomly generates the yards gained for the team with possession
yards_gained<- function(FP){
  halfed<-as.integer((100-FP)/2)
  next_field_position<- sample(c("less_than_10","first_half_FP","second_half_FP"),1,prob=c(0.5,0.25,0.25))
  if(next_field_position=="less_than_10"){
    yg<-sample(0:9, 1)
  }
  else if(next_field_position=="first_half_FP")
  {
    yg<-sample(10:halfed,1)
  }
  else{
    yg<-sample(halfed+1:100,1)
  }
  
}

## downs_123()
# Simulates outcomes for downs 1, 2, 3 
downs_123<-function(D,YTG,FP){
  options <- sample(c("NoTurnover", "Turnover", "Touchdown"), 1, prob=c((0.50), (0.35), (0.15)))
  YG<- yards_gained(FP)
  if(options=="NoTurnover"){
    no_turn<-c(D+1,YTG-YG,FP+YG,0)
    return(no_turn)
  }
  else if(options="Turnover"){
    turnover<-c(D,YTG,FP,1)
    return(turnover)
  }
  else{
    touchdown<-c(D,YTG,105,1)
    return(touchdown)
  }
}

## Using Multinomial Regression to determine which play to run 

# Filter data for only 4th down 
onlyDown4 <- data %>% 
  filter(down==4) %>% 
  rename(YTG=ydstogo,FP=yardline_100) %>% 
  filter(play_type %in% c("field_goal","punt","run"))

mult.model <- multinom(play_type ~ YTG + FP, data = onlyDown4)

## playChoice() 
# Determines the play type in the 4th down
playChoice <- function(YTG, FP) {
  # Compute the predicted probabilities for each play choice
  probabilities <- predict(mult.model, newdata = data.frame(YTG = YTG, FP = FP), type = "probs")
  
  # Randomly select a play choice based on probabilities
  play_choice <- sample(c("field_goal", "punt", "go_for_it"), size = 1, prob = probabilities)
  
  return(play_choice)
}


## Using Logistic Regression to Predict Field Goal Outcome 
# Get field goal positions from data for relevant plays
field_position <- data$yardline_100
field_position <- field_position %>% filter(field_goal_result %in% c("made", "missed"))
n <- 1000 # Set number of field goals to calculate success from 

data <- data.frame(
  field_position = field_position,
  field_goal_success = rbinom(n, 1, prob = 1 / (1 + exp(-(3 - 0.1 * sample(10:60, n, replace = TRUE)))))  # Sigmoid function
)

log.model <- glm(field_goal_success ~ field_position, data = data, family = binomial)

## down_4 
# Simulates the 4th down 
# NOTE: Uses logistic regression model (log.model) above
down_4 <- function(D, YTG, FP, play_type, model) {
  # Initialize the flag to indicate whether the epoch function should be triggered
  flag <- 0
  play_type<-playChoice(YTG,FP)
  play_result <- NULL
  
  # Field Goal Scenario
  if (play_type == 'field_goal') {
    # Predict probability of field goal success using log.model 
    prob_success <- predict(model, newdata = data.frame(field_position = FP), type = "response")
    play_result <- ifelse(runif(1) < prob_success, 'made', 'missed')
    
    play_result <- sample(c("made", "missed"), 1)
    if (play_result == 'made') {
      # Successful field goal, set FP to 115 and return to epoch function
      new_state <- list(D = D, YTG = YTG, FP = 115)
      flag <- 1
    } else if (play_result == 'missed') {
      # Missed field goal, possession switches, return to epoch function
      new_state <- list(D = D, YTG = YTG, FP = FP)
      flag <- 1
    }
  }

  # Note: For this scenario, we want to call log.model as our model parameter in down_4(). 
  model <- log.model
  
  # Punt Scenario
  else if (play_type == 'punt') {
    play_result <- sample(c("typical", "rare"), 1,prob=c(0.9,0.1))
                          if (play_result == 'typical') {
                            # Typical punt, possession switches to epoch function
                            new_state <- list(D = D, YTG = YTG, FP = FP)
                            flag <- 1
                          } else if (play_result == 'rare') {
                            # Rare scenario (e.g., mishandled punt), stay in drive function and continue
                            new_state <- list(D = D, YTG = YTG, FP = FP)
                            flag <- 0
                          }
  }
  
  # Go for It Scenario
  else if (play_type == 'go_for_it') {
    play_result <- sample(c("successful", "unsuccessful"), 1)
    if (play_result == 'successful') {
      # Successful "go for it" play, stay in the drive with updated first down
      new_state <- list(D = 1, YTG = 10, FP = FP + yards_gained(FP))
      flag <- 0
    } else if (play_result == 'unsuccessful') {
      # Unsuccessful "go for it" play, possession switches, return to epoch function
      new_state <- list(D = D, YTG = YTG, FP = FP)
      flag <- 1
    }
  }
  
  # Return the updated state and flag (0 or 1)
  return(list(state = new_state, flag = flag))
}
