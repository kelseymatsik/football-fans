## Lab5_v2.R is a modified version of Lab5.R for our final project. We define 
## yards gained, downs 1-3, down 4, and play choice functions which are shared between 
## the 4 models (one for each of the 4 quarters). We then loop through the data, 
## getting the data for each of the 4 quarters, and fit logistic and multinomial regression
## models on the quarter data. This trains models on data from each of the qaurters. 


library(tidyverse)
library(nnet)

data <- readRDS("~/Desktop/Sports Analytics/pbp2014-2024.rds")

## Shared helper functions
yards_gained <- function(FP){
  halfed <- as.integer((100 - FP) / 2)
  next_field_position <- sample(c("less_than_10", "first_half_FP", "second_half_FP"), 1, prob = c(0.5, 0.25, 0.25))
  if(next_field_position == "less_than_10"){
    return(sample(0:9, 1))
  } else if(next_field_position == "first_half_FP"){
    return(sample(10:halfed, 1))
  } else {
    return(sample((halfed + 1):100, 1))
  }
}

downs_123 <- function(D, YTG, FP){
  options <- sample(c("NoTurnover", "Turnover", "Touchdown"), 1, prob = c(0.50, 0.35, 0.15))
  YG <- yards_gained(FP)
  if(options == "NoTurnover"){
    return(c(D + 1, YTG - YG, FP + YG, 0))
  } else if(options == "Turnover"){
    return(c(D, YTG, FP, 1))
  } else {
    return(c(D, YTG, 105, 1))
  }
}

## Generalized quarter-specific modeling
# Loops through each of the 4 quarters, subsets the data by quarter, and stores 
# the models for each quarter
for (q in 1:4) {
  assign(paste0("q", q, ".data"), data %>% filter(qtr == q))
  qdata <- get(paste0("q", q, ".data"))
  
  onlyDown4 <- qdata %>% 
    filter(down == 4) %>% 
    rename(YTG = ydstogo, FP = yardline_100) %>% 
    filter(play_type %in% c("field_goal", "punt", "run"))
  
  assign(paste0("mult.model.q", q), multinom(play_type ~ YTG + FP, data = onlyDown4))
  
  fg_positions <- qdata %>% 
    filter(field_goal_result %in% c("made", "missed")) %>% 
    pull(yardline_100)
  
  n <- 1000
  sim_data <- data.frame(
    field_position = fg_positions,
    field_goal_success = rbinom(n, 1, prob = 1 / (1 + exp(-(3 - 0.1 * sample(10:60, n, replace = TRUE)))))
  )
  
  assign(paste0("log.model.q", q), glm(field_goal_success ~ field_position, data = sim_data, family = binomial))
}

## Generalized down_4 and playChoice function
playChoice <- function(YTG, FP, model) {
  probs <- predict(model, newdata = data.frame(YTG = YTG, FP = FP), type = "probs")
  sample(c("field_goal", "punt", "go_for_it"), 1, prob = probs)
}

down_4 <- function(D, YTG, FP, play_type, model_choice, model_fg) {
  flag <- 0
  play_type <- playChoice(YTG, FP, model_choice)
  
  if (play_type == "field_goal") {
    prob_success <- predict(model_fg, newdata = data.frame(field_position = FP), type = "response")
    play_result <- ifelse(runif(1) < prob_success, "made", "missed")
    new_state <- list(D = D, YTG = YTG, FP = ifelse(play_result == "made", 115, FP))
    flag <- 1
  } else if (play_type == "punt") {
    play_result <- sample(c("typical", "rare"), 1, prob = c(0.9, 0.1))
    new_state <- list(D = D, YTG = YTG, FP = FP)
    flag <- ifelse(play_result == "typical", 1, 0)
  } else if (play_type == "go_for_it") {
    play_result <- sample(c("successful", "unsuccessful"), 1)
    if (play_result == "successful") {
      new_state <- list(D = 1, YTG = 10, FP = FP + yards_gained(FP))
      flag <- 0
    } else {
      new_state <- list(D = D, YTG = YTG, FP = FP)
      flag <- 1
    }
  }
  return(list(state = new_state, flag = flag))
}
