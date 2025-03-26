## Simulates downs 1-3
downs_123<-function(D,YTG,FP){
  options <- sample(c("NoTurnover", "Turnover", "Touchdown"), 1, prob=c((0.50), (0.35), (0.15)))
  YG<- yards_gained(FP)
  if(options=="NoTurnover"){
    no_turn<-c(D+1,YTG-YG,FP+YG,0)
    no_turn
  }
  else if(options="Turnover"){
    turnover<-c(D,YTG,FP,1)
    turnover
  }
  else{
    touchdown<-c(D,YTG,105,1)
    touchdown
  }
}

## Helper function to decide 4th down in a game 
playChoice(YTG,FP){
  if(YTG<5)
  {
    "go_for_it"
  }
  else{
    if(FP<50){
      "field_goal"
    }
    else{
      "punt"
    }
  }
}

## Simulates 4th down
down_4 <- function(D, YTG, FP, play_type) {
  flag <- 0 # Initialize flag to indicate whether epoch function should be triggered
  play_type<-playChoice(YTG,FP)
  play_result <- NULL
    
  # Handling Field Goal Scenario
  if (play_type == 'field_goal') {
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
  
  # Handling Punt Scenario
  else if (play_type == 'punt') {
    play_result <- sample(c("typical", "rare"), 1,prob=c(0.9,0.1)
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
  
  # Handling Go for It Scenario
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
  list(state = new_state, flag = flag)
}
