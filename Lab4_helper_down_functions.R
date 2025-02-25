
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



# I made a seperate helping function to decide the play in a fourth down

playChoice(YTG,FP){
  if(YTG<5)
  {
    return("go_for_it")
  }
  else{
    if(FP<50){
      return("field_goal")
    }
    else{
      return("punt")
    }
  }
}


# This code simulates the fourth down.


down_4 <- function(D, YTG, FP, play_type) {
  # Initialize the flag to indicate whether the epoch function should be triggered
  flag <- 0
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
    play_result <- sample(c("typical", "rare"), 1)
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
  return(list(state = new_state, flag = flag))
}


