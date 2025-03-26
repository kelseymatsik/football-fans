source("helper_down.R")

run_play <- function(D, YTG, FP, team) {
  down <- 1  # Start from first down
  
  while (down <= 4) {
    if (down < 4) {  # First three downs
      result <- downs_123(D, YTG, FP)  # Get play result
      D <- result[1]
      YTG <- result[2]
      FP <- result[3]
      team <- result[4]  # Check if team possession changes
      
      if (team == 1) {  # Turnover occurred
        down <- 1  # Reset to first down for the other team
        print("Turnover! Possession switched.")
        break
      } else {
        down <- down + 1  # Move to next down
      }
    } else {  # Fourth down scenario
      play_type <- playChoice(YTG, FP)  # Determine the play type
      
      down_4_result <- down_4(D, YTG, FP, play_type)
      new_state <- down_4_result$state
      flag <- down_4_result$flag
      
      # Update values from down_4 result
      D <- new_state$D
      YTG <- new_state$YTG
      FP <- new_state$FP

      if (flag == 1) {  # If fourth-down play results in a turnover or field goal
        team <- 1  # Switch possession
        down <- 1  # Reset down for new team
        print("Fourth down play resulted in possession switch.")
        break
      } else {
        down <- 1  # Successful "go for it" means reset to first down
      }
    }
  }
  
  print(paste("End of play: Down", down, "| Yards to Go:", YTG, "| Field Position:", FP, "| Team:", team))
}
