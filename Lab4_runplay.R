source("helperdown_yardsgained_functions.R")

run_play <- function(D, YTG, FP, team) {
  down <- 1  # Start from first down
  
  while (down <= 3) {  # Loop until 4th down is reached
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
  }
  print(paste("End of play: Down", down, "| Yards to Go:", YTG, "| Field Position:", FP, "| Team:", team))
}
