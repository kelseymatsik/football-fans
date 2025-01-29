# Simulate_Epoch Function 
## Computes the expected points during the epoch (from that state until the next score)
simulate_epoch <- function(down, ytg, fp) {
  score <- NA
  team <- 1  # 1 for reference team, -1 for opponent
  drive_count <- 0
  max_drives <- 10 # Set max_drives as 10
  
  while (is.na(score) && drive_count < max_drives) {
    state <- drive(down, ytg, fp) # Get state of the game from drive() function
    down <- state[1]
    ytg <- state[2]
    fp <- state[3]
    
    result <- proccess_state(down, ytg, fp, team)
    added_points <- result[1]
    team <- result[2]
    
    
    # If the team scores 
    if (added_points != 0) {
      score <- added_points
    }
    
    drive_count <- drive_count + 1
  }
  
  # If the team doesn't score, set score to 0
  if (is.na(score)) {
    score <- 0
  }
  
  score
}