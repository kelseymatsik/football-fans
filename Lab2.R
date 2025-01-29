# Initialize possible points 
possible_points<-c(-7,-3,3,7)


# Random Point Function 
## Not necessary for code to run - for testing purposes 
rnd_pt_function<-function(down,ytg,fp){
  return(sample(possible_points,1))
}

rnd_pt_function()


# Drive Function
## Returns a state of the game
drive <- function(down,ytg,fp){
  next_fp <-sample.int(120,1)
  next_state <- c(1,10,next_fp)
  return(next_state)
}
drive()

# Initialize reference and opponent 
reference <- 1 
opponent <- -1 

# Process_State Function 
## takes in the state, adds points, and switches teams 
proccess_state <- function(down,ytg,fp,team){
  
  score <- 0
  if (fp > 110) {
    score <- score + 3 
  }
  else if (fp > 100 & fp <= 110) {
    score <- score + 7 } 
  
  added_points<-score*team
  
  next_team<-team
  if(added_points>0){
    next_team<-opponent
  }
  else  if(added_points<0){
    next_team<-reference
  }
  
  return (c(added_points,next_team))
}

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
  
  return(score)
}



# Top-Level Function (simulates game)
simulate_game <- function(down, ytg, fp, n = 1000) {
  results <- numeric(n)
  for (i in 1:n) {
    results[i] <- simulate_epoch(down, ytg, fp)
  }
  return(mean(results))
}


  
