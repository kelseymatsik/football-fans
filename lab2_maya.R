possible_points<-c(-7,-3,3,7)
possible_points<-c(-7,-3,3,7)

rnd_pt_function<-function(down,ytg,fp){
  return(sample(possible_points,1))
}

rnd_pt_function()


# Drive function

drive <- function(down,ytg,fp){
  location<-sample.int(120,1)
  end <- c(1,10,location)
  return(end)
}

drive()

reference <- 1 
opponent <- -1 

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

simulate_epoch <- function(down, ytg, fp) {
  score <- NA
  team <- 1  # 1 for reference team, -1 for opponent
  drive_count <- 0
  max_drives <- 10
  
  while (is.na(score) && drive_count < max_drives) {
    state <- drive(down, ytg, fp)
    down <- state[1]
    ytg <- state[2]
    fp <- state[3]
    
    result <- proccess_state(down, ytg, fp, team)
    added_points <- result[1]
    team <- result[2]
    
    if (added_points != 0) {
      score <- added_points
    }
    
    drive_count <- drive_count + 1
  }
  
  if (is.na(score)) {
    score <- 0
  }
  
  return(score)
}

# Top-Level Function
simulate_game <- function(down, ytg, fp, n = 1000) {
  results <- numeric(n)
  for (i in 1:n) {
    results[i] <- simulate_epoch(down, ytg, fp)
  }
  return(mean(results))
}
