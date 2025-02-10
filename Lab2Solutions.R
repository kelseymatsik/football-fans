source("run_epoch.R")

# This is our top level function. This is the function we will call when we want
# to compute an expected points for a certain "state" of the game.  For now,
# the states are: down, ytg, and fp.

get_EP <- function(down, ytg, fp) {
  
  n <- 1000 
  
  # create a vector to store our simulations
  points <- rep(NA, n)
  
  for(i in 1:n) {
    points[i] <- run_epoch(down, ytg, fp)
  }
  
  # Return mean points 
  mean(points)
  
}

# Testing get_EP()
get_EP(2, 5, 34)
get_EP(1, 10, 61)


run_drive <- function(down, ytg, fp) {
  
  # Get new field position from random sample 
  new_fp <- sample(c(80, 105, 115), 1, prob=c(.9, .05, .05))
  
  list(down=1, ytg=10, fp=new_fp)
}

source("run_drive.R")
source("utils.R")

run_epoch <- function(down, ytg, fp) {
  team_status <- -1
  
  # set max number of drives
  max_drives <- 10 
  
  # set drive counter
  cumulative_drives <- 0 
  
  # initialize flag
  no_score <- TRUE 
  
  
  print("Remember, the state of the first drive should be the input state.")
  
  # run loop
  while(no_score & (cumulative_drives < max_drives)) {
    
    # Flip team status immediately 
    # The first iteration of the while loop will set team_status 
    # to 1, which is the "original offense."  If we kick
    # out of the while loop after just one drive, then the score multiplier
    # will be 1, which is exactly what we want.
    team_status <- team_status * -1
    
    # Update the cumulative drives now 
    # Only purpose is to make the print statements nice 
    # Increment the run drive
    cumulative_drives <- cumulative_drives + 1
    
    
    # For testing purposes, we may want to track the state of the "game" as it
    # progresses, so we will print here to show that we are running a drive, 
    # and the state of the game.  Since our "run_drive" function has only one 
    # value for fp that will instigate another call to the function, we should
    # always see fp=80, except for the first printout, which will be the state
    # we are deriving an EP for.  If we see anything else, we are not exiting 
    # the while loop properly!
    
    print(paste0("starting down: ", down, ", ytg: ", ytg, ", fp: ", fp, 
                 ", drive number: ", cumulative_drives,
                 ", team status flag: ", team_status,"."))
    
    # Run drive
    tmp_state <- run_drive(down, ytg, fp)
    
    # Reassign variables 
    down <- tmp_state$down
    ytg <- tmp_state$ytg
    fp <- tmp_state$fp
    
    # Flip the score flag if there was a score
    no_score <- (fp <= 100)
    
    
    
    
    # If cumulative_drives < 10 and no_score is true, the while loop 
    # will just run again, this time with the updated
    # downs, ytg, and fp settings.  Otherwise, it will exit.
  }
  
  # Compute the points from the field position 
  score <- team_status * compute_score(fp)
  
  # Print out the scoring field position
  print(paste0("final fp: ", fp, 
               ", scoring drive number: ", cumulative_drives,
               ", team status flag: ", team_status, ", score: ", score, "."))
  
  # Return score for get_EP() to use 
  score
  
}


# Test run_epoch()
run_epoch(2, 7, 34) 

# Helper function to compute the score
compute_score <- function(fp){
  if(fp <= 100) {
    0
  } else if (fp <= 110) {
    7
  } else {
    3
  }
  
}

# Lab 4 
# Output of downs is based on yards gained 