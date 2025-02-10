# Down helper functions 
# down_one()...
# down_two()...
# down_three()...

YG <- 0 # NOTE: Need to figure out distribution for yards gained 
# downs 1-3: 
# 

down_one() <- function{
  if (YG < 10){
    YG  
  }
  else{ # If the team scores more than 10 yards, then go back to the first down
    down_one() 
    # NOTE: need to incorporate field position 
  }
}

down_two() <- function{
  if (YG < 10){
    YG  
  }
  else{ # If the team scores more than 10 yards, then go back to the first down
    down_one() 
    # NOTE: need to incorporate field position 
  }
}

down_three() <- function{
  if (YG < 10){
    YG  
  }
  else{ # If the team scores more than 10 yards, then go back to the first down
    down_one() 
    fp <-  
  }
}

down_four() <- 

run_play() <- function{
  
  # Instead of sampling FP, we'll sample YG 
  
  }