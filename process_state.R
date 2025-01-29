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
  
  c(added_points,next_team)
}