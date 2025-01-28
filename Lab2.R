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
  else (fp > 100 & fp <= 110) {
    score <- score + 7 
  } 
  return score*team
  
  
