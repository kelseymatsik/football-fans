# Drive Function
## Returns a state of the game
drive <- function(down,ytg,fp){
  next_fp <-sample.int(120,1)
  next_state <- c(1,10,next_fp)
  next_state
}
drive()