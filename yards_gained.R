yards_gained<- function(FP){
  halfed<-as.integer((100-FP)/2)
  next_field_position<- sample(c("less_than_10","first_half_FP","second_half_FP"),1,prob=c(0.5,0.25,0.25))
  if(next_field_position=="less_than_10"){
    yg<-sample(0:9, 1)
  }
  else if(next_field_position=="first_half_FP")
  {
    yg<-sample(10:halfed,1)
  }
  else{
    yg<-sample(halfed+1:100,1)
  }
  
}
