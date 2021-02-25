
## calculate the Z statistic based on the weight 
CalZ <- function(w,data){
    num_w1 <-t(w) %*%(data$n.event.x-data$exp.x)
    v_w1 <- t(w^2)%*%data$V
    z_w1 <- num_w1/sqrt(v_w1)
    return(z_w1)
}
## calculate the numerator based on the weight
CalN <- function(w,data){
  num_w1 <-t(w) %*%(data$n.event.x-data$exp.x)
  return(num_w1)
}  
## conditional assignment
chkV <- function(x,y=x,chkV=0,assignV=1){
     ifelse(x==chkV,assignV,y)   
  }
  