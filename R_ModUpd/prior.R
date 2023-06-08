prior <- function(theta,sigma){

  z1 <- -log(dnorm(theta[1],0,sigma))
  z2 <- -log(dnorm(theta[2],0,sigma))
  z3 <- -log(dnorm(theta[3],0,sigma))
  z4 <- -log(dnorm(theta[4],0,sigma))
  z5 <- -log(dnorm(theta[5],0,sigma))
  z6 <- -log(dnorm(theta[6],0,sigma))
  z7 <- -log(dnorm(theta[7],0,sigma))
  
  z <- z1+z2+z3+z4+z5+z6+z7
  
  return(z)

}