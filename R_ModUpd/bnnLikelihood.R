bnnLikelihood <- function(theta,x,y,n,sigma_prior){
  
  mu <- seq(0,n)
  a <- seq(0,n)
  for(i in 1:n){
     mu[i] <- y[i]-theta[1]*activation(x[i]*theta[3]+ theta[5])-theta[2]*activation(x[i]*theta[4]+theta[6]);
     a[i] <- dnorm(mu[i],0,exp(-theta[7]/2));
   }
  
  z <- sum(log(a))
  z <- exp(z)
  #z <- prod(a)
  #z <- -sum(log(a))
  #z <- z + prior(theta,sigma_prior) + 100
  
  return(z)
}