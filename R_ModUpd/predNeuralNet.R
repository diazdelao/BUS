predNeuralNet <- function(theta){
  
  theta <- as.numeric(theta) # otherwise it's a dataframe and FOR takes ages!
  x <- seq(.1,10,by<-.1)
  n <- length(x)
  #sigma_eps <- 0.1 # std of the pediction-error
  epsilon <- rnorm(n,0,1)*theta[7]  # prediction-error
  #epsilon <- rnorm(n,0,1)*sigma_eps  # prediction-error
  y <- rep(0,n)
  for(i in 1:n){
    y[i] <- theta[1]*activation(x[i]*theta[3]+ theta[5]) + theta[2]*activation(x[i]*theta[4]+theta[6]) + epsilon[i]
  }
  
  out <- list(x,y)
  return(out)
  
  }