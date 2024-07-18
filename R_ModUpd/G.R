# Performance function
#==============================================================================
G <- function(X){
  # CBUS
  #G.y <- identif_y(X) 
  
  # BNN
  #theta.true <- c( alpha1=5,alpha2=-5,beta1=-1,beta2=-3,gamma1=5,gamma2=2,sigma_eps=.1 )
  #G.y <- bnn(X,theta.true) 
  
  # Vanilla SuS
  p <- 2
  G.y <- X[1]^p - X[2]^p
  
  # Mixture of Gaussians
  #=================================
  a1 <- 0.8
  a2 <- 1-a1
  mo1 <- c(2.946,2.946)
  mo2 <- c(-1,-1)
  
  G.y <- a1/(2*pi)*exp(-0.5*((X[1]-mo1[1])^2+(X[2]-mo1[2])^2)) +
             + a2/(2*pi)*exp(-0.5*((X[1]-mo2[1])^2+(X[2]-mo2[2])^2))  
  
  return(G.y)
}