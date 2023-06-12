# Performance function
#==============================================================================
G <- function(X){
  #G.y <- identif_y(X) # CBUS paper example 
  
  theta.true <- c( alpha1=5,alpha2=-5,beta1=-1,beta2=-3,gamma1=5,gamma2=2,sigma_eps=.1 )
  G.y <- bnn(X,theta.true) # CBUS paper example 
  return(G.y)
}