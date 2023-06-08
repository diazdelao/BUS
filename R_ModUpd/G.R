# Performance function
#==============================================================================
G <- function(X){
  #G.y <- identif_y(X) # CBUS paper example 
  G.y <- bnn(X) # CBUS paper example 
  return(G.y)
}