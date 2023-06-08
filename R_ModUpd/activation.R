activation <- function(x){
  y <- (exp(x)-exp(-x))/(exp(x)+exp(-x))
  return(y)
}