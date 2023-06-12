plot_predNN <- function(myX){
# Function with true values
theta.true <- c( alpha1=5,alpha2=-5,beta1=-1,beta2=-3,gamma1=5,gamma2=2,sigma_eps=.1 )
true.nn <- predNeuralNet(theta.true)
nx <- length(true.nn[[1]])
ypred <- matrix(0,nrow=1000,ncol=nx) #1000 x 100

# Function with estimated values
for(i in 1:ncol(ypred)){
  theta.est <- myX[i,]
  print(i)
  ypred[,i] <- unlist(predNeuralNet(theta.est)[[2]])
}


plot(true.nn[[1]],true.nn[[2]],pch=16,col='springgreen4')
lines(true.nn[[1]],colMeans(ypred),col='red')
} 