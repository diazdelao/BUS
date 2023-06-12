bnn <- function(X,Theta){
     # Input X is a 8-dimensional standard Gaussian
     
     # Data
     #<-===========================================================================
     x <- seq(.1,10,by<-.1)
     n <- length(x)
     epsilon <- rnorm(n,0,1)*Theta[7]  # prediction-error
     y <- rep(0,n)
     for(i in 1:n){
       #y[i] <- alpha1*activation(beta1*x[i]+gamma1)+alpha2*activation(beta2*x[i]+gamma2)+epsilon[i];
       y[i] <- Theta[1]*activation(Theta[3]*x[i]+Theta[5])+Theta[2]*activation(Theta[4]*x[i]+Theta[6])+epsilon[i];
     }
     sigma_prior <- 5
     
     #thetaOPT <- c( alpha1,alpha2,beta1,beta2,gamma1,gamma2,log(sigma_eps^(-2)) )
     #Ptheta <- prior(thetaOPT,sigma_prior)    # value of the prior pdf at theta
     #Ltheta <- bnnLikelihood(thetaOPT,x,y,n,sigma_prior)     # value of the likelihood at theta
     Ltheta <- bnnLikelihood(X[2:8],x,y,n,sigma_prior)     # value of the likelihood at theta
     
     u <- pnorm(X[1],0,1) # map to uniform space
     
     #y <- log(Ltheta/u)
     y <- log(Ltheta) - log(u) # for ease of computation
     
     return(y) 
}