bnn <- function(X){
     # Input X is a 8-dimensional standard Gaussian
     
     # Data
     #<-===========================================================================
     x <- seq(.1,10,by<-.1)
     n <- length(x)
     alpha1 <- 5  # true values of parameters 
     alpha2 <- -5
     beta1 <- -1
     beta2 <- -3
     gamma1 <- 5
     gamma2 <- 2
     sigma_eps <- 0.1 # std of the pediction-error
     epsilon <- rnorm(n,0,1)*sigma_eps  # prediction-error
     y <- rep(0,n)
     for(i in 1:n){
       y[i] <- alpha1*activation(beta1*x[i]+gamma1)+alpha2*activation(beta2*x[i]+gamma2)+epsilon[i];
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