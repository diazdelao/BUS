
identif_y <- function(X){
     # Input X is a 3-dimensional standard Gaussian
     
     # Stiffnesses     
     k1 <- 29.7e6
     k2 <- 29.7e6
     
     # Masses
     m1 <- 16.5e3
     m2 <- 16.1e3
     
     # Data
     f_tilde1 <- 3.13
     f_tilde2 <- 9.83
     
     # Thetas are lognormals with modes 1.3 and 0.8. Mode is exp(mu-s^2).
     # Compute means and stds of lognormals such that modes equal 1.3 and 0.8:
     mu1 <-  0.510236734833878
     mu2 <-  0.169577686173127
     s1 <-  0.497867924620965 
     s2 <-  0.626674746170082
     
     u <- pnorm(X[1],0,1) # map to uniform space
     x1 <- X[2] # map to lognormal space
     x2 <- X[3]
     theta1 <-  exp(s1*x1 + mu1)
     theta2 <-  exp(s2*x2 + mu2)
     
     #solve eigenvalue problem to obtain finite element solution
     pol <- c(m1*m2, -m1*theta2*k2-m2*(theta1*k1+theta2*k2), theta1*k1*theta2*k2)
     f <-  Re( polyroot(rev(pol)) )
     
     # transform frequencies to Hertz
     f1 <-  sqrt(f[1])/(2*pi)
     f2 <-  sqrt(f[2])/(2*pi)
     
     # Heuristic measure of fit (Eqs. 9.1 and 9.2)
     lambda1 <- 1
     lambda2 <- 1
     sigma <- 1/8 # characteristic curves are sensitive to this parameter
     J <-  lambda1^2*((f1^2/f_tilde1^2)-1)^2 + lambda2^2*((f2^2/f_tilde2^2)-1)^2
     L <-  exp(-J/(2*sigma^2))
     
     y <- log(L/u)
     
     #return( list(y=y,L=L) ) 
     return(y) 
}
