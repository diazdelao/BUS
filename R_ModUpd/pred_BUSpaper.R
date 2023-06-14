pred_BUSpaper <- function(X){
     # Input X is the samples from the last SuS level
     # Input f is two natural frequencies
      
     f <- rep(0,2)
     f[1] <- 3.13
     f[2] <- 9.83
     X <- exp(X) 
     
     # Stiffnesses     
     k1 <- 29.7e6
     k2 <- 29.7e6
     
     # Masses
     m1 <- 16.5e3
     m2 <- 16.1e3
     
     theta1 <-  getmode(X[,1])
     theta1 <-  getmode(X[,1])
     theta2 <-  mean(X[,2])
     theta2 <-  mean(X[,2])
     
     #solve eigenvalue problem to obtain finite element solution
     pol <- c(m1*m2, -m1*theta2*k2-m2*(theta1*k1+theta2*k2), theta1*k1*theta2*k2)
     f_root <-  Re( polyroot(rev(pol)) )
     
     # transform frequencies to Hertz
     f_tilde <- rep(0,2)
     f_tilde[1] <-  sqrt(f_root[1])/(2*pi)
     f_tilde[2] <-  sqrt(f_root[2])/(2*pi)
     
     diff <- f-f_tilde
     
     # Heuristic measure of fit (Eqs. 9.1 and 9.2)
     #lambda1 <- 1
     #lambda2 <- 1
     #sigma <- 1/8 # characteristic curves are sensitive to this parameter
     #J <-  lambda1^2*((f1^2/f_tilde1^2)-1)^2 + lambda2^2*((f2^2/f_tilde2^2)-1)^2
     #L <-  exp(-J/(2*sigma^2))
     
     return(diff)
}