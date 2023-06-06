# MMA
#==============================================================================
MMA <- function(nc,ns,d,x,Yi,L){
     q <- rep(0,d) # array to store proposal distribution sample
     z <- array(0,dim = c(d,nc,ns+1)) # array of seeds
     z[,,1] <- x[,1:nc,L]
     for(j in 1:nc){ # governs no. chains (columns)
          for(m in 1:ns){ # governs no. of matrices in the array
               for(k in 1:d){ # governs no. of dimensions
                    a <- z[k,j,m] + rnorm(1,0,1)
                    r <- min(1,dnorm(a,0,1)/dnorm(z[k,j,m],0,1))
                    if(runif(1,0,1) < r) q[k] <- a else q[k] <- z[k,j,m]
               }
                 if(G(q) > Yi[L]){z[,j,m+1] <- q} else  z[,j,m+1] <- z[,j,m] 
          }
     }
     # Assign samples from p(x|Li)  
     # ============================
      x <- abind(x,array(0,dim = c(d,n,1)),along = 3) # will be x[,L] next iteration
      for(j in 1:nc){
           for(m in 1:(ns+1)){
                x[,(j-1)*(ns+1)+m,L+1] <- z[,j,m]
           }
      }
  return(x)
}
