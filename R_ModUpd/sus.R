
# Subset Simulation Vanilla
#==============================================================================
sus <- function(x,n,p,d,YF){
     nc <- n*p # number of chains
     ns <- (1-p)/p # number of states per chain
     y <- matrix(0,1,n) # output of performance function
     Yi <- 0 # intermediate failure thresholds
     L <- 0 # level number
     print('SuS Level = 0')
     nF <- 0 # counter for failure samples
     prob_ratio <- seq(from = (n-1)/n, to = 0,by=-(1-(n-1)/n)) # percentile estimator
     
     for(i in 1:n){
          y[i] <- G(x[,i,1]) # system response, Level 0
          if(y[i] > YF){nF <- nF + 1} # count initial failure samples, if any
     }
     si <- sort( y[L+1,]) # sort output ASCENDING
     bi <- si
     ccdf <- prob_ratio*(p^L) # Eq.(2.9) in CBUS.
     while(sum(nF)/n < p & L < 51){ 
          L <- L + 1 
          print(paste('SuS Level = ',as.character(L)))
          nF <- append(nF,0) # vector of failure samples (per level)
          Bi <- sort(y[L,],index.return = T,decreasing = T) # sorted responses and indices    
          y[L,] <- y[L,Bi$ix]
          y <- rbind(y,matrix(0,1,n)) # will be y[L,] in next iteration
          x[,,L] <- x[,Bi$ix,L] # sort x columns according to y
          Yi[L] <- (y[L,nc]+y[L,nc+1])/2 # i-th intermediate failure threshold
          Yi <- append(Yi,0) # will have a 0 in the end, substitute with YF
          
          x <- MMA(nc,ns,d,x,Yi,L) # Modified Metropolis
          
          # Evaluate new samples with G()
          # ==================================
          for(i in 1:n){
               y[L+1,i] <- G(x[,i,L+1])
               if(y[L+1,i]>YF) nF[L+1] <- nF[L+1] + 1
          }
          si <- sort(y[L+1,])
          bi <- append( bi[1:(L*(n-n*p))],si )
          ccdf <- append( ccdf[1:(L*(n-n*p))],prob_ratio*(p^L) )
     }
     si <- sort(y[L+1,])
     bi <- append( bi[1:(L*(n-n*p))],si )
     ccdf <- append(ccdf[1:(L*(n-n*p))],prob_ratio*(p^L) )
     Yi[L+1] <- YF # substitute last 0 with failure threshold
     pF <- p^L*nF[L+1]/n
     N <- n + n*(1-p)*L
     
    return( list(pF = pF, L = L, N = N, Y = y, Yi = Yi, nF = nF,
                  x = x, ccdf = ccdf, bi = bi) )
}

