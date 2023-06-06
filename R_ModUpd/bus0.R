# BUS, version in paper
#==============================================================================
bus0 <- function(x,n,p,d,exit.tol){ 
     ak <- 1 # initial probability of inadmissibility 
     ak.vec <- 1 # vector to store all ak's
       
     nc <- n*p # number of chains
     ns <- (1-p)/p # number of states per chain
     y <- matrix(0,1,n) # output of performance function
     Yi <- 0 # intermediate failure thresholds
     L <- 0 # level number
     print('BUS Level = 0')
     print('<============================================>')
     
     for(i in 1:n){
            y[i] <- G(x[,i,1])
     }
     while(ak > exit.tol & L < 51){
          L <- L + 1 
          print(paste('BUS Level = ',as.character(L)))
          print('<============================================>')
          Bi <- sort(y[L,],index.return = T,decreasing = T) # sorted responses and indices    
          y[L,] <- y[L,Bi$ix]
          y <- rbind(y,matrix(0,1,n)) # will be y[L,] in next iteration
          x[,,L] <- x[,Bi$ix,L] # sort x columns according to y
          Yi[L] <- y[L,nc] # i-th intermediate failure threshold
          logU <- log( pnorm(x[1,nc,L],0,1)  )
          Yi_BUS <- Yi[L] - logU # bk - log(U)
          # Instead of P(L>exp{bk}), I'm computing P(log(L/U) > bk-log(U)) to avoid modifying G.R
          
          # call subset with flag 'y' and plot flag 'n', exp(Yi[L]) as YF
          ak <- sus(x,n,p,d,Yi_BUS) 
          #ak <- sus(x[,,L],n,p,Yi_BUS,d) 
          ak <- ak$pF   
          ak.vec <- c(ak.vec,ak) # store current ak
          Yi <- append(Yi,0) # will have a 0 in the end, delete it!
          
          x <- MMA(nc,ns,d,x,Yi,L)
          
          # Evaluate new samples with G()
          # ==================================
          for(i in 1:n){
               y[L+1,i] <- G(x[,i,L+1])
          }
     }
     Yi <- Yi[1:L] # delete last zero
     N <- n + n*(1-p)*L
     
     # Compute CCDF
     # ====================================
     si <- sort( y[1,])
     bi <- si[1:(n-n*p)] # sort samples and exclude seeds for next level...
     if(L > 1){ # Guard against having 2 or less levels (L+1 > 2)
       for(i in 2:(L)){
         si <- sort( y[i,])
         bi <- c(bi,si[1:(n-n*p)])
       }
     }
    si <- sort(y[L+1,]) # ... except for the last level
    bi <- c(bi,si)
    # Create vector of corresponding level for each sample
    lev <- rep(0,n-n*p)
    if(L > 1){ # guard against only 2 levels (Level 0 and Level 1)
      for(i in 1:(L-1)){lev <- c(lev,rep(i,n-n*p))} 
    }
    lev <- c(lev,rep(L,n)) #lev <- rep(0:L,rep(n-n*p,L+1)) # one-line version (fix)
    # vector of ccdf estimator: (n-k)/n for k = 1,...,n  (Eq.(2.5) in CBUS)
    prob_rat_long <- seq(from = (n-1)/n, to = 0,by=-(1-(n-1)/n)) 
    prob_ratio <- prob_rat_long[1:(n-n*p)]
    prob_ratio <- rep(prob_ratio,L)
    prob_ratio <- c(prob_ratio,prob_rat_long)
    ccdf <- prob_ratio*rep(p,length(lev))^lev # p_k^(i) Eq.(2.9) in CBUS
    
    # return: thresholds Yi,  ak.vec
    # Yk = -log(multk) ===> exp(Yk) = 1/multk ===> multk = exp(-Yk)
    return( list(p.inadm = ak, p.inadm.vec = ak.vec, L = L, N = N, Y = y, Yi = Yi,
                  x = x, ccdf = ccdf, bi = bi) )
}

