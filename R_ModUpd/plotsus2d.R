plotsus2d <- function(x,L,Yi,d){ # plots samples and intermediate levels
     if(L==0){ip <- 1} else{ip <- 2} # ip <- 2 to discard level 0. affects first for loop below
     iptemp <- ip+1 # to affect loop below. is this the best way to do it?
     D <- array(0,dim = c(n,3)) # array of samples
     if(d == 2){ D <- as.data.frame(cbind(ip-1,t(x[,,ip]))) }
     else if (d == 3) { D <- as.data.frame(cbind(ip-1,t(x[2:3,,ip]))) }
     if(L > 0){ # if L = 0 simply plot initial samples
       for(ip in iptemp:(L+1)){ # ip starts in 3 to be consistent with ip <- 2
            if(d == 2){ Dtemp <- as.data.frame(cbind(ip-1,t(x[,,ip]))) }
            else if (d == 3) { Dtemp <- as.data.frame(cbind(ip-1,t(x[2:3,,ip]))) }
            D <- rbind(D,Dtemp)
       }
     } 
       ggobj1 <- ggplot(D,aes(x=D[,2],y=D[,3])) + geom_point()
       Xlims <- layer_scales(ggobj1)$x$get_limits() # vector of axes limits
       Ylims <- layer_scales(ggobj1)$y$get_limits()
       
       # For uniformly distributed grid:
       #==================================
       npts <- 50
       xx <- seq(Xlims[1], Xlims[2], length.out = npts)
       yy <- seq(Ylims[1], Ylims[2], length.out = npts)
       # For normally distributed grid:
       #==================================
       #npts <- 50
       #xx <- rnorm(npts,mean = (Xlims[1]+Xlims[2])/2,
       #           sd=(Xlims[2]-Xlims[1])/6)
       #y <- rnorm(npts,mean = (Ylims[1]+Ylims[2])/2,
       #           sd=(Ylims[2]-Ylims[1])/6)
       
       my.grid <- expand.grid(x = xx, y = yy)
       dat <- as.matrix(my.grid) # avoid it being data.frame
       nn <- dim(dat)[1]
       dat <- cbind(dat,rep(0,nn)) # attach zero column to grid
       zz <- rep(0,nn)
       for(i in 1:nn){zz[i] <- G(dat[i,])}
       my.curves <- as_tibble(cbind(my.grid,zz))
       my.plot <- ggplot(data=my.curves, aes(x=x,y=y,z=zz)) 
       
       # Combine the plots
       combined_plot <- ggplot() +
         geom_point(data = D, aes(x = D[, 2], y = D[, 3], color = as.factor(V1))) +
         geom_contour(data = my.curves, aes(x = x, y = y, z = zz), color = 'black') +
         xlab('X1') + 
         ylab('X2') + 
         labs(color = "Level") + 
         theme_bw() +
         ggtitle('All SuS Levels') +
         theme(plot.title = element_text(hjust = 0.5))
       
       # Print the combined plot
       return(combined_plot)
     }
