
# To Do:
# Would be nice to add contour lines

plotsus2d <- function(x,L,Yi,d){ # plots samples and intermediate levels
     ip <- 1
     D <- array(0,dim = c(n,3)) # array of samples
     if(d == 2){ D <- as.data.frame(cbind(ip-1,t(x[,,ip]))) }
     else if (d == 3) { D <- as.data.frame(cbind(ip-1,t(x[2:3,,ip]))) }
     if(L > 0){ # if L = 0 simply plot initial samples
       for(ip in 2:(L+1)){
            if(d == 2){ Dtemp <- as.data.frame(cbind(ip-1,t(x[,,ip]))) }
            else if (d == 3) { Dtemp <- as.data.frame(cbind(ip-1,t(x[2:3,,ip]))) }
            D <- rbind(D,Dtemp)
       }
     } 
       ggobj1 <- ggplot(D,aes(x=D[,2],y=D[,3],color=as.factor(V1))) +
             xlab('X1') + ylab('X2') + labs(color = "Level") + 
             geom_point() + #scale_color_brewer(palette="YlOrRd") +
             theme_bw() +
             ggtitle('All SuS Levels')  
       Xlims <- layer_scales(ggobj1)$x$get_limits() # vector of axes limits
       Ylims <- layer_scales(ggobj1)$y$get_limits()
       xx <- seq(Xlims[1], Xlims[2], length.out = 50)
       yy <- seq(Ylims[1], Ylims[2], length.out = 50)
       my.grid <- expand.grid(x = xx, y = yy)
       dat <- as.matrix(my.grid) # avoid it being data.frame
       nn <- dim(dat)[1]
       dat <- cbind(rnorm(nn),dat) # attach normal column to grid
       zz <- rep(0,nn)
       for(i in 1:nn){zz[i] <- G(dat[i,])}
       my.curves <- as_tibble(cbind(my.grid,zz))
       my.plot <- ggplot(data=my.curves, aes(x=x,y=y,z=zz)) 
       return(ggobj1)
     }
