# Parameters for BUS
#==========================================================================
n <- 1e3 # number of samples
p <- .1 # level probability
d <- 8 # dimensions
exit.tol <- 1e-6 # stopping condition tolerance
X <- array(0,dim = c(d,n,1)) 
X[,,1] <- mvrnorm(n = n,mu = rep(0,d),Sigma = diag(d)) # initial samples

# Run BUS as in the paper
#==========================================================================
ptm <- proc.time() # start timer
B <- bus0(X,n,p,d,exit.tol) 
my.time <- proc.time() - ptm
print(my.time[3]) # print elapsed time

# Print table of samples, last level
#==========================================================================
#tab <- input_freq(B$x,B$Y,B$L,B$L+1,sort.by='Count') # sort by either 'Count' or output 'Y'
#print(tab) # uncomment to print whole table

# Plots
#==========================================================================
#  Plot all samples

Xall <- B$x[2:3,,] # discard first dimension since it's used to produce uniform samples
plot.all_levels <- plotsus2d(Xall,B$L,B$Yi,d=2)
print(plot.all_levels)

# Characteristic trends
par(mfrow=c(2,1))
plot_trends(B$bi,B$ccdf,B$Yi,B$L)
