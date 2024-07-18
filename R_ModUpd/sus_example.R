rm(list = ls()) # clear memory

# Load R libraries
#=====================================================================================
library(MASS) # for multivariate normals
library(abind) # to combine multidimensional arrays
library(ggplot2)
library(GGally) # to create scatterplot matrix
library(data.table) # calls setDT to produce table of unique samples, also for melt()
library(dplyr)
# library(patchwork)

# Set home directory
#================================================================
myComputer <- as.character(Sys.info()["nodename"])
if(myComputer == 'ucl'){
  setwd('/home/user/GitHub/BUS/R_ModUpd/')
#} else if(myComputer == 'xps13'){
#  setwd('/home/alej/GitHub/BUS/R_ModUpd/')
} else if(myComputer == 'Alexs-MacBook-Air.local'){
  setwd('/Users/diazdelao/GitHub/BUS/R_ModUpd/')
}

# Load SuS, MMA, plotting functions, etc.
#==========================================================================
source('G.R') # performance function
source('sus.R') # original Subset Simulation
source('mma.R') # Modified Metropolis
source('plotsus2d.R') # Plots the samples
source('input_freq.R') # produces a table with frequencies of samples 

# Parameters for SUS
#==========================================================================
d <- 2 # dimensions
YF <- 0.124 # failure threshold

#exit.tol <- 1e-3 #stopping condition tolerance
n <- 1e3 # number of samples
p <- 0.1 # level probability
X <- array(0,dim = c(d,n,1)) 
X[,,1] <- mvrnorm(n = n,mu = rep(0,d),Sigma = diag(d)) # initial samples

# Run SUS
#==========================================================================
ptm <- proc.time() # start timer
S <- sus(X,n,p,d,YF) 
my.time <- proc.time() - ptm

# Plots. TO DO: Plot last level
#==========================================================================

# Plot all levels
Xall <- S$x # discard first dimension since it's used to produce uniform samples
#Xreduced <- S$x[,,2:S$L+1] # discard Level 0
plot.all_levels <- plotsus2d(Xall,S$L,S$Yi,d)
print(plot.all_levels)

if(S$L>0){ # Only makes sense with 2 or more levels
   source('sus_postproc.R') # different analyses to the S object
}

print(my.time[3]) # print elapsed time