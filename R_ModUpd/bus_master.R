rm(list = ls()) # clear memory

# Load R libraries
#=====================================================================================
library(MASS) # for multivariate normals
library(abind) # to combine multidimensional arrays
library(ggplot2)
library(data.table) # calls setDT to produce table of unique samples, also for melt()
library(dplyr)
library(cowplot) # for displaying graphs side by side

# Set home directory
#================================================================
myComputer <- as.character(Sys.info()["nodename"])
if(myComputer == 'ucl'){
  setwd('/home/user/GitHub/BUS/R_ModUpd/')
} else if(myComputer == 'xps13'){
  setwd('/home/alej/GitHub/BUS/R_ModUpd/')
}

# Load MMA, plotting functions, etc.
#==========================================================================
source('bus0.R') # Main algorithm in the paper
source('sus.R') # original Subset Simulation
source('G.R') # performance function: (example in the paper)
source('mma.R') # Modified Metropolis
source('input_freq.R') # produces a table with frequencies of samples 
source('bus_example.R') # implements example from paper
source('plot_trends.R') # Plots characteristic trends in paper
source('plotsus2d.R') # Plots the samples
source('bnn.R') # Bayesian Neural Network
source('bnnLikelihood.R') # Likelihood for BNN
source('prior.R') # Prior for BNN
source('activation.R') # Activation function for BNN

# Parameters for BUS
#==========================================================================
n <- 1e3 # number of samples
p <- .1 # level probability
d <- 8 # dimensions
exit.tol <- 1e-3 # stopping condition tolerance
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
