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

