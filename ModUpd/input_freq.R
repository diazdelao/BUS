# Compute table of unique input samples and count their frequency 
#=================================================================
input_freq <- function(x,Y,Levels,Ls,sort.by){
  # Ls is the level Selected by the user
  # sort.by is the option to sort (descending) either by 'output' or 'Count'
  
  X <- t(x[,,Ls]) # transpose all samples
  df <- data.frame(X) # convert samples into data frame
  df$Y <- Y[Ls,] # add column with the output of each sample
  # Note: Y comes from SuS, and df$Y is for the sake of printing the df
  my.table <- setDT(df)[,list(Count=.N),names(df)] # Count duplicates
  if(sort.by == 'Count'){
  my.table <- my.table[order(-my.table$Count),] # order according to count
  } else if(sort.by == 'Y'){
  my.table <- my.table[order(-my.table$Y),] # order according to count
  }
  
  # Display the samples with highest frequency
  max.freq <- max(my.table$Count)
  ind.max <- which(my.table$Count == max.freq)
  print( paste('The samples that repeat the most in Level',
               Ls,'(out of',as.character(Levels+1),') are:') )
  print(my.table[ind.max,])
  
  return(my.table)
}