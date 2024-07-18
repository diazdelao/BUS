# Count common points
#==========================================================================

matches <- numeric(S$L) # store comparisons
mycheck <- function(array1,array2){
  identical <- colSums(array1 == array2) == 2
  sum(identical)
}

for (i in 1:S$L){
  matches[i] <- mycheck(Xall[,,i],Xall[,,i+1])
}
#print(matches)

# Print table of samples, last level
#==========================================================================
tab <- input_freq(S$x,S$Y,S$L,S$L+1,sort.by='Count') # sort by either 'Count' or output 'Y'
#print(tab) # uncomment to print whole table

# Initialise file to save info about the arrays
#==========================================================================

dir_name <- "Samples"

# Clear the directory of any existing Array_x.csv files
csv_files <- list.files(dir_name, pattern = "^Array_[0-9]+\\.csv$", full.names = TRUE)
if (length(csv_files) > 0) {
  file.remove(csv_files)
}

info_file <- file.path(dir_name,'array_info.txt')
cat('',file = info_file) # create empty file, to be filled in within loop

# Save unique samples
#==========================================================================
for (i in 1:S$L){
  current <- Xall[,,i]
  unique_cols <- unique(t(current)) # unique() needs transposed array
  unique_df <- as.data.frame(unique_cols)
  file_name <- file.path(dir_name, paste0("Array_", i, ".csv"))
  write.csv(unique_df, file_name, row.names = FALSE)
  cat("Points in array", i, ":", dim(unique_df)[1],", Threshold:",round(S$Yi[i+1],2),"\n",
      file = info_file,append = TRUE)
}

# Read and print the contents of the info file
info_content <- readLines(info_file)
cat('\n',info_content, sep = "\n")