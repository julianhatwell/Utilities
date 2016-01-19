# paste two strings together - for creating file names from params
me_File <- function(f) { paste0(f, "_File") }

# paste two strings together - for combining file paths
pathTo <- function(f) { paste0(fileLoc, f) }

# deletes a file
fileInit <- function(f) { if (file.exists(f)) file.remove(f) }

save.as.csv <- function(df, fName) {
  write.csv(df, pathTo(paste0(fName, ".csv")))
} 

# give it a vector and returns a function to search that vector
find_in_vector <- function(vec) { function(find) { any(find == vec) } }