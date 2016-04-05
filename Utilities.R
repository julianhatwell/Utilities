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

# prediction function for regsubsets objects
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object, id = id)
  mat[,names(coefi)]%*%coefi
}

# return only the positive part or zero of a numeric vector
pos.part <- function(x) {
  x <- ifelse(x < 0, 0, x)
  x
}