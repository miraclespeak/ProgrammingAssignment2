# makeCacheMatrix: return a list of functions to: 
# 1. Set the value of the matrix 
# 2. Get the value of the matrix 
# 3. Set the value of the inverse 
# 4. Get the value of the inverse 
makeCacheMatrix <- function(x = matrix()) 
{ 
  ## Initialize m 
  m <- NULL 
  ## Create a function which is to keep global_x and global_m as passed matrix and Null, respectively.
  set <- function(y) 
  { 
    # y is the initial matrix from user. so it is stored in global_x. 
    global_x <<- y 
    # initialize global_m 
    global_m <<- NULL 
  } 
  # Create one line function(). a matrix stored by set() is returned. 
  get <- function() return(global_x) 
  # Create one line function(). a matrix is stored as global value. 
  set_global_m <- function(m) global_m <<- m 
  # Create one line function(). a matrix stored by set_global_m() is returned. 
  get_global_m <- function() return(global_m) 
  list(set = set, get = get, set_global_m = set_global_m, get_global_m = get_global_m) 
} 


# This function computes the inverse of matrix. 
# by checking previous history, this function avoids for redundancy. 
###cacheSolve <- function(x) 
cacheSolve <- function(x, ...) {
  # try to get the value from the global environment. 
  m<- x$get_global_m() 
  if(!is.null(m)) {  

    # Check the result. 
    # by checking if m is NULL, we can know whether this matrix was already computed or not. 
    # if so, return computed value in last time, then print the message. 
    message("getting cached data") 
    return(m) 
  } 
  # if m is NULL, the inverse of matrix is computed by solve() function. 
  # Then, this result should be stored in global value for reusing. 
  data <- x$get() 
  inverseMatrix <- solve(data) 
  x$set_global_m(inverseMatrix) 
  return(inverseMatrix) 
}