# Matrix inversion is usually a costly computation and there may be some benefit to caching the 
# inverse of a matrix rather than compute it repeatedly. Following two functions are used to show 
# how we can cache the inverse of a matrix
#
# Function "makeCacheMatrix" returns a list contaning a function to do the following operations:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix(nrow = 2, ncol = 2)) {
  # "Inv" is initialized to NULL
  Inv <- NULL
  
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  
  # Return the value of the matrix
  get <- function() x
  
  # Set the value of inverse of the matrix
  setInverse <- function(Inverse) Inv <<- Inverse
  
  # Get the value of inverse of the matrix
  getInverse <- function() Inv
  
  # Return the list of functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

# This function returns the inverse of a matrix. First it checks if the inverse of the matrix has already
# been computed and saved in a variable. If yes, it just gets the value and displays it without actually
# performing the computation. If no, it would compute the inverse and would save the value in a cache for
# later use.
cacheSolve <- function(listVar, ...) {
  Inv <- listVar$getInverse()
  
  # Check if inverse of the matrix has already been saved in the cache
  if (!is.null(Inv)) {
    message("Getting cached data")
    return(Inv)
  }
  
  # If computation is ncessary, get the inverse and save it in the cache for later use
  data <- listVar$get()
  Inv <- solve(data)
  
  listVar$setInverse(Inv)
  Inv
}

