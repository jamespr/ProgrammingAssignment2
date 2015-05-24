## makeCacheMatrix
## creates a special "matrix" object with getters/setters that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # The cached inverse. NULL if not solved yet, or if matrix has changed
  i <- NULL
  
  # Set internal matrix to the passed in value
  set <- function(y) {
    x <<- y
    
    # Clear the cache, as the old cached inverse may no longer apply to the new matrix
    i <<- NULL
  }
  
  # Get the internal matrix
  get <- function() x
  
  # Set the cached inverse
  setInverse <- function(inv) i <<- inv
  
  # Get the cached inverse
  getInverse <- function() i
  
  # Return a list containing the getters/setters for the special matrix object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CacheSolve
## This function computes the inverse of special "matrix" objects created by makeCacheMatrix 
## and caches them. If the cache exists and the matrix has not changed, then the cached value
## is returned to avoid additional expensive computations.
cacheSolve <- function(x) {
  
  # Get the existing cached inverse value from the specified special matrix object
  i <- x$getInverse()
  
  # Check if it has already been cached
  if(!is.null(i)) {
    message("getting cached data")
    
    # Return the cached inverse
    return(i)
  }
  
  # If it doesn't exists, start computing..

  # Get the internal matrix from the matrix object
  data <- x$get()
  
  # Solve it
  i <- solve(data)
  
  # Set the internal inverse cache in the matrix object
  x$setInverse(i)
  
  # Return the computed inverse
  i
}
