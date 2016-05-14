# This function creates a special "matrix" object that can cache its inverse.
# It returns a special "vector", which is really a list containing a function to:
# - get/set the value of the 'x' Matrix
# - get/set the value of the inverse of the 'x' Matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # imat: the inverse of matrix x
  imat <- NULL
  
  # matrix get / set functions
  set <- function(m) 
  { # replace for new matrix and initialize mean to NULL
    x <<- m
    imat <<- NULL
  }
  get <- function() x 
  
  # matrix inverse get / set functions
  setInverse <- function(m) imat <<- m
  getInverse <- function() imat
  
  # return the special "matrix" object
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The following function calculates the inverse of the special "matrix" created with the above makeCacheMatrix() function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.
## The setInverse function comutes the inverse with R's solve(a, b, ...) function.
cacheSolve <- function(x, ...) {
  
      imat <- x$getInverse()
      # check if the matrix inverse is already cached
      if ( ! is.null(imat))
      {
        message('return matrix inverse from cache (no computation done)')
        return(imat)
      }
      
      # else: do compute matrix inverse, store it in the cache and return it
      imat <- solve(x$get(), ...)
      x$setInverse(imat)
      imat
}