## This module contains functions to optimize the performance of calculation of the inverse
## of a matrix by caching the results of the inversion.

## This function creates a special "matrix" object that can cache its inverse.
## Params: x - matrix to be inverted
makeCacheMatrix <- function(x = matrix()) {
  xInverse <<- NULL
  set <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseForX) xInverse <<- inverseForX
  getInverse <- function() xInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieves the inverse from the cache.

## Params: 
## x - special "matrix" object created with makeCacheMatrix
## ... - further arguments (will be bypassed to the funciton applied to x to calculate inverse)

## NOTE: The function assumes that the matrix supplied is always invertible. 
## In case when matrix is non-invertible (the matrix is not square) the error is thrown
## NOTE: the funciton does not limit the sizes and number of matrixes in the cache.

cacheSolve <- function(x, ...) {
    
  
    xInverse <- x$getInverse() ## trying to get data from cache first
    if(!is.null(xInverse)) {
        message("getting cached data for the matrix")
        return(xInverse)
    }
    
    ## if cache data was not found -> calculate the inverse of x, put it to the cache and return the result
    data <- x$get()
    xInverse <- solve(data, ...)
    x$setInverse(xInverse)
    xInverse 
}

