## This file contains a couple of functions that compute and cache the inverse of a given matrix.
## For this assignment, it is assumed that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse. It performs the following tasks:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() return(x)
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() return(inverse)
  
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix() function above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve() function 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Getting cached data ...")
    return(inverse)
  }
  data <- x$get()
  ## The solve() function is the R function to calculate the matrix inverse
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  return(inverse)
}
