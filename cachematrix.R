## This R file provides two functions that can be used to speed up
## computation of inverse matrix.
## makeCacheMatrix will create an object that stores the matrix and
## caches the computed inverse.
## cacheSolve will use the object created by makeCacheMatrix to solve
## and cache the inverse of provided matrix.

## This function creates an object for caching the inverse of provided matrix.
## Creates an object that contains four functions:
## set - sets new matrix into the object
## get - returns data matrix
## setinverse - caches the inverse matrix
## getinverse - returns the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  cached <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    cached <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cached <<- inverse
  getinverse <- function() cached
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes and stores the result of solve function.
## If result was computed already, computation will not be repeated and
## cached result will be returned instead.
## Supplied argument has to be an object created by "makeCacheMatrix" function
## see ?solve for more information
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
