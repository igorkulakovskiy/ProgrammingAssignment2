## This is a bunch of functions that cache the inverse of a matrix:
##  1. "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.
##  2. "cacheSolve" function computes the inverse of the special "matrix" returned by 
##      makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##      then the cachesolve should retrieve the inverse from the cache.


##  "makeCacheMatrix" function receive a square matrix as an argument (or create empty one) 
##   and can store its inverse

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(Matrix) inverse <<- Matrix
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## "cacheSolve" function receive an object of "makeCacheMatrix" as an argument and returns matrix's inverse
##  either from the cache or computes it and stores it back to the object

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse ## Return a matrix that is the inverse of 'x'
}
