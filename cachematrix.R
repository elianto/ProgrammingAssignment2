## Coursera - R Programming - Assignment 2
## Caching the Inverse of a Matrix: This file contains a pair of functions that
## cache the inverse of a given matrix (assuming that the matrix supplied is
## always invertible).


# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL   # init

      get <- function() x   # return the original matrix
      setsolve <- function(solve) {
            s <<- solve    # access and store inverse using superassignment
      }
      getsolve <- function() s # return the stored inverse
      
      # list returned by the function makeCacheMatrix
      list(get = get, setsolve = setsolve, getsolve = getsolve)
}


# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been
# calculated (and the matrix has not changed), then cacheSolve should retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
      s <- x$getsolve()   # access x and get the inverse
      if(!is.null(s)) {   # check if inverse exists and if so returns it
            message("getting cached inverse")
            return(s)
      }
      data <- x$get()   # access x and get the matrix
      s <- solve(data, ...)   # calculate the inverse
      x$setsolve(s)   # store the inverse
      s   # return the inverse
}
