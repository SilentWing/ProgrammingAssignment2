## makeCacheMatrix and CacheSolve give the inverse of a given matrix.
## As it is faster get the value from the cache than calculate it, this progam 
## once calculated, store the inverse of a matrix and when it is used again in the
## same matrix, as the result is stored, it accesses the cache to give the result.

## What this function does is to store the matrix given and assigns to a variable m 
## a value (NULL). This function gives a list as an output with the values of 
## with the value of the matrix, m, and the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function reads the outcome of makeCacheMatrix and if m is NULL
## calculates the inverse of the matrix (and stores it in makeCacheMatrix). Otherwise, it gives the value 
## of the inverse as it is stored in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
