## These two functions work in tandem to avoid repeated calls to the solve()
## function (which inverts a matrix) under circumstances where the inverse has
## been previously calculated. This is achieved by cacheing the result, and by
## returning the result from the cache on subsequent calls, rather than by
## re-calculation.


## This first function creates a cache of two matrices - x and inv_x and returns a vector
## of set and get functions by which these matrices may be assigned and returned.

## Use makeCacheMatrix(x) to create a vector of set/get functions and a local cache
##   Note that x is the initial matrix of values.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  set_inv <- function(z) inv_x <<- z
  get_inv <- function() inv_x
  list(set = set, get = get,
       set_inv = set_inv,get_inv = get_inv)
}


## This second function works with the vector of set and get functions created by the above
## function to return the inverse of the provided matrix - either by looking this up
## if inv_x exists (ie is not null) or by calculating this directly using Solve().
## After performing the above calculation, it calls set_inv() to cache the result.

## Use cacheSolve(x,...) to return the inverse
##   Note that x is a vector of functions created by makeCacheMatrix above 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$get_inv()
  if(!is.null(inv_x)) {
##    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$set_inv(inv_x)
  inv_x
}
