## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The functions below create a special matrix which can cache the result of the 
## solve function (which calculates the inverse) thus avoiding the need to
## recalculate it.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   s <- NULL
   set <- function(y) {
      x <<- y
      s <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) s <<- solve
   getsolve <- function() s
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve will retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   s <- x$getsolve()
   if(!is.null(s)) {
      message("getting cached data")
      return(s)
   }
   data <- x$get()
   s <- solve(data, ...)
   x$setsolve(s)
   s   
}
