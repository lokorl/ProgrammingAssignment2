## Matrix inversion is usually a costly computation and their may be 
##some benefit to caching the inverse of a matrix rather than compute 
##it repeatedly.  If the contents of a matrix are not changing, it 
##may make sense to cache the value of the inverse so that when we need it again,
##it can be looked up in the cache rather than recomputed.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   # m will be our 'inverse' and it's reset to NULL every 
  #    time makeCacheMatrix is called
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


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
